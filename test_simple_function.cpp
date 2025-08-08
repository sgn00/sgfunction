#include "virt_simple_function.hpp"
#include "void_simple_function.hpp"
#include "inplace_simple_function.hpp"

#include <cassert>
#include <stdexcept>
#include <string>
#include <utility>
#include <type_traits>
#include <memory>

// =============================================================
// Helpers & Shared Fixtures
// =============================================================

// Test function
static int add(int x, int y) { return x + y; }

// Test class with state
struct Stateful {
    int state = 0;
    int operator()(int x, int y) { return x + y + state; }
};

// A tiny counter functor to verify deep copy (each copy tracks its own hits)
struct Counter {
    mutable int hits = 0;
    int operator()(int x) const { ++hits; return x; }
};

// Overload selector for reference-category behavior
struct Overload {
    int operator()(int& )  const { return 1; }
    int operator()(int&& ) const { return 2; }
};

// Target throws (non-void to avoid returning from void invokers)
struct ThrowsInt {
    int operator()() const { throw std::runtime_error("boom"); }
};

// Tracker for destructor/lifetime checks
struct Tracker {
    static inline int live = 0;
    int payload = 0;
    Tracker() { ++live; }
    explicit Tracker(int v) : payload(v) { ++live; }
    Tracker(const Tracker& o) : payload(o.payload) { ++live; }
    Tracker(Tracker&& o) noexcept : payload(o.payload) { o.payload = -9999; ++live; }
    Tracker& operator=(const Tracker& o) { payload = o.payload; return *this; }
    Tracker& operator=(Tracker&& o) noexcept { payload = o.payload; o.payload = -9999; return *this; }
    ~Tracker() { --live; }

    int operator()(int x, int y) const { return payload + x + y; }
};

// =============================================================
// Test Cases (templated where possible to exercise all impls)
// =============================================================

template <typename T>
void test_basic() {
    T f = add;
    assert(f(1, 2) == 3);

    // Copy construct
    auto f2 = f;
    assert(f2(1, 2) == 3);
    assert(f(1, 2) == 3); // Original still valid

    // Move construct
    auto f3 = std::move(f2);
    assert(f3(1, 2) == 3);
    assert(!f2); // Moved-from should be empty

    // Copy assign
    T f4;
    f4 = f3;
    assert(f4(1, 2) == 3);
    assert(f3(1, 2) == 3); // Original still valid

    // Move assign
    T f5;
    f5 = std::move(f3);
    assert(f5(1, 2) == 3);
    assert(!f3); // Moved-from should be empty

    // Reset
    f5.reset();
    assert(!f5);

    // Test empty function call
    try {
        (void)f5(1, 2);
        assert(!"Expected std::runtime_error");
    } catch (const std::runtime_error& e) {
        assert(std::string(e.what()) == "bad function call");
    }
}

template <typename T>
void test_lambda() {
    T f = [](int x, int y) { return x * y; };
    assert(f(3, 4) == 12);

    // Test with capture
    int multiplier = 2;
    T f2 = [multiplier](int x, int y) { return (x + y) * multiplier; };
    assert(f2(1, 2) == 6);
}

template <typename T>
void test_stateful() {
    Stateful s; s.state = 10;
    T f = s;
    assert(f(1, 2) == 13); // 1 + 2 + 10

    // Test copy preserves state
    auto f2 = f;
    assert(f2(1, 2) == 13);

    // Test move preserves state
    auto f3 = std::move(f);
    assert(f3(1, 2) == 13);
    assert(!f); // Moved-from should be empty
}

template <typename T>
void test_swap() {
    T f1 = add;
    T f2 = [](int x, int y) { return x * y; };
    assert(f1(2, 3) == 5);
    assert(f2(2, 3) == 6);

    f1.swap(f2);
    assert(f1(2, 3) == 6);
    assert(f2(2, 3) == 5);

    T f3;
    f1.swap(f3);
    assert(!f1);
    assert(f3(2, 3) == 6);
}

template <typename T> // T is SimpleFunction<int(int)>
void test_deep_copy_and_move_independence() {
    Counter c{};
    T f = c;
    auto c1 = f;   // copy #1
    auto c2 = f;   // copy #2

    (void)f(1);
    (void)f(2);
    (void)c1(3);
    (void)c2(4);

    // Copies should not share state; call each again to ensure independence
    (void)f(5);    // f.hits = 3
    (void)c1(6);   // c1.hits = 2
    (void)c2(7);   // c2.hits = 2

    // Move empties source but keeps target alive
    auto moved = std::move(f);
    assert(static_cast<bool>(moved));
    bool threw = false;
    try { (void)f(0); } catch (...) { threw = true; }
    assert(threw);
}

// TRef=SimpleFunction<int(int&)>, TRR=SimpleFunction<int(int&&)>
template <typename TRef, typename TRR>
void test_reference_categories() {
    TRef lfun = Overload{};
    TRR  rfun = Overload{};
    int a = 0;
    // lvalue route
    assert(lfun(a) == 1);
    // rvalue route
    assert(rfun(0) == 2);
}

// T0=SimpleFunction<int()>, T2=SimpleFunction<int(int,int)>
template <typename T0, typename T2>
void test_exceptions_and_empty_more() {
    // Empty throws
    T2 empty;
    bool bad_call = false;
    try { (void)empty(0,0); } catch (const std::runtime_error&) { bad_call = true; }
    assert(bad_call);

    // Propagation from target
    T0 th = ThrowsInt{};
    bool propagated = false;
    try { (void)th(); } catch (const std::runtime_error& e) { propagated = std::string(e.what()) == "boom"; }
    assert(propagated);
}

template <typename T>
void test_bool_and_reset_paths() {
    T f;
    assert(!f);
    f = add;
    assert(f);
    f.reset();
    assert(!f);
    // swap-with-empty also empties and re-enables depending on direction
    T g = [](int x, int y){ return x - y; };
    f.swap(g);
    assert(f(5,3) == 2);
    assert(!g);
}

template <typename T>
void test_self_ops() {
    T f = [](int x, int y){ return x + y; };
    f = f; // self-copy-assign should be no-op
    assert(f(2,3) == 5);

    f.swap(f); // self-swap should be no-op
    assert(f(2,3) == 5);
}

template <typename T> // T=SimpleFunction<int(int,int)>
void test_reassign_and_destruction() {
    assert(Tracker::live == 0);
    {
        T f = Tracker{100};
        assert(Tracker::live == 1);
        assert(f(1,2) == 103);

        // Reassign to a new target should destroy old one
        f = [](int a, int b){ return a*b; };
        assert(Tracker::live == 0);
        assert(f(3,4) == 12);

        // Assign another Tracker and then reset
        f = Tracker{5};
        assert(Tracker::live == 1);
        assert(f(1,1) == 7);
        f.reset();
        assert(Tracker::live == 0);
    }
    assert(Tracker::live == 0);
}

template<typename T>
void test_const() {
    const T f = add;
    (void)f(1, 2);
}

// In-place only: wrapper size equals CAPACITY (catches buffer math regressions)
template <size_t N>
void test_inplace_size_invariant() {
    using Fn = sg::inplace::SimpleFunction<int(int,int), N>;
    static_assert(sizeof(Fn) == N, "inplace::SimpleFunction size must equal CAPACITY");
}

// Forwarding semantics battery
// The tests here verify overload resolution and that we actually forward rvalue-ness
// for move-only arguments, and count moves/copies for by-value signatures.

template <template <typename> class Fn>
void test_forwarding() {
    // --- 1) Overload resolution: lvalue / const lvalue / rvalue ---
    struct Overloaded {
        int operator()(int&) { return 1; }             // lvalue
        int operator()(const int&) { return 2; }       // const lvalue
        int operator()(int&&) { return 3; }            // rvalue
    };

    {
        Fn<int(int&)> f{ Overloaded{} };
        int x = 0;
        assert(f(x) == 1); // expect lvalue overload
    }
    {
        Fn<int(const int&)> f{ Overloaded{} };
        const int x = 0;
        assert(f(x) == 2); // expect const lvalue overload
    }
    {
        Fn<int(int&&)> f{ Overloaded{} };
        assert(f(0) == 3); // expect rvalue overload
    }

    // --- 2) Move-only argument is actually moved ---
    {
        Fn<int(std::unique_ptr<int>)> f = [](std::unique_ptr<int> p) {
            // If forwarding preserved rvalue-ness, we own the pointer here
            return *p;
        };

        auto up = std::make_unique<int>(7);
        assert(static_cast<bool>(up));
        const int got = f(std::move(up));
        assert(got == 7);
        assert(up == nullptr); // must be moved-from
    }

    // --- 3) Count moves vs copies for by-value signature ---
    struct Count { int moves = 0; int copies = 0; } tr;

    struct MoveCopyCounter {
        Count* tr = nullptr;
        MoveCopyCounter() = default;
        explicit MoveCopyCounter(Count* t) : tr(t) {}

        MoveCopyCounter(const MoveCopyCounter& other) : tr(other.tr) {
            if (tr) tr->copies++;
        }
        MoveCopyCounter(MoveCopyCounter&& other) noexcept : tr(other.tr) {
            if (tr) tr->moves++;
        }
        MoveCopyCounter& operator=(const MoveCopyCounter& other) {
            if (this != &other) {
                tr = other.tr;
                if (tr) tr->copies++;
            }
            return *this;
        }
        MoveCopyCounter& operator=(MoveCopyCounter&& other) noexcept {
            if (this != &other) {
                tr = other.tr;
                if (tr) tr->moves++;
            }
            return *this;
        }
    };

    // By-value: expect 2 moves, 0 copies
    {
        tr.moves = tr.copies = 0;
        Fn<void(MoveCopyCounter)> f = [](MoveCopyCounter) { /* consume by value */ };

        MoveCopyCounter m{ &tr };
        f(std::move(m));

        // Explanation (step-by-step):
        // - 1 move into operator()'s by-value parameter (construct param from rvalue)
        // - forward to invoke as reference (no construction)
        // - lambda takes by value → 1 more move into lambda’s parameter
        // => total moves = 2, copies = 0
        assert(tr.copies == 0);
        assert(tr.moves  == 2);
    }

    // --- 4) By-reference signatures do NOT cause moves/copies ---
    {
        tr.moves = tr.copies = 0;
        Fn<void(MoveCopyCounter&)> f = [](MoveCopyCounter&) { /* just observe */ };
        MoveCopyCounter m{ &tr };
        f(m); // lvalue
        assert(tr.copies == 0);
        assert(tr.moves  == 0);
    }
    {
        tr.moves = tr.copies = 0;
        Fn<void(MoveCopyCounter&&)> f = [](MoveCopyCounter&&) { /* just observe */ };
        MoveCopyCounter m{ &tr };
        f(std::move(m)); // rvalue ref binding; no construction
        assert(tr.copies == 0);
        assert(tr.moves  == 0);
    }
}

template <typename T> // T = SimpleFunction<void(int&, int)>
void test_void_return_and_throw() {
    // Normal path
    T f = [](int& x, int y){ x += y; };
    int a = 1;
    f(a, 4);
    assert(a == 5);

    // Throwing path (void-return)
    using T0 = typename std::conditional<
        std::is_same_v<T, sg::virt::SimpleFunction<void(int&,int)>>,
        sg::virt::SimpleFunction<void()>,
        std::conditional_t<
            std::is_same_v<T, sg::voidptr::SimpleFunction<void(int&,int)>>,
            sg::voidptr::SimpleFunction<void()>,
            sg::inplace::SimpleFunction<void(), 1024>
        >
    >::type;

    struct ThrowsVoid { void operator()() const { throw std::runtime_error("boom-void"); } };
    T0 tv = ThrowsVoid{};
    bool propagated = false;
    try { tv(); } catch (const std::runtime_error& e) { propagated = std::string(e.what()) == "boom-void"; }
    assert(propagated);
}

template <typename T> // T = SimpleFunction<int()>
void test_mutable_lambda_called_through_const() {
    // Mutable lambda increments internal counter; SimpleFunction::operator() is const.
    struct C { int c = 0; };
    C state;
    T f = [m = std::make_shared<C>(state)]() mutable -> int {
        // non-const operator() (because mutable)
        m->c += 1;
        return m->c;
    };

    const T& cf = f; // call through const ref
    assert(cf() == 1);
    assert(cf() == 2);
    // Ensure original still works
    assert(f() == 3);
}

template <typename T> // T = SimpleFunction<int()>
void test_capture_by_value_vs_reference_copy_semantics() {
    // By-value capture: copies take a snapshot
    int v = 10;
    T byval = [v]() { return v; };
    auto byval2 = byval; // deep-copy the lambda (v copied)
    v = 99;
    assert(byval()  == 10);
    assert(byval2() == 10);

    // By-reference capture: copies share the same referent
    int w = 10;
    T byref = [&w]() { return w; };
    auto byref2 = byref;
    w = 42;
    assert(byref()  == 42);
    assert(byref2() == 42);
}

template <typename T> // T = SimpleFunction<int(int,int)>
void test_self_move_assignment_is_safe() {
    T f = [](int a, int b){ return a + b; };
    // Self move-assign must not crash or leak; we expect it to remain callable.
    f = std::move(f);
    assert(f(2,3) == 5);
}

template <template <typename> class Fn>
void test_move_only_functor_is_rejected_at_construction() {
    struct MO {
        std::unique_ptr<int> p;
        int operator()() const { return *p; }
        MO(std::unique_ptr<int> q) : p(std::move(q)) {}
        MO(const MO&) = delete;      // non-copyable
        MO(MO&&) = default;
    };

    // Your implementations require copyability of the stored target for clone().
    // So constructing from MO should be ill-formed.
    static_assert(!std::is_constructible_v< Fn<int()>, MO >,
        "SimpleFunction should not be constructible from move-only targets");
    static_assert(!std::is_constructible_v< Fn<int()>, MO&& >,
        "SimpleFunction should not be constructible from move-only targets (rvalue)");
}

template <template <typename> class Fn>
void test_extra_overload_edges() {
    // Ensure function pointer overloads resolve as expected under cv-qualifiers.
    int (*pf_l)(int&)  = +[](int&  x){ return x += 1; };
    int (*pf_c)(const int&) = +[](const int& x){ return x + 2; };

    {
        Fn<int(int&)> f = pf_l;
        int a = 3;
        assert(f(a) == 4);
    }
    {
        Fn<int(const int&)> f = pf_c;
        const int a = 3;
        assert(f(a) == 5);
    }
}

template <size_t N>
void test_inplace_large_but_safe_functor() {
    // Functor with a reasonably large footprint that should still fit in buffers like 256/1024.
    struct Big {
        // ~96 bytes payload (adjust if you tweak capacities)
        alignas(std::max_align_t) unsigned char pad[96];
        int bias = 7;
        int operator()(int a, int b) const { return a + b + bias; }
    };
    using FN = sg::inplace::SimpleFunction<int(int,int), N>;
    FN f = Big{};
    assert(f(1,2) == 10);
}

// =============================================================
// Main — fan out each test across the three implementations
// =============================================================

int main() {
    // --- Keep your originals (deduped by templating) ---
    test_basic< sg::virt::SimpleFunction<int(int, int)>>() ;
    test_basic< sg::voidptr::SimpleFunction<int(int, int)>>() ;
    test_basic< sg::inplace::SimpleFunction<int(int, int), 1024>>() ;

    test_lambda< sg::virt::SimpleFunction<int(int, int)>>() ;
    test_lambda< sg::voidptr::SimpleFunction<int(int, int)>>() ;
    test_lambda< sg::inplace::SimpleFunction<int(int, int), 1024>>() ;

    test_stateful< sg::virt::SimpleFunction<int(int, int)>>() ;
    test_stateful< sg::voidptr::SimpleFunction<int(int, int)>>() ;
    test_stateful< sg::inplace::SimpleFunction<int(int, int), 1024>>() ;

    test_swap< sg::virt::SimpleFunction<int(int, int)>>() ;
    test_swap< sg::voidptr::SimpleFunction<int(int, int)>>() ;
    test_swap< sg::inplace::SimpleFunction<int(int, int), 1024>>() ;

    // --- New extras ---
    // Deep copy vs move (int(int))
    test_deep_copy_and_move_independence< sg::virt::SimpleFunction<int(int)>>() ;
    test_deep_copy_and_move_independence< sg::voidptr::SimpleFunction<int(int)>>() ;
    test_deep_copy_and_move_independence< sg::inplace::SimpleFunction<int(int), 1024>>() ;

    // Reference category correctness
    test_reference_categories<
        sg::virt::SimpleFunction<int(int&)>,
        sg::virt::SimpleFunction<int(int&&)>
    >();
    test_reference_categories<
        sg::voidptr::SimpleFunction<int(int&)>,
        sg::voidptr::SimpleFunction<int(int&&)>
    >();
    test_reference_categories<
        sg::inplace::SimpleFunction<int(int&), 1024>,
        sg::inplace::SimpleFunction<int(int&&), 1024>
    >();

    // Exceptions + empty
    test_exceptions_and_empty_more<
        sg::virt::SimpleFunction<int()>,
        sg::virt::SimpleFunction<int(int,int)>
    >();
    test_exceptions_and_empty_more<
        sg::voidptr::SimpleFunction<int()>,
        sg::voidptr::SimpleFunction<int(int,int)>
    >();
    test_exceptions_and_empty_more<
        sg::inplace::SimpleFunction<int() , 1024>,
        sg::inplace::SimpleFunction<int(int,int), 1024>
    >();

    // Explicit bool/reset flow
    test_bool_and_reset_paths< sg::virt::SimpleFunction<int(int,int)>>() ;
    test_bool_and_reset_paths< sg::voidptr::SimpleFunction<int(int,int)>>() ;
    test_bool_and_reset_paths< sg::inplace::SimpleFunction<int(int,int), 1024>>() ;

    // Self operations
    test_self_ops< sg::virt::SimpleFunction<int(int,int)>>() ;
    test_self_ops< sg::voidptr::SimpleFunction<int(int,int)>>() ;
    test_self_ops< sg::inplace::SimpleFunction<int(int,int), 1024>>() ;

    // Reassign + destruction
    test_reassign_and_destruction< sg::virt::SimpleFunction<int(int,int)>>() ;
    test_reassign_and_destruction< sg::voidptr::SimpleFunction<int(int,int)>>() ;
    test_reassign_and_destruction< sg::inplace::SimpleFunction<int(int,int), 1024>>() ;

    // In-place size invariant (a few capacities)
    test_inplace_size_invariant<64>();
    test_inplace_size_invariant<256>();
    test_inplace_size_invariant<1024>();

    test_const< sg::virt::SimpleFunction<int(int,int)>>() ;
    test_const< sg::voidptr::SimpleFunction<int(int,int)>>() ;
    test_const< sg::inplace::SimpleFunction<int(int,int), 1024>>() ;

    // Forwarding semantics (virt + void* versions)
    test_forwarding< sg::virt::SimpleFunction >();
    test_forwarding< sg::voidptr::SimpleFunction >();


    // --- Void return + throwing void target
    test_void_return_and_throw< sg::virt::SimpleFunction<void(int&,int)>>();
    test_void_return_and_throw< sg::voidptr::SimpleFunction<void(int&,int)>>();
    test_void_return_and_throw< sg::inplace::SimpleFunction<void(int&,int), 1024>>();

    // --- Mutable lambda under const call
    test_mutable_lambda_called_through_const< sg::virt::SimpleFunction<int()>>();
    test_mutable_lambda_called_through_const< sg::voidptr::SimpleFunction<int()>>();
    test_mutable_lambda_called_through_const< sg::inplace::SimpleFunction<int(), 1024>>();

    // --- Capture semantics
    test_capture_by_value_vs_reference_copy_semantics< sg::virt::SimpleFunction<int()>>();
    test_capture_by_value_vs_reference_copy_semantics< sg::voidptr::SimpleFunction<int()>>();
    test_capture_by_value_vs_reference_copy_semantics< sg::inplace::SimpleFunction<int(), 1024>>();

    // --- Self move-assign safety
    test_self_move_assignment_is_safe< sg::virt::SimpleFunction<int(int,int)>>();
    test_self_move_assignment_is_safe< sg::voidptr::SimpleFunction<int(int,int)>>();
    test_self_move_assignment_is_safe< sg::inplace::SimpleFunction<int(int,int), 1024>>();

    // --- Move-only functor must be rejected (compile-time)
    test_move_only_functor_is_rejected_at_construction< sg::virt::SimpleFunction >();
    test_move_only_functor_is_rejected_at_construction< sg::voidptr::SimpleFunction >();

    // --- Extra overload edges
    test_extra_overload_edges< sg::virt::SimpleFunction >();
    test_extra_overload_edges< sg::voidptr::SimpleFunction >();

    // --- In-place: large-but-safe functor (choose capacities you actually build)
    test_inplace_large_but_safe_functor<256>();
    test_inplace_large_but_safe_functor<1024>();

    return 0;
}