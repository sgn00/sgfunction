#pragma once

#include <stdexcept>

namespace sg::inplace {

template<typename, size_t = 1024>
struct SimpleFunction;

template<class T>
struct is_any_simple_function : std::false_type {};

template<class Sig, size_t C>
struct is_any_simple_function<SimpleFunction<Sig, C>> : std::true_type {};

template <typename F, typename Ret, typename... Params>
concept ValidFunctor = std::is_invocable_r_v<Ret, std::decay_t<F>&, Params...> &&
                        !is_any_simple_function<std::decay_t<F>>::value  &&
                           std::copy_constructible<std::decay_t<F>>;

template<typename R, typename... Args, size_t CAPACITY>
struct SimpleFunction<R(Args...), CAPACITY> {

    SimpleFunction() = default;

    template<typename F> requires ValidFunctor<F, R, Args...>
    SimpleFunction(F&& f) {
        using DecayedF = std::decay_t<F>;
        // Check that our buffer can fit F at compile time
        static_assert(sizeof(DecayedF) <= BUF_CAP, "Callable too large for in-place buffer");
        static_assert(alignof(DecayedF) <= alignof(std::max_align_t), "Alignment mismatch");

        ptr_ = new (buffer_) DecayedF(std::forward<F>(f));

        invoker_ = [](void* ptr, Args&&... args) -> R {
            return (*static_cast<DecayedF*>(ptr))(std::forward<Args>(args)...);
        };
        deleter_ = [](void* ptr) {
            static_cast<DecayedF*>(ptr)->~DecayedF();
        };
        cloner_ = [](void* ptr, char* buf) {
            auto* ret = new (buf) DecayedF(*static_cast<DecayedF*>(ptr));
            return static_cast<void*>(ret);
        };
        mover_ = [](void* ptr, char* buf) {
            auto* ret = new (buf) DecayedF(std::move(*static_cast<DecayedF*>(ptr)));
            return static_cast<void*>(ret);
        };
    }

    ~SimpleFunction() noexcept {
        if (ptr_ && deleter_) {
            deleter_(ptr_);
        }
    }

    SimpleFunction(const SimpleFunction& other) {
        if (other.ptr_) {
            ptr_ = other.cloner_(other.ptr_, this->buffer_);
            invoker_ = other.invoker_;
            deleter_ = other.deleter_;
            cloner_ = other.cloner_;
            mover_ = other.mover_;
        }
    }

    SimpleFunction& operator=(const SimpleFunction& other) {
        SimpleFunction temp(other);
        this->swap(temp);
        return *this;
    }

    SimpleFunction(SimpleFunction&& other) {
        this->swap(other);
    }

    SimpleFunction& operator=(SimpleFunction&& other) {
        SimpleFunction temp(std::move(other));
        this->swap(temp);
        return *this;
    }

    R operator()(Args... args) const {
        if (!invoker_) {
            throw std::runtime_error("bad function call");
        }
        return invoker_(ptr_, std::forward<Args>(args)...);
    }

    // Note: not noexcept and not exception safe, can be implemented better.
    void swap(SimpleFunction& other) {
        if (this == &other) return;

        alignas(std::max_align_t) char tmp[sizeof(buffer_)];
        void* tptr = nullptr;

        MoverFn this_mover = mover_;
        DeleterFn this_deleter = deleter_;

        // 1) move *this -> tmp
        if (this_mover && ptr_) {
            tptr = this_mover(ptr_, tmp);
            this_deleter(ptr_);
            ptr_ = nullptr;
        }

        // 2) move other -> *this
        if (other.mover_ && other.ptr_) {
            ptr_ = other.mover_(other.ptr_, buffer_);
            other.deleter_(other.ptr_);
            other.ptr_ = nullptr;
        }

        // 3) move tmp -> other
        if (tptr) {
            if (this_mover) {
                other.ptr_ = this_mover(tptr, other.buffer_);
                this_deleter(tptr);
            }
        }

        std::swap(invoker_, other.invoker_);
        std::swap(deleter_, other.deleter_);
        std::swap(cloner_,  other.cloner_);
        std::swap(mover_,   other.mover_);
    }

    explicit operator bool() const noexcept { return invoker_ != nullptr; }
    void reset() noexcept { SimpleFunction{}.swap(*this); }

private:
    void* ptr_ = nullptr;

    using InvokerFn = R(*)(void*, Args&&...);
    using DeleterFn = void(*)(void*);
    using ClonerFn = void*(*)(void*, char*);
    using MoverFn = void*(*)(void*, char*);
    InvokerFn invoker_ = nullptr;
    DeleterFn deleter_ = nullptr;
    ClonerFn cloner_ = nullptr;
    MoverFn mover_ = nullptr;

    static constexpr size_t kMetaSize =
    sizeof(decltype(ptr_)) + sizeof(InvokerFn) + sizeof(DeleterFn) + sizeof(ClonerFn) + sizeof(MoverFn);
    static_assert(CAPACITY >= kMetaSize, "CAPACITY too small for metadata");
    static constexpr size_t BUF_CAP = CAPACITY - kMetaSize;

    alignas(std::max_align_t) char buffer_[BUF_CAP];
};

}

