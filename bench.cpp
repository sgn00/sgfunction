#include <benchmark/benchmark.h>
#include <functional>
#include <cstdlib>
#include <cstddef>

static long long g_allocs{0};

void* operator new(std::size_t sz) {
    g_allocs++;
    if (void* p = std::malloc(sz)) return p;
    throw std::bad_alloc();
}

static void reset_counts() {
    g_allocs = 0;
}

// ===== Sweep capture sizes to find the cutoff =====
template <size_t N>
struct Blob { char pad[N]; };

template <size_t N>
static void BM_SBO_Size(benchmark::State& st) {
    Blob<N> b{};
    for (auto _ : st) {
        reset_counts();
        {
            auto lam = [b]() { benchmark::DoNotOptimize(&b); };
            std::function<void()> f = lam; // construct
            benchmark::DoNotOptimize(f);
        }
        st.counters["new_calls"] = static_cast<double>(g_allocs);
    }
}

BENCHMARK_TEMPLATE(BM_SBO_Size, 0);
BENCHMARK_TEMPLATE(BM_SBO_Size, 8);
BENCHMARK_TEMPLATE(BM_SBO_Size, 16);
BENCHMARK_TEMPLATE(BM_SBO_Size, 24);
BENCHMARK_TEMPLATE(BM_SBO_Size, 32);
BENCHMARK_TEMPLATE(BM_SBO_Size, 40);

BENCHMARK_MAIN();
