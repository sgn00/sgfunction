#pragma once
#include <utility>
#include <stdexcept>

namespace sg::voidptr {

template<typename>
struct SimpleFunction;

template <typename F, typename Ret, typename... Params>
concept ValidFunctor = std::is_invocable_r_v<Ret, std::decay_t<F>&, Params...> &&
                      !std::same_as<std::decay_t<F>, SimpleFunction<Ret(Params...)>> &&
                          std::copy_constructible<std::decay_t<F>>;

template<typename R, typename... Args>
struct SimpleFunction<R(Args...)> {

    SimpleFunction() = default;

    template<typename F> requires ValidFunctor<F, R, Args...>
    SimpleFunction(F&& f) {
        using DecayedF = std::decay_t<F>;
        ptr_ = new DecayedF(std::forward<F>(f));

        invoker_ = [](void* ptr, Args&&... args) -> R {
            return (*static_cast<DecayedF*>(ptr))(std::forward<Args>(args)...);
        };

        deleter_ = [](void* ptr) {
            delete static_cast<DecayedF*>(ptr);
        };

        cloner_ = [](void* ptr) {
            auto* ret = new DecayedF(*static_cast<DecayedF*>(ptr));
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
            ptr_ = other.cloner_(other.ptr_);
            invoker_ = other.invoker_;
            deleter_ = other.deleter_;
            cloner_ = other.cloner_;
        }
    }

    SimpleFunction& operator=(const SimpleFunction& other) {
        SimpleFunction temp(other);
        this->swap(temp);
        return *this;
    }

    SimpleFunction(SimpleFunction&& other) noexcept
    : ptr_(other.ptr_), invoker_(other.invoker_),
        deleter_(other.deleter_), cloner_(other.cloner_) {
        other.ptr_ = nullptr;
        other.invoker_ = nullptr;
        other.deleter_ = nullptr;
        other.cloner_ = nullptr;
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

    void swap(SimpleFunction& other) noexcept {
        if (this == &other) return;
        std::swap(ptr_, other.ptr_);
        std::swap(invoker_, other.invoker_);
        std::swap(deleter_, other.deleter_);
        std::swap(cloner_, other.cloner_);
    }

    explicit operator bool() const noexcept { return invoker_ != nullptr; }
    void reset() noexcept { SimpleFunction{}.swap(*this); }

private:
    void* ptr_ = nullptr;

    using InvokerFn = R(*)(void*, Args&&...);
    using DeleterFn = void(*)(void*);
    using ClonerFn = void*(*)(void*);
    InvokerFn invoker_ = nullptr;
    DeleterFn deleter_ = nullptr;
    ClonerFn cloner_ = nullptr;
};

}

