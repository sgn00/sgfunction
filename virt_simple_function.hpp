#pragma once

#include <memory>
#include <stdexcept>

namespace sg::virt {

template <typename>
struct SimpleFunction;

template <typename F, typename Ret, typename... Params>
concept ValidFunctor = std::is_invocable_r_v<Ret, std::decay_t<F>&, Params...> &&
                      !std::same_as<std::decay_t<F>, SimpleFunction<Ret(Params...)>> &&
                          std::copy_constructible<std::decay_t<F>>;


template <typename R, typename... Args>
struct SimpleFunction<R(Args...)> {

    struct CallableBase {
        virtual R invoke(Args&&... args) = 0;
        virtual std::unique_ptr<CallableBase> clone() const = 0;
        virtual ~CallableBase() = default;
    };

    template <typename F>
    struct CallableImpl : CallableBase {
        CallableImpl(F f) : functor(std::move(f)) {}

        R invoke(Args&&... args) override {
            return functor(std::forward<Args>(args)...);
        }

        std::unique_ptr<CallableBase> clone() const override {
            return std::make_unique<CallableImpl<F>>(*this);
        }
    private:
        F functor;
    };

    SimpleFunction() = default;

    template <typename F> requires ValidFunctor<F, R, Args...>
    SimpleFunction(F&& f) {
        using DecayedF = std::decay_t<F>;
        ptr_ = std::make_unique<CallableImpl<DecayedF>>(std::forward<F>(f));
    }

    R operator()(Args... args) const {
        if (!ptr_) {
            throw std::runtime_error("bad function call");
        }
        return ptr_->invoke(std::forward<Args>(args)...);
    }

    SimpleFunction(const SimpleFunction& other) {
        if (other.ptr_) {
            ptr_ = other.ptr_->clone();
        }
    }

    SimpleFunction& operator=(const SimpleFunction& other) {
        SimpleFunction temp(other);
        this->swap(temp);
        return *this;
    }

    SimpleFunction(SimpleFunction&& other) noexcept : ptr_(std::move(other.ptr_)) {}

    SimpleFunction& operator=(SimpleFunction&& other) noexcept {
        SimpleFunction temp(std::move(other));
        this->swap(temp);
        return *this;
    }

    void swap(SimpleFunction& other) noexcept {
        if (this == &other) return;
        std::swap(ptr_, other.ptr_);
    }

    explicit operator bool() const noexcept { return static_cast<bool>(ptr_); }
    void reset() noexcept { ptr_.reset(); }

private:
    std::unique_ptr<CallableBase> ptr_ = nullptr;

};

}



