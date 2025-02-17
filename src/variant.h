#pragma once

#include "non-narrowing-overload.h"
#include "storage.h"
#include "type-at-index.h"
#include "variant-classes.h"
#include "visit.h"

#include <type_traits>

inline constexpr std::size_t variant_npos = std::size_t(-1);

namespace variant_detail {

enum class property {
  present,
  trivial,
  deleted
};

template <template <typename> typename IsPresent, template <typename> typename IsTrivial, typename... Types>
inline constexpr property property_of =
    std::conjunction_v<IsTrivial<Types>...>
        ? property::trivial
        : (std::conjunction_v<IsPresent<Types>...> ? property::present : property::deleted);

template <typename T>
using is_copy_assignable = std::bool_constant<std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>>;

template <typename T>
using is_move_assignable = std::bool_constant<std::is_move_constructible_v<T> && std::is_move_assignable_v<T>>;

template <typename T>
using is_trivially_copy_assignable = std::bool_constant<
    std::is_trivially_copy_constructible_v<T> && std::is_trivially_copy_assignable_v<T> &&
    std::is_trivially_destructible_v<T>>;

template <typename T>
using is_trivially_move_assignable = std::bool_constant<
    std::is_trivially_move_constructible_v<T> && std::is_trivially_move_assignable_v<T> &&
    std::is_trivially_destructible_v<T>>;

template <typename T>
struct is_in_place_type : std::false_type {};

template <typename T>
struct is_in_place_type<in_place_type_t<T>> : std::true_type {};

template <typename T>
struct is_in_place_index : std::false_type {};

template <std::size_t I>
struct is_in_place_index<in_place_index_t<I>> : std::true_type {};

template <std::size_t I, typename Variant>
constexpr decltype(auto) get_impl(Variant&& v) {
  if (I != v.index()) {
    throw bad_variant_access{};
  }
  return storage::constant::get<I>(std::forward<Variant>(v)._storage);
}

} // namespace variant_detail

template <typename... Types>
  requires (sizeof...(Types) > 0)
class variant {
  using property = variant_detail::property;
  static constexpr property copy_construction =
      variant_detail::property_of<std::is_copy_constructible, std::is_trivially_copy_constructible, Types...>;

  static constexpr property move_construction =
      variant_detail::property_of<std::is_move_constructible, std::is_trivially_move_constructible, Types...>;

  static constexpr property copy_assignment = variant_detail::
      property_of<variant_detail::is_copy_assignable, variant_detail::is_trivially_copy_assignable, Types...>;

  static constexpr property move_assignment = variant_detail::
      property_of<variant_detail::is_move_assignable, variant_detail::is_trivially_move_assignable, Types...>;

  using T_0 = variant_detail::type_at_index_t<0, Types...>;

  template <typename Variant>
  constexpr void construct(Variant&& other) {
    variant_detail::visit_with_indices<void>(
        [this]<std::size_t Idx>(std::index_sequence<Idx>, Variant&& var) {
          variant_detail::storage::constant::construct<Idx>(this->_storage, get<Idx>(std::forward<Variant>(var)));
        },
        std::forward<Variant>(other)
    );
  }

public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<T_0>)
    requires (std::is_default_constructible_v<T_0>)
  {
    // https://github.com/CPP-KT/variant-postmodernist1848/actions/runs/13000547056/job/36258119741?pr=1
    variant_detail::storage::constant::construct<0>(_storage);
    _index = 0;
  }

  constexpr variant(const variant& other)
    requires (variant::copy_construction == property::present)
      : _index(other._index) {
    construct(other);
  }

  constexpr variant(const variant& other)
    requires (variant::copy_construction == property::trivial)
  = default;

  constexpr variant(const variant& other)
    requires (variant::copy_construction == property::deleted)
  = delete;

  constexpr variant(variant&& other) noexcept((std::is_nothrow_move_constructible_v<Types> && ...))
    requires (variant::move_construction == property::present)
      : _index(other._index) {
    construct(std::move(other));
  }

  constexpr variant(variant&& other)
    requires (variant::move_construction == property::trivial)
  = default;

  constexpr variant(variant&& other)
    requires (variant::move_construction == property::deleted)
  = delete;

  template <typename T>
    requires (!std::same_as<std::remove_cvref_t<T>, variant>) && (!variant_detail::is_in_place_index<T>::value) &&
             (!variant_detail::is_in_place_type<T>::value) &&
             std::is_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>
  constexpr variant(
      T&& t
  ) noexcept(std::is_nothrow_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>)
      : variant(in_place_type<variant_detail::non_narrowing_overload_t<T, Types...>>, std::forward<T>(t)) {}

  template <typename T, typename... Args>
    requires variant_detail::unique<T, Types...> && std::is_constructible_v<T, Args...>
  constexpr explicit variant(in_place_type_t<T>, Args&&... args)
      : variant(in_place_index<variant_detail::find_type_v<T, Types...>>, std::forward<Args>(args)...) {}

  template <std::size_t I, typename... Args>
    requires (I < sizeof...(Types)) && std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, Args...>
  constexpr explicit variant(in_place_index_t<I>, Args&&... args) {
    emplace<I>(std::forward<Args>(args)...);
  }

  template <typename T, typename... Args>
    requires variant_detail::unique<T, Types...> && (std::is_constructible_v<T, Args...>)
  T& emplace(Args&&... args) {
    return emplace<variant_detail::find_type_v<T, Types...>>(std::forward<Args>(args)...);
  }

  template <std::size_t I, typename... Args>
    requires (std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, Args...>)
  constexpr variant_alternative_t<I, variant>& emplace(Args&&... args) {
    if (!valueless_by_exception()) {
      destroy();
    }
    variant_detail::storage::constant::construct<I>(_storage, std::forward<Args>(args)...);
    _index = I;
    return variant_detail::storage::constant::get<I>(_storage);
  }

  constexpr variant& operator=(const variant& rhs)
    requires (copy_assignment == property::present)
  {
    if (valueless_by_exception() && rhs.valueless_by_exception()) {
      return *this;
    }

    if (rhs.valueless_by_exception()) {
      this->destroy();
      return *this;
    }

    if (this->_index == rhs._index) {
      variant_detail::storage::runtime::assign(this->_index, this->_storage, rhs._storage);
      return *this;
    }

    // Otherwise, if the alternative held by rhs is either nothrow copy constructible or not nothrow move
    // constructible
    // ..., equivalent to this->emplace<rhs.index()>(*std::get_if<rhs.index()>(std::addressof(rhs))).
    // Otherwise, equivalent to this->operator=(variant(rhs))
    visit(
        [this, &rhs](auto&& y) {
          using right_alternative = std::remove_cvref_t<decltype(y)>;
          if constexpr (std::is_nothrow_copy_constructible_v<right_alternative> ||
                        !std::is_nothrow_move_constructible_v<right_alternative>) {
            if (!valueless_by_exception()) {
              destroy();
            }
            construct(rhs);
            this->_index = rhs._index;
          } else {
            this->operator=(variant(rhs));
          }
        },
        rhs
    );
    return *this;
  }

  constexpr variant& operator=(const variant& rhs)
    requires (copy_assignment == property::trivial)
  = default;
  constexpr variant& operator=(const variant& rhs)
    requires (copy_assignment == property::deleted)
  = delete;

  constexpr variant& operator=(variant&& rhs
  ) noexcept(((std::is_nothrow_move_constructible_v<Types> && std::is_nothrow_move_assignable_v<Types>) && ...))
    requires (move_assignment == property::present)
  {
    if (valueless_by_exception() && rhs.valueless_by_exception()) {
      return *this;
    }
    if (rhs.valueless_by_exception()) {
      destroy();
      return *this;
    }
    if (this->_index == rhs._index) {
      variant_detail::storage::runtime::assign(this->_index, this->_storage, std::move(rhs)._storage);
      return *this;
    }

    if (!valueless_by_exception()) {
      destroy();
    }
    construct(std::move(rhs));
    this->_index = rhs._index;
    return *this;
  }

  constexpr variant& operator=(variant&& rhs)
    requires (move_assignment == property::trivial)
  = default;

  constexpr variant& operator=(variant&& rhs)
    requires (move_assignment == property::deleted)
  = delete;

  template <typename T>
    requires (!std::same_as<std::remove_cvref_t<T>, variant>) &&
             std::is_assignable_v<variant_detail::non_narrowing_overload_t<T, Types...>&, T> &&
             std::is_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>
  variant& operator=(T&& t
  ) noexcept(std::is_nothrow_assignable_v<variant_detail::non_narrowing_overload_t<T, Types...>&, T> && std::is_nothrow_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>) {
    using T_j = variant_detail::non_narrowing_overload_t<T, Types...>;
    if (T_j* pv = get_if<T_j>(this)) {
      *pv = std::forward<T>(t);
    } else if constexpr (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>) {
      emplace<T_j>(std::forward<T>(t));
    } else {
      emplace<T_j>(T_j(std::forward<T>(t)));
    }
    return *this;
  }

  constexpr ~variant() = default;

  constexpr ~variant()
    requires (!std::conjunction_v<std::is_trivially_destructible<Types>...>)
  {
    if (!valueless_by_exception()) {
      destroy();
    }
  }

  friend constexpr void swap(variant& lhs, variant<Types...>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
    lhs.swap(rhs);
  }

  constexpr void swap(variant& other) noexcept(
      ((std::is_nothrow_move_constructible_v<Types> && std::is_nothrow_swappable_v<Types>) && ...)
  ) {
    if (this->valueless_by_exception() && other.valueless_by_exception()) {
      return;
    }
    if (this->valueless_by_exception() && !other.valueless_by_exception()) {
      construct(std::move(other));
      this->_index = other._index;
      other.destroy();
      return;
    }
    if (!this->valueless_by_exception() && other.valueless_by_exception()) {
      other.construct(std::move(*this));
      other._index = this->_index;
      this->destroy();
      return;
    }

    variant_detail::visit_with_indices<void>(
        []<std::size_t I1, std::size_t I2>(std::index_sequence<I1, I2>, variant& lhs, variant& rhs) {
          if constexpr (I1 == I2) {
            using std::swap;
            swap(get<I1>(lhs), get<I2>(rhs));
          } else {
            variant tmp(std::move(lhs));
            lhs.destroy();
            lhs.construct(std::move(rhs));
            lhs._index = rhs._index;
            rhs.destroy();
            rhs.construct(std::move(tmp));
            rhs._index = tmp._index;
          }
        },
        *this,
        other
    );
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

  constexpr std::size_t index() const noexcept {
    return _index;
  }

private:
  constexpr void destroy() {
    visit([](auto& x) { std::destroy_at(std::addressof(x)); }, *this);
    _index = variant_npos;
  }

  variant_detail::storage::storage<Types...> _storage;
  std::size_t _index = variant_npos;

  template <std::size_t I, typename Variant>
  friend constexpr decltype(auto) variant_detail::get_impl(Variant&& v);

  template <typename R, typename Visitor, typename... Variants>
  friend constexpr R variant_detail::visit_impl(Visitor&& vis, Variants&&... vars);

  template <typename Visitor, typename... Variants>
  friend constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars);
};

template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
  if (pv && I == pv->index()) {
    return std::addressof(get<I>(*pv));
  } else {
    return nullptr;
  }
}

template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(const variant<Types...>* pv) noexcept {
  if (pv && I == pv->index()) {
    return std::addressof(get<I>(*pv));
  } else {
    return nullptr;
  }
}

template <typename T, typename... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
  return get_if<variant_detail::find_type_v<T, Types...>>(pv);
}

template <typename T, typename... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept {
  return get_if<variant_detail::find_type_v<T, Types...>>(pv);
}

// we don't want to expose a template <std::size_t, typename T> get(T) overload to the user?
template <std::size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant<Types...>>& get(const variant<Types...>& v) {
  return variant_detail::get_impl<I>(v);
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v) {
  return variant_detail::get_impl<I>(v);
}

template <std::size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant<Types...>>&& get(const variant<Types...>&& v) {
  return variant_detail::get_impl<I>(std::move(v));
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v) {
  return variant_detail::get_impl<I>(std::move(v));
}

template <typename T, typename... Types>
constexpr const T& get(const variant<Types...>& v) {
  return variant_detail::get_impl<variant_detail::find_type_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr T& get(variant<Types...>& v) {
  return variant_detail::get_impl<variant_detail::find_type_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr const T&& get(const variant<Types...>&& v) {
  return variant_detail::get_impl<variant_detail::find_type_v<T, Types...>>(std::move(v));
}

template <typename T, typename... Types>
constexpr T&& get(variant<Types...>&& v) {
  return variant_detail::get_impl<variant_detail::find_type_v<T, Types...>>(std::move(v));
}

namespace variant_detail {

template <typename... Types, typename Comp>
constexpr decltype(auto) comparison_operator(const variant<Types...>& v, const variant<Types...>& w, Comp comp) {
  if (w.valueless_by_exception() && v.valueless_by_exception()) {
    return comp(0, 0);
  }
  if (v.valueless_by_exception()) {
    return comp(0, 1);
  }
  if (w.valueless_by_exception()) {
    return comp(1, 0);
  }
  return variant_detail::visit_with_indices<decltype(comp(0, 0))>(
      [&comp]<std::size_t I1, std::size_t I2>(
          std::index_sequence<I1, I2>,
          const variant<Types...>& a,
          const variant<Types...>& b
      ) {
        if constexpr (I1 == I2) {
          return comp(get<I1>(a), get<I2>(b));
        } else {
          return comp(I1, I2);
        }
      },
      v,
      w
  );
}

} // namespace variant_detail

template <typename... Types>
constexpr bool operator==(const variant<Types...>& v, const variant<Types...>& w) {
  return variant_detail::comparison_operator(v, w, std::equal_to{});
}

template <typename... Types>
constexpr bool operator!=(const variant<Types...>& v, const variant<Types...>& w) {
  return variant_detail::comparison_operator(v, w, std::not_equal_to{});
}

template <typename... Types>
constexpr bool operator<(const variant<Types...>& v, const variant<Types...>& w) {
  return variant_detail::comparison_operator(v, w, std::less{});
}

template <typename... Types>
constexpr bool operator>(const variant<Types...>& v, const variant<Types...>& w) {
  return variant_detail::comparison_operator(v, w, std::greater{});
}

template <typename... Types>
constexpr bool operator<=(const variant<Types...>& v, const variant<Types...>& w) {
  return variant_detail::comparison_operator(v, w, std::less_equal{});
}

template <typename... Types>
constexpr bool operator>=(const variant<Types...>& v, const variant<Types...>& w) {
  return variant_detail::comparison_operator(v, w, std::greater_equal{});
}

template <typename... Types>
constexpr std::common_comparison_category_t<std::compare_three_way_result_t<Types>...>
operator<=>(const variant<Types...>& v, const variant<Types...>& w) {
  return variant_detail::comparison_operator(
      v,
      w,
      [](const auto& x, const auto& y) -> std::common_comparison_category_t<std::compare_three_way_result_t<Types>...> {
        return x <=> y;
      }
  );
}
