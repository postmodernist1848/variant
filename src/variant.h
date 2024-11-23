#pragma once

#include "non-narrowing-overload.h"
#include "type-by-index.h"
#include "variant-alternative.h"

#include <exception>
#include <iostream>

template <class T>
struct variant_size;

template <class... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

struct bad_variant_access : public std::exception {
  const char* what() const noexcept override {
    return "bad_variant_access";
  }
};

template <class T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <class T>
constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <std::size_t I>
constexpr in_place_index_t<I> in_place_index{};

inline constexpr std::size_t variant_npos = -1;

namespace variant_detail {

template <typename Type, typename... Types>
union storage {
  Type value;
  storage<Types...> next;
};

template <typename Type>
union storage<Type> {
  Type value;
};

template <typename Storage, typename... Types>
constexpr void construct_storage(std::size_t i, storage<Types...>& s, Storage&& other) {
  if (i == 0) {
    std::construct_at(std::addressof(s.value), std::forward<Storage>(other).value);
  } else if constexpr (sizeof...(Types) > 1) {
    construct_storage(i - 1, s.next, std::forward<Storage>(other).next);
  }
}

template <typename Storage, typename... Types>
constexpr void assign_storage(std::size_t i, storage<Types...>& s, Storage&& other) {
  if (i == 0) {
    s.value = std::forward<Storage>(other).value;
  } else if constexpr (sizeof...(Types) > 1) {
    assign_storage(i - 1, s.next, std::forward<Storage>(other).next);
  }
}

template <typename... Types>
constexpr void destroy_storage(std::size_t i, storage<Types...>& s) {
  if (i == 0) {
    std::destroy_at(std::addressof(s.value));
  } else if constexpr (sizeof...(Types) > 1) {
    destroy_storage(i - 1, s.next);
  }
}

template <std::size_t I, typename Storage>
constexpr auto&& storage_get(Storage&& v) {
  if constexpr (I == 0) {
    return std::forward<Storage>(v).value;
  } else {
    return storage_get<I - 1>(std::forward<Storage>(v).next);
  }
}

template <std::size_t I, typename Variant>
constexpr decltype(auto) get_impl(Variant&& v) {
  if (I != v.index()) {
    throw bad_variant_access{};
  }
  return storage_get<I>(std::forward<Variant>(v)._storage);
}

template <typename T>
struct is_in_place_type : std::false_type {};

template <typename T>
struct is_in_place_type<in_place_type_t<T>> : std::true_type {};

template <typename T>
struct is_in_place_index : std::false_type {};

template <std::size_t I>
struct is_in_place_index<in_place_index_t<I>> : std::true_type {};

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

template <property prop, typename... Types>
class variant_base_impl {
public:
  variant_base_impl() {}

  // present
  ~variant_base_impl() {
    destroy_storage(_index, _storage);
  }

private:
  variant_detail::storage<Types...> _storage;
  std::size_t _index = 0;
};

template <typename... Types>
class variant_base_impl<property::trivial, Types...> {
public:
  variant_detail::storage<Types...> _storage;
  std::size_t _index;
};

template <typename... Types>
class variant_base_impl<property::deleted, Types...> {
  static_assert(false, "This probably should not exist");
};

template <typename... Types>
using variant_base =
    variant_base_impl<property_of<std::is_destructible, std::is_trivially_destructible, Types...>, Types...>;

} // namespace variant_detail

template <class... Types>
class variant : variant_detail::variant_base<Types...> {
  static constexpr variant_detail::property copy_construction =
      variant_detail::property_of<std::is_copy_constructible, std::is_trivially_copy_constructible, Types...>;

  static constexpr variant_detail::property move_construction =
      variant_detail::property_of<std::is_move_constructible, std::is_trivially_move_constructible, Types...>;

  static constexpr variant_detail::property copy_assignment = variant_detail::
      property_of<variant_detail::is_copy_assignable, variant_detail::is_trivially_copy_assignable, Types...>;

  static constexpr variant_detail::property move_assignment = variant_detail::
      property_of<variant_detail::is_move_assignable, variant_detail::is_trivially_move_assignable, Types...>;

  using property = variant_detail::property;
  using T_0 = variant_detail::type_at_index_t<0, Types...>;

public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<T_0>)
    requires (std::is_default_constructible_v<T_0>)
  {
    std::construct_at(std::addressof(storage_get<0>(this->_storage)), T_0());
    this->_index = 0;
  }

  constexpr variant(const variant& other)
    requires (variant::copy_construction == property::present)
  {
    construct_storage(other._index, this->_storage, other._storage);
    this->_index = other._index;
  }

  constexpr variant(const variant& other)
    requires (variant::copy_construction == property::trivial)
  = default;

  constexpr variant(const variant& other)
    requires (variant::copy_construction == property::deleted)
  = delete;

  constexpr variant(variant&& other) noexcept((std::is_nothrow_move_constructible_v<Types> && ...))
    requires (variant::move_construction == property::present)
  {
    construct_storage(other._index, this->_storage, std::move(other._storage));
    this->_index = other._index;
  }

  constexpr variant(variant&& other)
    requires (variant::move_construction == property::trivial)
  = default;

  constexpr variant(variant&& other)
    requires (variant::move_construction == property::deleted)
  = delete;

  template <class T>
    requires (sizeof...(Types) > 0) && (!std::same_as<std::remove_cvref_t<T>, variant>) &&
             (!variant_detail::is_in_place_index<T>::value) && (!variant_detail::is_in_place_type<T>::value) &&
             std::is_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T> &&
             requires { variant_detail::id_function<T, Types...>::f(std::declval<T>()); }
  constexpr variant(T&& t
  ) noexcept(std::is_nothrow_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>) {
    constexpr std::size_t i =
        variant_detail::find_type_v<variant_detail::non_narrowing_overload_t<T, Types...>, Types...>;
    std::construct_at(std::addressof(storage_get<i>(this->_storage)), std::forward<T>(t));
    this->_index = i;
  }

  template <class T, class... Args>
    requires variant_detail::unique<T, Types...> && std::is_constructible_v<T, Args...>
  constexpr explicit variant(in_place_type_t<T>, Args&&... args) {
    constexpr std::size_t i = variant_detail::find_type_v<T, Types...>;
    std::construct_at(std::addressof(storage_get<i>(this->_storage)), std::forward<Args>(args)...);
    this->_index = i;
  }

  template <class T, class U, class... Args>
    requires variant_detail::unique<T, Types...> && std::is_constructible_v<T, std::initializer_list<U>&, Args...>
  constexpr explicit variant(std::in_place_type_t<T>, std::initializer_list<U> il, Args&&... args) {
    constexpr std::size_t i = variant_detail::find_type_v<T, Types...>;
    std::construct_at(std::addressof(storage_get<i>(this->_storage)), il, std::forward<Args>(args)...);
    this->_index = i;
  }

  template <std::size_t I, class... Args>
  constexpr explicit variant(std::in_place_index_t<I>, Args&&... args) {
    std::construct_at(std::addressof(storage_get<I>(this->_storage)), std::forward<Args>(args)...);
    this->_index = I;
  }

  template <std::size_t I, class U, class... Args>
  constexpr explicit variant(std::in_place_index_t<I>, std::initializer_list<U> il, Args&&... args) {
    std::construct_at(std::addressof(storage_get<I>(this->_storage)), il, std::forward<Args>(args)...);
    this->_index = I;
  }

  template <class T, class... Args>
    requires variant_detail::unique<T, Args...> && (std::is_constructible_v<T, Args...>)
  T& emplace(Args&&... args) {
    return emplace<variant_detail::find_type_v<T, Args...>>(std::forward<Args>(args)...);
  }

  template <class T, class U, class... Args>
    requires variant_detail::unique<T, Args...> && (std::is_constructible_v<T, std::initializer_list<U>&, Args...>)
  T& emplace(std::initializer_list<U> il, Args&&... args) {
    return emplace<variant_detail::find_type_v<T, Args...>>(il, std::forward<Args>(args)...);
  }

  template <std::size_t I, class... Args>
    requires (std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, Args...>)
  variant_alternative_t<I, variant>& emplace(Args&&... args) {
    if (!valueless_by_exception()) {
      destroy_storage(this->_index, this->_storage);
    }
    this->_index = variant_npos; // in case of exception
    std::construct_at(std::addressof(storage_get<I>(this->_storage)), std::forward<Args>(args)...);
    this->_index = I;
    return storage_get<I>(this->_storage);
  }

  template <std::size_t I, class U, class... Args>
    requires (std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, std::initializer_list<U>&, Args...>)
  variant_alternative_t<I, variant>& emplace(std::initializer_list<U> il, Args&&... args) {
    if (!valueless_by_exception()) {
      destroy_storage(this->_index, this->_storage);
    }
    this->_index = variant_npos;
    std::construct_at(std::addressof(storage_get<I>(this->_storage)), il, std::forward<Args>(args)...);
    this->_index = I;
    return storage_get<I>(this->_storage);
  }

  /*
  constexpr variant& operator=(variant&& rhs)
    requires (copy_assignment == property::present)
  {
    if (this == std::addressof(rhs)) {
      return *this;
    }
    if (rhs.valueless_by_exception()) {
      if (!valueless_by_exception()) {
        destroy_storage(this->_index, this->_storage);
        this->_index = variant_npos;
      }
      return *this;
    }

    if (this->_index == rhs.index()) {
      assign_storage(this->_index, this->_storage, rhs.storage);
      return *this;
    }
  }
   */

  constexpr variant& operator=(const variant& rhs)
    requires (copy_assignment == property::trivial)
  = default;
  constexpr variant& operator=(const variant& rhs)
    requires (copy_assignment == property::deleted)
  = delete;

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

  constexpr std::size_t index() const noexcept {
    return this->_index;
  }

  template <std::size_t I, typename Variant>
  friend constexpr decltype(auto) variant_detail::get_impl(Variant&& v);

  template <typename Visitor, typename... Types2>
  friend constexpr decltype(auto) visit(Visitor&& vis, variant<Types2...>& var);
};

// we don't want to expose a template <std::size_t, typename T> get(T) overload to the user
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
template <typename Visitor, typename... Types>
constexpr decltype(auto) visit_storage(Visitor&& vis, variant_detail::storage<Types...>& storage, std::size_t i) {
  if (i == 0) {
    return vis(storage.value);
  } else if constexpr (sizeof...(Types) > 1) {
    return visit_storage(std::forward<Visitor>(vis), storage.next, i - 1);
  }
}

} // namespace variant_detail

template <typename Visitor, typename... Types>
constexpr decltype(auto) visit(Visitor&& vis, variant<Types...>& var) {
  // constexpr auto overloads =
  // std::make_tuple(static_cast<std::invoke_result_t<Visitor, Types> (Visitor::*)(Types)>(&vis.operator())...);

  // return vis->*(arr(var[0]._index, var[1]._index, ...))();

  // return (vis->*overloads[var.index()])(var)
  // array<Visitor>(vars.index()...)(vis, vars)
  return variant_detail::visit_storage(std::forward<Visitor>(vis), var._storage, var._index);
}

template <class Visitor, class... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) {
  // array<Visitor>(vars.index()...)(vis, vars)
}
