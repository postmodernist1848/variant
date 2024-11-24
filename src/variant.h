#pragma once

#include "base.h"
#include "non-narrowing-overload.h"
#include "storage.h"
#include "type-by-index.h"
#include "variant-alternative.h"
#include "visit.h"

#include <exception>

namespace variant_detail {

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

template <class... Types>
class variant : protected variant_detail::variant_base<Types...> {
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

public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<T_0>)
    requires (std::is_default_constructible_v<T_0>)
  {
    variant_detail::storage::constant::construct<0>(this->_storage);
    this->_index = 0;
  }

  constexpr variant(const variant& other)
    requires (variant::copy_construction == property::present)
  {
    variant_detail::storage::runtime::construct(other._index, this->_storage, other._storage);
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
    variant_detail::storage::runtime::construct(other._index, this->_storage, std::move(other)._storage);
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
             requires {
               variant_detail::id_function<T, Types...>::f(std::declval<T>());
             } && std::is_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>
  constexpr variant(T&& t
  ) noexcept(std::is_nothrow_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>) {
    constexpr std::size_t i =
        variant_detail::find_type_v<variant_detail::non_narrowing_overload_t<T, Types...>, Types...>;
    variant_detail::storage::constant::construct<i>(this->_storage, std::forward<T>(t));
    this->_index = i;
  }

  template <class T, class... Args>
    requires variant_detail::unique<T, Types...> && std::is_constructible_v<T, Args...>
  constexpr explicit variant(in_place_type_t<T>, Args&&... args) {
    constexpr std::size_t i = variant_detail::find_type_v<T, Types...>;
    variant_detail::storage::constant::construct<i>(this->_storage, std::forward<Args>(args)...);
    this->_index = i;
  }

  template <class T, class U, class... Args>
    requires variant_detail::unique<T, Types...> && std::is_constructible_v<T, std::initializer_list<U>&, Args...>
  constexpr explicit variant(in_place_type_t<T>, std::initializer_list<U> il, Args&&... args) {
    constexpr std::size_t i = variant_detail::find_type_v<T, Types...>;
    variant_detail::storage::constant::construct<i>(this->_storage, il, std::forward<Args>(args)...);
    this->_index = i;
  }

  template <std::size_t I, class... Args>
    requires (I < sizeof...(Types)) && std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, Args...>
  constexpr explicit variant(in_place_index_t<I>, Args&&... args) {
    variant_detail::storage::constant::construct<I>(this->_storage, std::forward<Args>(args)...);
    this->_index = I;
  }

  template <std::size_t I, class U, class... Args>
    requires (I < sizeof...(Types)) &&
             std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, std::initializer_list<U>&, Args...>
  constexpr explicit variant(in_place_index_t<I>, std::initializer_list<U> il, Args&&... args) {
    variant_detail::storage::constant::construct<I>(this->_storage, il, std::forward<Args>(args)...);
    this->_index = I;
  }

  template <class T, class... Args>
    requires variant_detail::unique<T, Types...> && (std::is_constructible_v<T, Args...>)
  T& emplace(Args&&... args) {
    return emplace<variant_detail::find_type_v<T, Types...>>(std::forward<Args>(args)...);
  }

  template <class T, class U, class... Args>
    requires variant_detail::unique<T, Types...> && (std::is_constructible_v<T, std::initializer_list<U>&, Args...>)
  T& emplace(std::initializer_list<U> il, Args&&... args) {
    return emplace<variant_detail::find_type_v<T, Types...>>(il, std::forward<Args>(args)...);
  }

  template <std::size_t I, class... Args>
    requires (std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, Args...>)
  variant_alternative_t<I, variant>& emplace(Args&&... args) {
    if (!valueless_by_exception()) {
      variant_detail::storage::runtime::destroy(this->_index, this->_storage);
      this->_index = variant_npos;
    }
    variant_detail::storage::constant::construct<I>(this->_storage, std::forward<Args>(args)...);
    this->_index = I;
    return variant_detail::storage::constant::get<I>(this->_storage);
  }

  template <std::size_t I, class U, class... Args>
    requires (std::is_constructible_v<variant_detail::type_at_index_t<I, Types...>, std::initializer_list<U>&, Args...>)
  variant_alternative_t<I, variant>& emplace(std::initializer_list<U> il, Args&&... args) {
    if (!valueless_by_exception()) {
      variant_detail::storage::runtime::destroy(this->_index, this->_storage);
      this->_index = variant_npos;
    }
    variant_detail::storage::constant::construct<I>(this->_storage, il, std::forward<Args>(args)...);
    this->_index = I;
    return variant_detail::storage::constant::get<I>(this->_storage);
  }

  constexpr variant& operator=(const variant& rhs)
    requires (copy_assignment == property::present)
  {
    if (this == &rhs) {
      return *this;
    }
    if (valueless_by_exception() && rhs.valueless_by_exception()) {
      return *this;
    }

    if (rhs.valueless_by_exception()) {
      variant_detail::storage::runtime::destroy(this->_index, this->_storage);
      this->_index = variant_npos;
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
              variant_detail::storage::runtime::destroy(this->_index, this->_storage);
              this->_index = variant_npos;
            }
            variant_detail::storage::runtime::construct(rhs._index, this->_storage, rhs._storage);
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
    if (this == &rhs) {
      return *this;
    }
    if (valueless_by_exception() && rhs.valueless_by_exception()) {
      return *this;
    }

    if (rhs.valueless_by_exception()) {
      variant_detail::storage::runtime::destroy(this->_index, this->_storage);
      this->_index = variant_npos;
    }

    if (this->_index == rhs._index) {
      variant_detail::storage::runtime::assign(this->_index, this->_storage, std::move(rhs)._storage);
      return *this;
    }
    if (!valueless_by_exception()) {
      variant_detail::storage::runtime::destroy(this->_index, this->_storage);
      this->_index = variant_npos;
    }
    variant_detail::storage::runtime::construct(rhs.index(), this->_storage, std::move(rhs)._storage);
    this->_index = rhs._index;
    return *this;
  }

  constexpr variant& operator=(variant&& rhs)
    requires (move_assignment == property::trivial)
  = default;

  /*
  constexpr variant& operator=(variant&& rhs)
    requires (move_assignment == property::deleted)
  = delete;
   */

  template <class T>
    requires (!std::same_as<std::remove_cvref_t<T>, variant>) &&
             requires { variant_detail::id_function<T, Types...>::f(std::declval<T>()); } &&
             std::is_assignable_v<variant_detail::non_narrowing_overload_t<T, Types...>&, T> &&
             std::is_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>
  variant& operator=(T&& t
  ) noexcept(std::is_nothrow_assignable_v<variant_detail::non_narrowing_overload_t<T, Types...>&, T> && std::is_nothrow_constructible_v<variant_detail::non_narrowing_overload_t<T, Types...>, T>) {
    using T_j = variant_detail::non_narrowing_overload_t<T, Types...>;
    if (T_j* pv = get_if<T_j>(this)) {
      *pv = std::forward<T>(t);
    } else if (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>) {
      this->emplace<T_j>(std::forward<T>(t));
    } else {
      this->emplace<T_j>(T_j(std::forward<T>(t)));
    }
    return *this;
  }

  friend constexpr void swap(variant& lhs, variant<Types...>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
    lhs.swap(rhs);
  }

  constexpr void swap(variant& rhs
  ) noexcept(((std::is_nothrow_move_constructible_v<Types> && std::is_nothrow_swappable_v<Types>) && ...)) {
    if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
      return;
    }
    if (this->valueless_by_exception() && !rhs.valueless_by_exception()) {
      variant_detail::storage::runtime::construct(rhs._index, this->_storage, std::move(rhs)._storage);
      this->_index = rhs._index;
      variant_detail::storage::runtime::destroy(rhs._index, rhs._storage);
      rhs._index = variant_npos;
      return;
    }
    if (!this->valueless_by_exception() && rhs.valueless_by_exception()) {
      variant_detail::storage::runtime::construct(this->_index, rhs._storage, std::move(*this)._storage);
      rhs._index = this->_index;
      variant_detail::storage::runtime::destroy(this->_index, this->_storage);
      this->_index = variant_npos;
      return;
    }
    if (this->index() == rhs.index()) {
      visit(
          [](auto& x, auto& y) {
            using std::swap;
            if constexpr (std::is_same_v<decltype(x), decltype(y)>) {
              swap(x, y);
            }
          },
          *this,
          rhs
      );
      return;
    }

    variant tmp(std::move(*this));
    variant_detail::storage::runtime::destroy(this->_index, this->_storage);
    this->_index = variant_npos;
    variant_detail::storage::runtime::construct(rhs._index, this->_storage, std::move(rhs)._storage);
    this->_index = rhs._index;

    variant_detail::storage::runtime::destroy(rhs._index, rhs._storage);
    rhs._index = variant_npos;
    variant_detail::storage::runtime::construct(tmp._index, rhs._storage, std::move(tmp)._storage);
    rhs._index = tmp._index;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

  constexpr std::size_t index() const noexcept {
    return this->_index;
  }

  template <std::size_t I, typename Variant>
  friend constexpr decltype(auto) variant_detail::get_impl(Variant&& v);

  template <class R, class Visitor, class... Variants>
  friend constexpr R variant_detail::visit_impl(Visitor&& vis, Variants&&... vars);

  template <typename Visitor, typename... Variants>
  friend constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars);
};

template <std::size_t I, class... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
  if (pv && I == pv->index()) {
    return std::addressof(get<I>(*pv));
  } else {
    return nullptr;
  }
}

template <std::size_t I, class... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>> get_if(const variant<Types...>* pv
) noexcept {
  if (pv && I == pv->index()) {
    return std::addressof(get<I>(*pv));
  } else {
    return nullptr;
  }
}

template <class T, class... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
  return get_if<variant_detail::find_type_v<T, Types...>>(pv);
}

template <class T, class... Types>
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

template <typename... Types>
constexpr bool operator==(const variant<Types...>& v, const variant<Types...>& w) {
  if (v.index() != w.index()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  return visit(
      [](auto&& a, auto&& b) -> bool {
        if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
          return a == b;
        } else {
          return false;
        }
      },
      v,
      w
  );
}

template <typename... Types>
constexpr bool operator!=(const variant<Types...>& v, const variant<Types...>& w) {
  if (v.index() != w.index()) {
    return true;
  }
  if (v.valueless_by_exception()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) -> bool {
        if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
          return a != b;
        } else {
          return false;
        }
      },
      v,
      w
  );
}

template <typename... Types>
constexpr bool operator<(const variant<Types...>& v, const variant<Types...>& w) {
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) -> bool {
        if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
          return a < b;
        } else {
          return false;
        }
      },
      v,
      w
  );
}

template <typename... Types>
constexpr bool operator>(const variant<Types...>& v, const variant<Types...>& w) {
  if (v.valueless_by_exception()) {
    return false;
  }
  if (w.valueless_by_exception()) {
    return true;
  }
  if (v.index() > w.index()) {
    return true;
  }
  if (v.index() < w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) -> bool {
        if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
          return a > b;
        } else {
          return false;
        }
      },
      v,
      w
  );
}

template <typename... Types>
constexpr bool operator<=(const variant<Types...>& v, const variant<Types...>& w) {
  if (v.valueless_by_exception()) {
    return true;
  }
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) -> bool {
        if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
          return a <= b;
        } else {
          return false;
        }
      },
      v,
      w
  );
}

template <typename... Types>
constexpr bool operator>=(const variant<Types...>& v, const variant<Types...>& w) {
  if (w.valueless_by_exception()) {
    return true;
  }
  if (v.valueless_by_exception()) {
    return false;
  }
  if (v.index() > w.index()) {
    return true;
  }
  if (v.index() < w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) -> bool {
        if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
          return a >= b;
        } else {
          return false;
        }
      },
      v,
      w
  );
}

template <typename... Types>
constexpr std::common_comparison_category_t<std::compare_three_way_result_t<Types>...>
operator<=>(const variant<Types...>& v, const variant<Types...>& w) {
  if (w.valueless_by_exception() && v.valueless_by_exception()) {
    return std::strong_ordering::equal;
  }
  if (v.valueless_by_exception()) {
    return std::strong_ordering::less;
  }
  if (w.valueless_by_exception()) {
    return std::strong_ordering::greater;
  }
  if (v.index() != w.index()) {
    return v.index() <=> w.index();
  }
  return visit(
      [](auto&& a, auto&& b) -> std::common_comparison_category_t<std::compare_three_way_result_t<Types>...> {
        if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
          return a <=> b;
        } else {
          return std::strong_ordering::less;
        }
      },
      v,
      w
  );
}
