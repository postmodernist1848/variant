#pragma once

#include "type-at-index.h"

template <typename... Types>
  requires (sizeof...(Types) > 0)
class variant;

template <std::size_t I, typename T>
struct variant_alternative;

template <std::size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
  using type = variant_detail::type_at_index_t<I, Types...>;
};

template <std::size_t I, typename T>
using variant_alternative_t = variant_alternative<I, T>::type;

template <std::size_t I, typename T>
struct variant_alternative<I, const T> : std::add_const<variant_alternative_t<I, T>> {};

template <typename T, typename... Types>
constexpr bool holds_alternative(const variant<Types...>& v) noexcept {
  return variant_detail::find_type_v<T, Types...> == v.index();
}

template <typename T>
struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<const T> : variant_size<T> {};

template <typename T>
constexpr std::size_t variant_size_v = variant_size<T>::value;

class bad_variant_access : public std::exception {
public:
  const char* what() const noexcept override {
    return "bad_variant_access";
  }
};

template <typename T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <typename T>
constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <std::size_t I>
constexpr in_place_index_t<I> in_place_index{};
