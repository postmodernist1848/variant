#pragma once

#include "type-by-index.h"

template <class... Types>
class variant;

template <std::size_t I, typename T>
struct variant_alternative;

template <std::size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
  using type = variant_detail::type_at_index_t<I, Types...>;
};

template <std::size_t I, typename T>
using variant_alternative_t = variant_alternative<I, T>::type;

template <std::size_t I, class T>
struct variant_alternative<I, const T> : std::add_const<typename variant_alternative<I, T>::type> {};

template <class T, class... Types>
constexpr bool holds_alternative(const variant<Types...>& v) noexcept {
  return variant_detail::find_type_v<T, Types...> == v.index();
}
