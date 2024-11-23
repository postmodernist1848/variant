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
