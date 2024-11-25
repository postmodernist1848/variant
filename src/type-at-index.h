#pragma once
#include <cstddef>

namespace variant_detail {

template <std::size_t I, typename... Types>
struct type_at_index {
  static_assert(false, "invalid index");
};

template <std::size_t I, typename First, typename... Types>
struct type_at_index<I, First, Types...> {
  using type = type_at_index<I - 1, Types...>::type;
};

template <typename First, typename... Types>
struct type_at_index<0, First, Types...> {
  using type = First;
};

template <std::size_t I, typename... Types>
using type_at_index_t = type_at_index<I, Types...>::type;

template <typename T, typename... Types>
inline constexpr bool contains_type = (std::is_same_v<T, Types> || ...);

// find unique type T index in Types
template <typename T, typename... Types>
struct find_type {
  static_assert(false, "type not found");
};

template <typename T, typename... Types>
struct find_type<T, T, Types...> {
  inline static constexpr std::size_t value = 0;
  static_assert(!contains_type<T, Types...>, "type is not unique");
};

template <typename T, typename Head, typename... Types>
struct find_type<T, Head, Types...> {
  inline static constexpr std::size_t value = 1 + find_type<T, Types...>::value;
};

template <typename T, typename... Types>
inline constexpr std::size_t find_type_v = find_type<T, Types...>::value;

template <typename T, typename... Types>
concept unique = (std::size_t(0) + ... + (std::is_same_v<T, Types>) ) == 1;

} // namespace variant_detail
