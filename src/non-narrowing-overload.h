#pragma once

#include <utility>

namespace variant_detail {

// An overload F(T_i) is only considered if the declaration T_i x[] = {std::forward<T>(t)};
// is valid for some invented variable x.
template <typename T, typename U>
concept non_narrowing_convertible = requires (T t) { new U[1]{std::forward<T>(t)}; };

template <typename T, typename... Types>
struct id_function;

template <typename T, typename Head, typename... Types>
struct id_function<T, Head, Types...> : id_function<T, Types...> {
  using id_function<T, Types...>::f;

  static constexpr std::type_identity<Head> f(Head)
    requires non_narrowing_convertible<T, Head>;
};

template <typename T>
struct id_function<T> {
  static constexpr void f();
};

template <typename T, typename... Types>
concept f_well_formed = requires { id_function<T, Types...>::f(std::declval<T>()); };

template <typename T, typename... Types>
  requires f_well_formed<T, Types...>
struct non_narrowing_overload {
  using type = decltype(id_function<T, Types...>::f(std::declval<T>()))::type;
};

template <typename T, typename... Types>
using non_narrowing_overload_t = non_narrowing_overload<T, Types...>::type;

} // namespace variant_detail
