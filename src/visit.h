#pragma once

#include "variant-classes.h"

#include <utility>

namespace variant_detail {

// (sizeof...(Sizes)+1)-dimensional array that holds type T
template <typename T, std::size_t Size, std::size_t... Sizes>
struct table {
  table<T, Sizes...> arr[Size];

  // Constructor is a class with a static template member 'value' of type T
  template <typename Constructor, std::size_t... PrevIs, std::size_t... Is>
  constexpr table(Constructor c, std::index_sequence<PrevIs...>, std::index_sequence<Is...>)
      : arr{table<T, Sizes...>(c, std::index_sequence<PrevIs..., Is>{})...} {}

  template <typename Constructor, std::size_t... PrevIs>
  constexpr explicit table(Constructor c, std::index_sequence<PrevIs...> is = std::index_sequence<>{})
      : table(c, is, std::make_index_sequence<Size>{}) {}

  template <typename... Indices>
  constexpr T operator()(std::size_t i, Indices... indices) const {
    return arr[i](indices...);
  }
};

template <typename T, std::size_t Size>
struct table<T, Size> {
  T arr[Size];

  template <typename Constructor, std::size_t... PrevIs, std::size_t... Is>
  constexpr table(Constructor, std::index_sequence<PrevIs...>, std::index_sequence<Is...>)
      : arr{Constructor::template value<PrevIs..., Is>...} {}

  template <typename Constructor, std::size_t... PrevIs>
  constexpr explicit table(Constructor c, std::index_sequence<PrevIs...> is = std::index_sequence<>{})
      : table(c, is, std::make_index_sequence<Size>{}) {}

  constexpr T operator()(std::size_t i) const {
    return arr[i];
  }
};

template <typename R, typename Visitor, typename... Variants>
struct visit_constructor {
  template <std::size_t... Is>
  static constexpr auto value = +[](Visitor&& vis, Variants&&... vars) -> R {
    return std::forward<Visitor>(vis)(std::index_sequence<Is...>{}, std::forward<Variants>(vars)...);
  };
};

template <typename R, typename Visitor, typename... Variants>
constexpr R visit_with_indices(Visitor&& vis, Variants&&... vars) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access{};
  }
  using table = table<R (*)(Visitor&&, Variants&&...), variant_size_v<std::remove_cvref_t<Variants>>...>;
  constexpr table tbl(visit_constructor<R, Visitor, Variants&&...>{});
  return tbl(vars.index()...)(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <typename R, typename Visitor, typename... Variants>
constexpr R visit_impl(Visitor&& vis, Variants&&... vars) {
  auto lambda = [&vis]<std::size_t... Is, typename... Variants2>(std::index_sequence<Is...>, Variants2&&... vars) -> R {
    return std::forward<Visitor>(vis)(get<Is>(std::forward<Variants2>(vars))...);
  };

  return visit_with_indices<R>(lambda, std::forward<Variants>(vars)...);
}

} // namespace variant_detail

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) {
  using R = decltype(std::forward<Visitor>(vis)(get<0>(std::forward<Variants>(vars))...));
  return variant_detail::visit_impl<R>(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <typename R, typename Visitor, typename... Variants>
constexpr R visit(Visitor&& vis, Variants&&... vars) {
  return variant_detail::visit_impl<R>(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}
