#pragma once

#include "variant-classes.h"

#include <utility>

namespace variant_detail {

// (sizeof...(Sizes)+1)-dimensional array that holds type T
template <typename T, std::size_t Size, std::size_t... Sizes>
struct table {
  table<T, Sizes...> arr[Size];

  // Construct is a class with a static template member 'value' of type T
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

template <typename R, typename Visitor, typename... Storages>
struct storage_visit_constructor {
  template <std::size_t... Is>
  static constexpr auto value = +[](Visitor&& vis, Storages&&... storages) -> R {
    return std::forward<Visitor>(vis)(storage::constant::get<Is>(std::forward<Storages>(storages))...);
  };
};

template <class R, class Visitor, class... Variants>
constexpr R visit_impl(Visitor&& vis, Variants&&... vars) {
  using namespace variant_detail;
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access{};
  }
  using table = table<
      R (*const)(Visitor&&, decltype((std::declval<Variants>()._storage))...),
      variant_size_v<std::remove_cvref_t<Variants>>...>;
  using Constructor = storage_visit_constructor<R, Visitor, decltype((std::declval<Variants>()._storage))...>;
  constexpr table tbl(Constructor{});
  return tbl(vars.index()...)(std::forward<Visitor>(vis), std::forward<Variants>(vars)._storage...);
}

} // namespace variant_detail

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) {
  using R = decltype(std::forward<Visitor>(vis)(
      variant_detail::storage::constant::get<0>(std::forward<Variants>(vars)._storage)...
  ));
  return variant_detail::visit_impl<R>(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <class R, class Visitor, class... Variants>
constexpr R visit(Visitor&& vis, Variants&&... vars) {
  return variant_detail::visit_impl<R>(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}
