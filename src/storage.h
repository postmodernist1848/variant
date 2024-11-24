#pragma once
#include <memory>
#include <type_traits>

namespace variant_detail::storage {
template <typename Type, typename... Types>
union storage {
  constexpr storage() {}

  constexpr ~storage()
    requires (!(std::conjunction_v<std::is_trivially_destructible<Type>, std::is_trivially_destructible<Types>...>) )
  {}

  constexpr ~storage() = default;

  Type value;
  storage<Types...> next;
};

template <typename Type>
union storage<Type> {
  constexpr storage() {}

  constexpr ~storage()
    requires (!std::is_trivially_destructible_v<Type>)
  {}

  constexpr ~storage() = default;

  Type value;
};

namespace runtime {
// these functions operate on (possibly) runtime index of storage

template <typename Storage, typename... Types>
constexpr void construct(std::size_t i, storage<Types...>& s, Storage&& other) {
  if (i == 0) {
    std::construct_at(std::addressof(s.value), std::forward<Storage>(other).value);
  } else if constexpr (sizeof...(Types) > 1) {
    construct(i - 1, s.next, std::forward<Storage>(other).next);
  }
}

template <typename Storage, typename... Types>
constexpr void assign(std::size_t i, storage<Types...>& s, Storage&& other) {
  if (i == 0) {
    s.value = std::forward<Storage>(other).value;
  } else if constexpr (sizeof...(Types) > 1) {
    assign(i - 1, s.next, std::forward<Storage>(other).next);
  }
}

template <typename... Types>
constexpr void destroy(std::size_t i, storage<Types...>& s) {
  if (i == 0) {
    std::destroy_at(std::addressof(s.value));
  } else if constexpr (sizeof...(Types) > 1) {
    destroy(i - 1, s.next);
  }
}

} // namespace runtime

namespace constant {
// these functions operate on a compile-time constant index of storage

template <std::size_t I, typename... Types, typename... Args>
constexpr void construct(storage<Types...>& s, Args&&... args) {
  if constexpr (I == 0) {
    std::construct_at(std::addressof(s.value), std::forward<Args>(args)...);
  } else if constexpr (sizeof...(Types) > 1) {
    std::construct_at(&s.next); // activate next
    construct<I - 1>(s.next, std::forward<Args>(args)...);
  }
}

template <std::size_t I, typename Storage>
constexpr auto&& get(Storage&& v) {
  if constexpr (I == 0) {
    return std::forward<Storage>(v).value;
  } else {
    return get<I - 1>(std::forward<Storage>(v).next);
  }
}

} // namespace constant
} // namespace variant_detail::storage
