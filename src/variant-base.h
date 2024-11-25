#pragma once
#include "storage.h"

#include <exception>
#include <type_traits>

template <typename...>
class variant;

template <class T>
struct variant_size;

template <class... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <class T>
struct variant_size<const T> : variant_size<T> {};

template <class T>
constexpr std::size_t variant_size_v = variant_size<T>::value;

class bad_variant_access : public std::exception {
public:
  const char* what() const noexcept override {
    return "bad_variant_access";
  }
};

inline constexpr std::size_t variant_npos = std::size_t(-1);

namespace variant_detail {

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

template <typename... Types>
class variant_base {
public:
  constexpr variant_base()
      : _index(variant_npos) {}

  constexpr ~variant_base() = default;

  constexpr ~variant_base()
    requires (!std::conjunction_v<std::is_trivially_destructible<Types>...>)
  {
    if (_index != variant_npos) {
      storage::runtime::destroy(_index, _storage);
    }
  }

protected:
  storage::storage<Types...> _storage;
  std::size_t _index = 0;
};

} // namespace variant_detail
