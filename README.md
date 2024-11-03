# variant

Интерфейс, все свойства и гарантии должны соответствовать [std::variant](https://en.cppreference.com/w/cpp/utility/variant). Специализацию `std::hash` писать не надо.

По аналогии с заданием `optional`, требуется по возможности сохранять тривиальности для [special member functions](https://en.cppreference.com/w/cpp/language/member_functions#Special_member_functions). Также стоит уделить внимание правильности расстановки `noexcept`.
