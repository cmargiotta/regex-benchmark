#ifndef E_REGEX_E_REGEX_HPP_
#define E_REGEX_E_REGEX_HPP_

#ifndef E_REGEX_REGEX_HPP_
#define E_REGEX_REGEX_HPP_

#ifndef E_REGEX_MATCH_RESULT_HPP_
#define E_REGEX_MATCH_RESULT_HPP_

#include <array>
#include <cstddef>

#ifndef E_REGEX_NODES_COMMON_HPP_
#define E_REGEX_NODES_COMMON_HPP_

#include <algorithm>
#include <tuple>
#include <type_traits>

#ifndef E_REGEX_UTILITIES_ADMITTED_SET_HPP_
#define E_REGEX_UTILITIES_ADMITTED_SET_HPP_

#include <array>
#include <limits>
#include <utility>

namespace e_regex
{
    template<typename Char, Char... chars_>
    struct admitted_set
    {
            using Char_t                = Char;
            static constexpr auto empty = sizeof...(chars_) == 0;
            static constexpr std::array<Char, sizeof...(chars_)> chars {
                chars_...};

            static constexpr auto __attribute__((always_inline))
            can_admit(Char c) -> bool
            {
                return (... || (chars_ == c));
            }
    };

    // Generate an admitted_set from a range
    template<typename Char,
             Char start,
             Char end,
             typename seq = std::make_integer_sequence<unsigned, 1 + end - start>>
        requires(end > start)
    struct admitted_set_range;

    template<typename Char, Char start, Char end, auto... seq>
    struct admitted_set_range<Char, start, end, std::integer_sequence<unsigned, seq...>>
    {
            using type
                = admitted_set<Char, (static_cast<Char>(seq) + start)...>;
    };

    template<typename Char, Char start, Char end>
    using admitted_set_range_t =
        typename admitted_set_range<Char, start, end>::type;

    // Concatenate two admitted sets
    template<typename set1,
             typename set2,
             typename difference = admitted_set<typename set1::Char_t>>
    struct merge_admitted_sets;

    template<typename Char, Char common, Char... chars1, Char... chars2, Char... current>
    struct merge_admitted_sets<admitted_set<Char, common, chars1...>,
                               admitted_set<Char, common, chars2...>,
                               admitted_set<Char, current...>>
    {
            // Common char found
            using type = typename merge_admitted_sets<
                admitted_set<Char, chars1...>,
                admitted_set<Char, chars2...>,
                admitted_set<Char, current..., common>>::type;
    };

    template<typename Char, Char... chars2, Char... current>
    struct merge_admitted_sets<admitted_set<Char>,
                               admitted_set<Char, chars2...>,
                               admitted_set<Char, current...>>
    {
            // First set is empty, finished
            using type = admitted_set<Char, current..., chars2...>;
    };

    template<typename Char, Char... chars1, Char... current>
    struct merge_admitted_sets<admitted_set<Char, chars1...>,
                               admitted_set<Char>,
                               admitted_set<Char, current...>>
    {
            // Second set is empty, finished
            using type = admitted_set<Char, current..., chars1...>;
    };

    template<typename Char, Char... current>
    struct merge_admitted_sets<admitted_set<Char>,
                               admitted_set<Char>,
                               admitted_set<Char, current...>>
    {
            // Both sets are empty, finished
            using type = admitted_set<Char, current...>;
    };

    template<typename Char, Char head1, Char head2, Char... chars1, Char... chars2, Char... current>
        requires(head1 < head2)
    struct merge_admitted_sets<admitted_set<Char, head1, chars1...>,
                               admitted_set<Char, head2, chars2...>,
                               admitted_set<Char, current...>>
    {
            using type = typename merge_admitted_sets<
                admitted_set<Char, chars1...>,
                admitted_set<Char, head2, chars2...>,
                admitted_set<Char, current..., head1>>::type;
    };

    template<typename Char, Char head1, Char head2, Char... chars1, Char... chars2, Char... current>
        requires(head1 > head2)
    struct merge_admitted_sets<admitted_set<Char, head1, chars1...>,
                               admitted_set<Char, head2, chars2...>,
                               admitted_set<Char, current...>>
    {
            // head2 is in difference
            using type = typename merge_admitted_sets<
                admitted_set<Char, head1, chars1...>,
                admitted_set<Char, chars2...>,
                admitted_set<Char, current..., head2>>::type;
    };

    template<typename set1, typename set2>
    using merge_admitted_sets_t =
        typename merge_admitted_sets<set1, set2>::type;

    // Difference of two sets
    template<typename set1,
             typename set2,
             typename difference = admitted_set<typename set1::Char_t>>
    struct admitted_sets_difference;

    template<typename Char, Char common, Char... chars1, Char... chars2, Char... current>
    struct admitted_sets_difference<admitted_set<Char, common, chars1...>,
                                    admitted_set<Char, common, chars2...>,
                                    admitted_set<Char, current...>>
    {
            // Common char found, discard it
            using type = typename admitted_sets_difference<
                admitted_set<Char, chars1...>,
                admitted_set<Char, chars2...>,
                admitted_set<Char, current...>>::type;
    };

    template<typename Char, Char... chars2, Char... current>
    struct admitted_sets_difference<admitted_set<Char>,
                                    admitted_set<Char, chars2...>,
                                    admitted_set<Char, current...>>
    {
            // First set is empty, finished
            using type = admitted_set<Char, current..., chars2...>;
    };

    template<typename Char, Char... chars1, Char... current>
    struct admitted_sets_difference<admitted_set<Char, chars1...>,
                                    admitted_set<Char>,
                                    admitted_set<Char, current...>>
    {
            // Second set is empty, finished
            using type = admitted_set<Char, current..., chars1...>;
    };

    template<typename Char, Char... current>
    struct admitted_sets_difference<admitted_set<Char>,
                                    admitted_set<Char>,
                                    admitted_set<Char, current...>>
    {
            // Both sets are empty, finished
            using type = admitted_set<Char, current...>;
    };

    template<typename Char, Char head1, Char head2, Char... chars1, Char... chars2, Char... current>
        requires(head1 < head2)
    struct admitted_sets_difference<admitted_set<Char, head1, chars1...>,
                                    admitted_set<Char, head2, chars2...>,
                                    admitted_set<Char, current...>>
    {
            // head1 is in difference
            using type = typename admitted_sets_difference<
                admitted_set<Char, chars1...>,
                admitted_set<Char, head2, chars2...>,
                admitted_set<Char, current..., head1>>::type;
    };

    template<typename Char, Char head1, Char head2, Char... chars1, Char... chars2, Char... current>
        requires(head1 > head2)
    struct admitted_sets_difference<admitted_set<Char, head1, chars1...>,
                                    admitted_set<Char, head2, chars2...>,
                                    admitted_set<Char, current...>>
    {
            // head2 is in difference
            using type = typename admitted_sets_difference<
                admitted_set<Char, head1, chars1...>,
                admitted_set<Char, chars2...>,
                admitted_set<Char, current..., head2>>::type;
    };

    template<typename set1, typename set2>
    using admitted_sets_difference_t =
        typename admitted_sets_difference<set1, set2>::type;

    // Admitted set complement
    template<typename set, typename Char = typename set::Char_t>
    using admitted_set_complement_t = admitted_sets_difference_t<
        set,
        admitted_set_range_t<Char, 0, std::numeric_limits<Char>::max()>>;

    // Intersection of two sets
    template<typename set1,
             typename set2,
             typename intersection = admitted_set<typename set1::Char_t>>
    struct admitted_sets_intersection;

    template<typename Char, Char common, Char... chars1, Char... chars2, Char... current>
    struct admitted_sets_intersection<admitted_set<Char, common, chars1...>,
                                      admitted_set<Char, common, chars2...>,
                                      admitted_set<Char, current...>>
    {
            // Common char found
            using type = typename admitted_sets_intersection<
                admitted_set<Char, chars1...>,
                admitted_set<Char, chars2...>,
                admitted_set<Char, current..., common>>::type;
    };

    template<typename Char, Char... chars2, Char... current>
    struct admitted_sets_intersection<admitted_set<Char>,
                                      admitted_set<Char, chars2...>,
                                      admitted_set<Char, current...>>
    {
            // First set is empty, finished
            using type = admitted_set<Char, current...>;
    };

    template<typename Char, Char... chars1, Char... current>
    struct admitted_sets_intersection<admitted_set<Char, chars1...>,
                                      admitted_set<Char>,
                                      admitted_set<Char, current...>>
    {
            // Second set is empty, finished
            using type = admitted_set<Char, current...>;
    };

    template<typename Char, Char... current>
    struct admitted_sets_intersection<admitted_set<Char>,
                                      admitted_set<Char>,
                                      admitted_set<Char, current...>>
    {
            // Both sets are empty, finished
            using type = admitted_set<Char, current...>;
    };

    template<typename Char, Char head1, Char head2, Char... chars1, Char... chars2, Char... current>
        requires(head1 < head2)
    struct admitted_sets_intersection<admitted_set<Char, head1, chars1...>,
                                      admitted_set<Char, head2, chars2...>,
                                      admitted_set<Char, current...>>
    {
            // head1 is not common
            using type = typename admitted_sets_intersection<
                admitted_set<Char, chars1...>,
                admitted_set<Char, head2, chars2...>,
                admitted_set<Char, current...>>::type;
    };

    template<typename Char, Char head1, Char head2, Char... chars1, Char... chars2, Char... current>
        requires(head1 > head2)
    struct admitted_sets_intersection<admitted_set<Char, head1, chars1...>,
                                      admitted_set<Char, head2, chars2...>,
                                      admitted_set<Char, current...>>
    {
            // head2 is not common
            using type = typename admitted_sets_intersection<
                admitted_set<Char, head1, chars1...>,
                admitted_set<Char, chars2...>,
                admitted_set<Char, current...>>::type;
    };

    template<typename set1, typename set2>
    using admitted_sets_intersection_t =
        typename admitted_sets_intersection<set1, set2>::type;
} // namespace e_regex

#endif /* E_REGEX_UTILITIES_ADMITTED_SET_HPP_*/

#ifndef E_REGEX_UTILITIES_FIRST_TYPE_HPP_
#define E_REGEX_UTILITIES_FIRST_TYPE_HPP_

#include <tuple>

namespace e_regex
{
    template<typename data>
    struct first_type;

    template<typename head, typename... tail>
    struct first_type<std::tuple<head, tail...>>
    {
            using type      = head;
            using remaining = std::tuple<tail...>;
    };

    template<>
    struct first_type<std::tuple<>>
    {
            using type      = void;
            using remaining = std::tuple<>;
    };

    template<typename data>
    using first_type_t = typename first_type<data>::type;

} // namespace e_regex

#endif /* E_REGEX_UTILITIES_FIRST_TYPE_HPP_*/

#ifndef E_REGEX_UTILITIES_MATH_HPP_
#define E_REGEX_UTILITIES_MATH_HPP_

#include <algorithm>

namespace e_regex
{
    consteval auto sum()
    {
        return 0;
    }

    consteval auto sum(auto... n)
    {
        return (n + ...);
    }

    consteval auto min()
    {
        return 0;
    }

    consteval auto min(auto n)
    {
        return n;
    }

    consteval auto min(auto n1, auto n2, auto... tail)
    {
        return min(std::min(n1, n2), tail...);
    }

    consteval auto max()
    {
        return 0;
    }

    consteval auto max(auto n)
    {
        return n;
    }

    consteval auto max(auto n1, auto n2, auto... tail)
    {
        return max(std::max(n1, n2), tail...);
    }

} // namespace e_regex

#endif /* E_REGEX_UTILITIES_MATH_HPP_*/

namespace e_regex::nodes
{
    template<typename... children>
    struct extract_admission_set;

    template<typename child, typename... children>
    struct extract_admission_set<child, children...>
    {
            using type = merge_admitted_sets_t<
                typename decltype(child::meta)::admission_set,
                typename extract_admission_set<children...>::type>;
    };

    template<typename... children>
    struct extract_admission_set<void, children...>
    {
            using type =
                typename extract_admission_set<children...>::type;
    };

    template<>
    struct extract_admission_set<>
    {
            using type = admitted_set<char>;
    };

    template<typename T>
    concept has_groups = requires(T t) { t.groups; };

    template<typename T>
    struct group_getter
    {
            static constexpr auto value = 0U;
    };

    template<has_groups T>
    struct group_getter<T>
    {
            static constexpr auto value = T::groups;
    };

    template<typename T>
    concept has_group_index = requires(T t) { t.next_group_index; };

    template<typename T>
    struct group_index_getter
    {
            static constexpr auto value = 0;
    };

    template<has_groups T>
    struct group_index_getter<T>
    {
            static constexpr auto value = T::next_group_index;
    };

    template<typename matcher, typename... children>
    struct base
    {
            static constexpr auto next_group_index
                = max(group_index_getter<matcher>::value,
                      group_index_getter<children>::value...);

            static constexpr unsigned groups
                = group_getter<matcher>::value
                  + sum(group_getter<children>::value...);
    };

    template<typename injected_children>
    struct invoke_match;

    template<typename... injected_children>
    struct invoke_match<std::tuple<injected_children...>>
    {
            template<typename T>
            static constexpr __attribute__((always_inline)) auto
                match(auto& res) -> auto&
            {
                return T::template match<injected_children...>(res);
            }
    };

    template<typename nodes,
             typename terminals = std::tuple<>,
             typename others    = std::tuple<>>
    struct zipper;

    template<typename node, typename... tail, typename... terminals, typename... others>
    struct zipper<std::tuple<node, tail...>,
                  std::tuple<terminals...>,
                  std::tuple<others...>>
    {};

    template<typename children          = std::tuple<>,
             typename injected_children = std::tuple<>>
    constexpr __attribute__((always_inline)) auto
        dfs(auto& match_result) noexcept -> auto&
    {
        if constexpr (std::tuple_size_v<children> == 0)
        {
            return match_result;
        }
        else
        {
            using _children = first_type<children>;
            using invoker   = invoke_match<injected_children>;

            if constexpr (std::tuple_size_v<children> == 1)
            {
                return invoker::template match<typename _children::type>(
                    match_result);
            }
            else
            {
                if (invoker::template match<typename _children::type>(
                        match_result))
                {
                    return match_result;
                }

                match_result.accepted = true;
                return dfs<typename _children::remaining>(match_result);
            }
        }
    }
} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_COMMON_HPP_*/

#ifndef E_REGEX_UTILITIES_LITERAL_STRING_VIEW_HPP_
#define E_REGEX_UTILITIES_LITERAL_STRING_VIEW_HPP_

#include <functional>
#include <string_view>

namespace e_regex
{
    template<typename Type = char>
    struct literal_string_view
    {
            using iterator = const Type*;

            iterator begin_;
            iterator end_;

            constexpr __attribute__((always_inline)) literal_string_view()
                : begin_ {nullptr}, end_ {nullptr}
            {}

            constexpr __attribute__((always_inline))
            literal_string_view(iterator begin_, iterator end_)
                : begin_ {begin_}, end_ {end_}
            {}

            template<unsigned size>
            constexpr __attribute__((always_inline))
            literal_string_view(const char (&data)[size]) noexcept
                : begin_ {std::begin(data)}, end_ {std::end(data) - 1}
            {}

            constexpr __attribute__((always_inline))
            literal_string_view(std::string_view data)
                : begin_ {data.begin()}, end_ {data.end()}
            {}

            constexpr literal_string_view(const literal_string_view& other) noexcept
                = default;
            constexpr literal_string_view(literal_string_view&& other) noexcept
                = default;
            constexpr auto operator=(const literal_string_view& other) noexcept
                -> literal_string_view& = default;
            constexpr auto operator=(literal_string_view&& other) noexcept
                -> literal_string_view&               = default;
            constexpr ~literal_string_view() noexcept = default;

            constexpr auto __attribute__((always_inline)) begin() noexcept
            {
                return begin_;
            }

            constexpr auto __attribute__((always_inline)) end() noexcept
            {
                return end_;
            }

            constexpr auto __attribute__((always_inline)) begin() const noexcept
            {
                return begin_;
            }

            constexpr auto __attribute__((always_inline)) end() const noexcept
            {
                return end_;
            }

            constexpr auto __attribute__((always_inline)) size() const noexcept
            {
                return end_ - begin_;
            }

            constexpr __attribute__((always_inline))
            operator std::string_view() const noexcept
            {
                return std::string_view {begin(), end()};
            }

            constexpr __attribute__((always_inline)) bool
                operator==(literal_string_view other) const
            {
                if (begin() == other.begin() && end() == other.end())
                {
                    return true;
                }

                if (size() != other.size())
                {
                    return false;
                }

                for (auto i = 0; i < size(); ++i)
                {
                    if (begin()[i] != other.begin()[i])
                    {
                        return false;
                    }
                }

                return true;
            }

            constexpr __attribute__((always_inline)) auto empty() const noexcept
            {
                return size() == 0;
            }
    };
} // namespace e_regex

#endif /* E_REGEX_UTILITIES_LITERAL_STRING_VIEW_HPP_*/

namespace e_regex
{
    template<unsigned groups, typename Char_Type>
    struct match_result_data
    {
            literal_string_view<Char_Type> query;
            typename literal_string_view<Char_Type>::iterator actual_iterator_start;
            typename literal_string_view<Char_Type>::iterator actual_iterator_end;
            std::array<literal_string_view<Char_Type>, groups> match_groups
                = {};
            bool accepted = true;

            constexpr auto __attribute__((always_inline))
            operator=(bool accepted) noexcept -> match_result_data&
            {
                this->accepted = accepted;

                return *this;
            }

            constexpr
                __attribute__((always_inline)) operator bool() const noexcept
            {
                return accepted;
            }
    };

    template<typename matcher, typename Char_Type = char>
    class match_result
    {
        public:
            static constexpr auto expression = matcher::expression;

        private:
            static constexpr auto groups_
                = nodes::group_getter<matcher>::value;

            match_result_data<groups_, Char_Type> data;

        public:
            constexpr __attribute__((always_inline))
            match_result(literal_string_view<> query) noexcept
            {
                data.query                 = query;
                data.actual_iterator_start = query.begin();
                data.actual_iterator_end = data.actual_iterator_start;

                data = matcher::match(data);
                if (!data)
                {
                    next();
                }
            }

            constexpr __attribute__((always_inline)) auto
                operator=(bool accepted) noexcept -> match_result&
            {
                this->accepted = accepted;

                return *this;
            }

            constexpr __attribute__((always_inline)) auto begin() const noexcept
            {
                return match_result {data.query};
            }

            constexpr __attribute__((always_inline)) auto
                operator!=(const match_result& other) const noexcept
            {
                return data != other.data;
            }

            constexpr __attribute__((always_inline)) auto
                operator++() noexcept -> auto&
            {
                next();

                return *this;
            }

            constexpr __attribute__((always_inline)) auto end() const noexcept
            {
                auto res = *this;
                res.data.actual_iterator_start
                    = res.data.actual_iterator_end;
                res.data.accepted = false;

                return res;
            }

            constexpr __attribute__((always_inline)) auto operator*() const noexcept
            {
                return to_view();
            }

            constexpr
                __attribute__((always_inline)) operator bool() const noexcept
            {
                return data;
            }

            constexpr __attribute__((always_inline)) auto is_accepted() const noexcept
            {
                return operator bool();
            }

            template<unsigned index>
            constexpr __attribute__((always_inline)) auto get() const noexcept
            {
                static_assert(
                    index <= matcher::groups,
                    "Group index is greater than the number of groups.");

                return get_group(index);
            }

            constexpr __attribute__((always_inline)) auto
                get_group(unsigned index) const noexcept
            {
                if (index == 0)
                {
                    return to_view();
                }

                return static_cast<std::string_view>(
                    data.match_groups[index - 1]);
            }

            constexpr __attribute__((always_inline)) auto
                operator[](unsigned index) const noexcept
            {
                return get_group(index);
            }

            constexpr __attribute__((always_inline)) auto to_view() const noexcept
            {
                if (!is_accepted())
                {
                    return std::string_view {};
                }

                return std::string_view {data.actual_iterator_start,
                                         data.actual_iterator_end};
            }

            constexpr __attribute__((always_inline))
            operator literal_string_view<Char_Type>() const noexcept
            {
                return to_view();
            }

            constexpr __attribute__((always_inline)) auto size() const noexcept
            {
                return data.matches;
            }

            static __attribute__((always_inline)) constexpr auto groups() noexcept
            {
                return groups_;
            }

            /**
             * @brief Iterate matches
             *
             * @return false if there are no other matches
             */
            constexpr __attribute__((always_inline)) auto next() noexcept
            {
                data.match_groups = {};
                data.actual_iterator_start = data.actual_iterator_end;

                while (data.actual_iterator_start < data.query.end())
                {
                    matcher::match(data);

                    if (data.accepted)
                    {
                        return true;
                    }

                    data.actual_iterator_start
                        = ++data.actual_iterator_end;
                }

                data.accepted = false;
                return false;
            }
    };
} // namespace e_regex

// For structured decomposition
namespace std
{
    template<typename matcher>
    struct tuple_size<e_regex::match_result<matcher>>
    {
            static const unsigned value = matcher::groups + 1;
    };

    template<std::size_t N, typename matcher, typename Char_Type>
    struct tuple_element<N, e_regex::match_result<matcher, Char_Type>>
    {
            static_assert(N <= e_regex::nodes::group_getter<matcher>::value);

            using type = std::string_view;
    };

    template<unsigned N, typename matcher>
    constexpr __attribute__((always_inline)) auto
        get(e_regex::match_result<matcher> t) noexcept
    {
        return t.template get<N>();
    }

} // namespace std

#endif /* E_REGEX_MATCH_RESULT_HPP_*/

#ifndef E_REGEX_TOKENIZATION_TOKEN_HPP_
#define E_REGEX_TOKENIZATION_TOKEN_HPP_

#include <array>

#ifndef E_REGEX_UTILITIES_STATIC_STRING_HPP_
#define E_REGEX_UTILITIES_STATIC_STRING_HPP_

#include <algorithm>
#include <array>
#include <cstddef>
#include <string_view>
#include <type_traits>

namespace e_regex
{
    template<unsigned size_, typename Char_Type = char>
    struct static_string
    {
            static constexpr auto size = size_;
            std::conditional_t<(size > 0), std::array<char, size>, std::tuple<>> data;

            template<auto S>
            constexpr static_string(const Char_Type (&data)[S]) noexcept
            {
                if constexpr (size > 0)
                {
                    std::copy(data, data + size, this->data.begin());
                }
            }

            constexpr static_string(Char_Type data) noexcept
            {
                this->data[0] = data;
            }

            template<auto S = size>
                requires(size != 0)
            constexpr static_string(const std::array<Char_Type, size>& data) noexcept
                : data {data}
            {}

            // template<literal_string_view string>
            // constexpr static_string();

            constexpr static_string() noexcept = default;

            constexpr operator literal_string_view<Char_Type>() const noexcept
            {
                return to_view();
            }

            constexpr auto to_view() const noexcept
            {
                if constexpr (size > 0)
                {
                    return std::string_view {data.begin(), data.end()};
                }
                else
                {
                    return std::string_view {};
                }
            }

            [[nodiscard]] constexpr bool empty() const noexcept
            {
                return size == 0;
            }

            template<unsigned begin, unsigned end = size>
            constexpr auto substring() const noexcept
            {
                if constexpr (size > 0)
                {
                    static_string<end - begin> result;

                    std::copy(data.begin() + begin,
                              data.begin() + end,
                              result.data.begin());
                    return result;
                }
                else
                {
                    return *this;
                }
            }
    };

    template<auto S1, auto S2, typename C>
    constexpr auto operator+(static_string<S1, C> first,
                             static_string<S2, C> other)
    {
        if constexpr (first.size == 0)
        {
            return other;
        }
        else if constexpr (other.size == 0)
        {
            return first;
        }
        else
        {
            static_string<S1 + S2> result {};

            std::copy(first.data.begin(),
                      first.data.end(),
                      result.data.begin());
            std::copy(other.data.begin(),
                      other.data.end(),
                      result.data.begin() + S1);

            return result;
        }
    }

    template<auto S1, auto S2, typename C>
    constexpr auto operator+(const C (&first)[S1],
                             static_string<S2, C> other)
    {
        return static_string {first} + other;
    }

    template<auto S1, auto S2, typename C>
    constexpr auto operator+(static_string<S1, C> first,
                             const C (&other)[S2])
    {
        return first + static_string {other};
    }

    template<auto S2, typename C>
    constexpr auto operator+(C first, static_string<S2, C> other)
    {
        return static_string {first} + other;
    }

    template<auto S1, typename C>
    constexpr auto operator+(static_string<S1, C> first, C other)
    {
        return first + static_string {other};
    }

    // CTAD
    template<typename C>
    static_string(C data) -> static_string<1, C>;

    template<auto S, typename C>
    static_string(const C (&data)[S]) -> static_string<S - 1, C>;

} // namespace e_regex

#endif /* E_REGEX_UTILITIES_STATIC_STRING_HPP_*/

namespace e_regex
{
    template<typename T, auto size>
    struct token
    {
            T                   type;
            static_string<size> matcher;

            template<auto S>
            constexpr token(T type, const char (&matcher)[S])
                : type {type}, matcher {matcher}
            {}
    };

    template<typename T, auto S>
    token(T type, const char (&matcher)[S]) -> token<T, S - 1>;

    template<auto size>
    struct separator
    {
            static_string<size> matcher;

            template<auto S>
            constexpr separator(const char (&matcher)[S])
                : matcher {matcher}
            {}
    };

    template<auto S>
    separator(const char (&matcher)[S]) -> separator<S - 1>;

    template<typename T>
    struct is_token : public std::false_type
    {};

    template<typename T, auto size>
    struct is_token<e_regex::token<T, size>> : public std::true_type
    {};

    template<typename T>
    concept is_token_c = is_token<T>::value;

    template<typename T>
    struct is_separator : public std::false_type
    {};

    template<auto size>
    struct is_separator<e_regex::separator<size>> : public std::true_type
    {};

    template<typename T>
    concept is_separator_c = is_separator<T>::value;

    template<typename T>
    concept token_definition = is_separator_c<T> || is_token_c<T>;

} // namespace e_regex

#endif /* E_REGEX_TOKENIZATION_TOKEN_HPP_*/

#ifndef E_REGEX_TREE_BUILDER_HPP_
#define E_REGEX_TREE_BUILDER_HPP_

#ifndef NODES_HPP
#define NODES_HPP

#ifndef E_REGEX_NODES_GET_EXPRESSION_HPP_
#define E_REGEX_NODES_GET_EXPRESSION_HPP_

#include <tuple>

#ifndef E_REGEX_UTILITIES_PACK_STRING_HPP_
#define E_REGEX_UTILITIES_PACK_STRING_HPP_

namespace e_regex
{
    template<char... data>
    struct pack_string;

    namespace __private
    {
        /* Merge two or more pack strings into one */
        template<typename... Strings>
        struct merge_pack_strings;

        template<char... data1, char... data2, typename... Strings>
        struct merge_pack_strings<pack_string<data1...>, pack_string<data2...>, Strings...>
        {
                using type =
                    typename merge_pack_strings<pack_string<data1..., data2...>,
                                                Strings...>::type;
        };

        template<typename String>
        struct merge_pack_strings<String>
        {
                using type = String;
        };

        template<>
        struct merge_pack_strings<>
        {
                using type = pack_string<>;
        };

        /* Merge two or more pack strings into one and add a separator
         * between them */
        template<typename separator, typename... strings>
        struct concatenate_pack_strings;

        template<typename separator, typename string>
        struct concatenate_pack_strings<separator, string>
        {
                using type = string;
        };

        template<typename separator>
        struct concatenate_pack_strings<separator>
        {
                using type = pack_string<>;
        };

        template<typename separator, typename... strings>
            requires(sizeof...(strings) > 0)
        struct concatenate_pack_strings<separator, pack_string<>, strings...>
            : public concatenate_pack_strings<separator, strings...>
        {
                // Skip empty strings
        };

        template<typename separator, typename string, typename... strings>
            requires(sizeof...(strings) > 0)
        struct concatenate_pack_strings<separator, string, strings...>
        {
                using type = typename merge_pack_strings<
                    string,
                    separator,
                    typename concatenate_pack_strings<separator, strings...>::type>::type;
        };
    } // namespace __private

    /*
     * String unrolled as template parameters
     */
    template<char... data>
    struct pack_string
    {
            static constexpr auto size   = sizeof...(data);
            static constexpr auto string = []() {
                if constexpr (size > 0)
                {
                    return static_string<size> {std::array {data...}};
                }
                else
                {
                    return static_string {""};
                }
            }();

            template<typename... others>
            using merge =
                typename __private::merge_pack_strings<others...>::type;

            template<typename separator, typename... others>
            using concatenate =
                typename __private::concatenate_pack_strings<separator,
                                                             others...>::type;
    };

    template<typename... args>
    using concatenate_pack_strings_t =
        typename __private::concatenate_pack_strings<args...>::type;

    template<typename... args>
    using merge_pack_strings_t =
        typename __private::merge_pack_strings<args...>::type;

    /*
     * Build a pack string from a static string
     */
    template<static_string instance,
             typename indices
             = std::make_integer_sequence<unsigned, instance.size>>
    struct build_pack_string;

    template<static_string instance, unsigned... indices>
    struct build_pack_string<instance, std::integer_sequence<unsigned, indices...>>
    {
            using type = pack_string<instance.data[indices]...>;
    };

    template<static_string instance>
    using build_pack_string_t =
        typename build_pack_string<instance>::type;
} // namespace e_regex

#endif /* E_REGEX_UTILITIES_PACK_STRING_HPP_*/

namespace e_regex::nodes
{
    template<typename... children>
    constexpr __attribute__((always_inline)) auto get_children_expression()
    {
        if constexpr (sizeof...(children) == 0)
        {
            return static_string {""};
        }
        else if constexpr (sizeof...(children) == 1)
        {
            return std::tuple_element_t<0, std::tuple<children...>>::expression;
        }
        else
        {
            return concatenate_pack_strings_t<
                pack_string<'|'>,
                build_pack_string_t<("(?:" + children::expression + ")")>...>::string;
        }
    }
} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_GET_EXPRESSION_HPP_*/

#ifndef E_REGEX_NODES_GREEDY_HPP_
#define E_REGEX_NODES_GREEDY_HPP_

#include <concepts>
#include <cstddef>
#include <limits>
#include <type_traits>

#ifndef E_REGEX_NODES_POSSESSIVE_HPP_
#define E_REGEX_NODES_POSSESSIVE_HPP_

#include <cstddef>
#include <limits>

#ifndef E_REGEX_NODES_BASIC_HPP_
#define E_REGEX_NODES_BASIC_HPP_

#include <concepts>
#include <tuple>
#include <type_traits>

#ifndef E_REGEX_NODES_META_HPP_
#define E_REGEX_NODES_META_HPP_

#include <array>

namespace e_regex
{
    enum class policy
    {
        GREEDY,
        LAZY,
        POSSESSIVE,
        EXACT,
        NONE
    };

    template<typename _admission_set>
    struct meta
    {
            policy   policy_;
            unsigned minimum_match_size;
            unsigned maximum_match_size;

            using admission_set = _admission_set;
    };
} // namespace e_regex

#endif /* E_REGEX_NODES_META_HPP_*/

#ifndef E_REGEX_TERMINALS_HPP_
#define E_REGEX_TERMINALS_HPP_

#ifndef E_REGEX_TERMINALS_ALARM_HPP_
#define E_REGEX_TERMINALS_ALARM_HPP_

#ifndef E_REGEX_TERMINALS_COMMON_HPP_
#define E_REGEX_TERMINALS_COMMON_HPP_

#include <utility>

namespace e_regex::terminals
{
    template<typename terminal, typename admission_set>
    struct terminal_common
    {
            template<typename... injected_children>
            using optimize = terminal;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_            = e_regex::policy::NONE,
                .minimum_match_size = 1,
                .maximum_match_size = 1,
            };

            // Template used only for compatibility with nodes
            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                if (result.actual_iterator_end >= result.query.end())
                {
                    result.accepted = false;
                    return result;
                }

                return terminal::match_(result);
            }
    };

    template<typename terminal, typename admission_set>
    struct negated_terminal
        : public terminal_common<negated_terminal<terminal, admission_set>,
                                 admitted_set_complement_t<admission_set>>
    {
            template<typename... injected_children>
            using optimize = negated_terminal;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match_(auto& result) -> auto&
            {
                result = terminal::match_(std::move(result));
                result = !result.accepted;

                return result;
            }
    };

    template<typename... identifiers>
    struct terminal;
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_COMMON_HPP_*/

#ifndef E_REGEX_TERMINALS_EXACT_MATCHER_HPP_
#define E_REGEX_TERMINALS_EXACT_MATCHER_HPP_

namespace e_regex::terminals
{
    template<typename identifier>
    struct exact_matcher;

    template<char identifier, char... identifiers>
    struct exact_matcher<pack_string<identifier, identifiers...>>
        : public terminal_common<
              exact_matcher<pack_string<identifier, identifiers...>>,
              admitted_set<char, identifier, identifiers...>>
    {
            static constexpr auto expression
                = pack_string<identifier, identifiers...>::string;

            static constexpr auto meta
                = e_regex::meta<admitted_set<char, identifier, identifiers...>> {
                    .policy_            = e_regex::policy::EXACT,
                    .minimum_match_size = sizeof...(identifiers) + 1,
                    .maximum_match_size = sizeof...(identifiers) + 1,
                };

            static constexpr __attribute__((always_inline)) auto
                match_(auto& result) -> auto&
            {
                const auto start = result.actual_iterator_end;
                result.accepted  = true;

                for (const auto c:
                     pack_string<identifier, identifiers...>::string.to_view())
                {
                    if (c != *result.actual_iterator_end)
                    {
                        result.actual_iterator_end = start;
                        result.accepted            = false;
                        return result;
                    }

                    result.actual_iterator_end++;
                }

                return result;
            }
    };

    template<typename identifier>
    struct terminal<identifier> : public exact_matcher<identifier>
    {
            static constexpr bool exact = true;
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_EXACT_MATCHER_HPP_*/

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'\\', 'a'>>
        : public exact_matcher<pack_string<0x07>>
    {
            static constexpr auto expression = static_string {"\\a"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_ALARM_HPP_*/

#ifndef ANCHORS_ANCHORS
#define ANCHORS_ANCHORS

#ifndef E_REGEX_TERMINALS_ANCHORS_END_HPP_
#define E_REGEX_TERMINALS_ANCHORS_END_HPP_

namespace e_regex::terminals::anchors
{
    struct end
    {
            static constexpr auto expression = static_string {"$"};

            static constexpr auto meta = e_regex::meta<
                admitted_set_complement_t<admitted_set<char, '\n'>>> {
                .policy_            = e_regex::policy::NONE,
                .minimum_match_size = 0,
                .maximum_match_size = 1,
            };

            template<typename... injected_children>
            using optimize = end;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                if (result.actual_iterator_end > result.query.end())
                {
                    result.accepted = false;
                    return result;
                }

                result
                    = (result.actual_iterator_end == result.query.end())
                      || *result.actual_iterator_end == '\n';

                if (*result.actual_iterator_end == '\n')
                {
                    result.actual_iterator_end++;
                }

                return result;
            }
    };
} // namespace e_regex::terminals::anchors

#endif /* E_REGEX_TERMINALS_ANCHORS_END_HPP_*/

#ifndef E_REGEX_TERMINALS_ANCHORS_START_HPP_
#define E_REGEX_TERMINALS_ANCHORS_START_HPP_

namespace e_regex::terminals::anchors
{
    struct start
    {
            static constexpr auto expression = static_string {"^"};

            static constexpr auto meta
                = e_regex::meta<admitted_set_complement_t<admitted_set<char>>> {
                    .policy_            = e_regex::policy::NONE,
                    .minimum_match_size = 0,
                    .maximum_match_size = 0,
                };

            template<typename... injected_children>
            using optimize = start;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                if (result.actual_iterator_end >= result.query.end())
                {
                    result.accepted = false;
                    return result;
                }

                result = (result.actual_iterator_end
                          == result.query.begin());

                return result;
            }
    };
} // namespace e_regex::terminals::anchors

#endif /* E_REGEX_TERMINALS_ANCHORS_START_HPP_*/

#endif /* ANCHORS_ANCHORS */

#ifndef E_REGEX_TERMINALS_ANY_HPP_
#define E_REGEX_TERMINALS_ANY_HPP_

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'.'>>
        : public terminal_common<terminal<pack_string<'.'>>,
                                 admitted_set_range_t<char, '\0', '\x7F'>>
    {
            static constexpr auto expression = static_string {"."};

            static constexpr __attribute__((always_inline)) auto
                match_(auto& result) -> auto&
            {
                result = *result.actual_iterator_end != '\n';

                if (result)
                {
                    result.actual_iterator_end++;
                }

                return result;
            }
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_ANY_HPP_*/

#ifndef E_REGEX_NODES_TERMINALS_CARRIAGE_RETURN_HPP_
#define E_REGEX_NODES_TERMINALS_CARRIAGE_RETURN_HPP_

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'\\', 'r'>>
        : public exact_matcher<pack_string<'\r'>>
    {
            static constexpr auto expression = static_string {"\\r"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_NODES_TERMINALS_CARRIAGE_RETURN_HPP_*/

#ifndef E_REGEX_TERMINALS_DIGIT_CHARACTERS_HPP_
#define E_REGEX_TERMINALS_DIGIT_CHARACTERS_HPP_

#ifndef E_REGEX_TERMINALS_RANGE_HPP_
#define E_REGEX_TERMINALS_RANGE_HPP_

namespace e_regex::terminals
{
    template<typename start, typename end>
    struct range_terminal;

    template<char start, char end>
    struct range_terminal<pack_string<start>, pack_string<end>>
        : public terminal_common<
              range_terminal<pack_string<start>, pack_string<end>>,
              admitted_set_range_t<char, start, end>>
    {
            static constexpr auto expression
                = pack_string<'[', start, '-', end, ']'>::string;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match_(auto& result) -> auto&
            {
                static_assert(end >= start,
                              "Range [a-b] must respect b >= a");

                const auto& current = *result.actual_iterator_end;

                result = current >= start && current <= end;
                if (result)
                {
                    result.actual_iterator_end++;
                }

                return result;
            }
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_RANGE_HPP_*/

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'\\', 'd'>>
        : public range_terminal<pack_string<'0'>, pack_string<'9'>>
    {
            static constexpr auto expression = static_string {"\\d"};
    };

    template<>
    struct terminal<pack_string<'\\', 'D'>>
        : public negated_terminal<terminal<pack_string<'\\', 'd'>>,
                                  admitted_set_range_t<char, '0', '9'>>
    {
            static constexpr auto expression = static_string {"\\D"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_DIGIT_CHARACTERS_HPP_*/

#ifndef E_REGEX_NODES_TERMINALS_ESCAPE_HPP_
#define E_REGEX_NODES_TERMINALS_ESCAPE_HPP_

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'\\', 'e'>>
        : public exact_matcher<pack_string<0x1B>>
    {
            static constexpr auto expression = static_string {"\\e"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_NODES_TERMINALS_ESCAPE_HPP_*/

#ifndef E_REGEX_NODES_TERMINALS_ESCAPED_HPP_
#define E_REGEX_NODES_TERMINALS_ESCAPED_HPP_

namespace e_regex::terminals
{
    template<char identifier>
    struct terminal<pack_string<'\\', identifier>>
        : public exact_matcher<pack_string<identifier>>
    {
            static constexpr auto expression
                = pack_string<'\\', identifier>::string;
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_NODES_TERMINALS_ESCAPED_HPP_*/

#ifndef E_REGEX_NODES_TERMINALS_FORM_FEED_HPP_
#define E_REGEX_NODES_TERMINALS_FORM_FEED_HPP_

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'\\', 'f'>>
        : public exact_matcher<pack_string<0x0C>>
    {
            static constexpr auto expression = static_string {"\\f"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_NODES_TERMINALS_FORM_FEED_HPP_*/

#ifndef E_REGEX_TERMINALS_NEW_LINE_HPP_
#define E_REGEX_TERMINALS_NEW_LINE_HPP_

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'\\', 'n'>>
        : public exact_matcher<pack_string<'\n'>>
    {
            static constexpr auto expression = static_string {"\\n"};
    };

    template<>
    struct terminal<pack_string<'\\', 'N'>>
        : public negated_terminal<terminal<pack_string<'\\', 'n'>>,
                                  admitted_set<char, '\n'>>
    {
            static constexpr auto expression = static_string {"\\N"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_NEW_LINE_HPP_*/

#ifndef E_REGEX_TERMINALS_SPACE_CHARACTERS_HPP_
#define E_REGEX_TERMINALS_SPACE_CHARACTERS_HPP_

#include <algorithm>
#include <array>

namespace e_regex::terminals
{
    using space_characters_admission_set
        = admitted_set<char, '\t', '\n', '\f', '\r', ' '>;

    template<>
    struct terminal<pack_string<'\\', 's'>>
        : public terminal_common<terminal<pack_string<'\\', 's'>>, space_characters_admission_set>
    {
            static constexpr auto expression = static_string {"\\s"};

            static constexpr __attribute__((always_inline)) auto
                match_(auto& result) -> auto&
            {
                constexpr std::array matched {
                    '\t', '\n', '\f', '\r', ' '};

                result = std::find(matched.begin(),
                                   matched.end(),
                                   *result.actual_iterator_end)
                         != matched.end();

                result.actual_iterator_end++;

                return result;
            }
    };

    template<>
    struct terminal<pack_string<'\\', 'S'>>
        : public negated_terminal<terminal<pack_string<'\\', 's'>>,
                                  space_characters_admission_set>
    {
            static constexpr auto expression = static_string {"\\S"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_SPACE_CHARACTERS_HPP_*/

#ifndef E_REGEX_NODES_TERMINALS_TAB_HPP_
#define E_REGEX_NODES_TERMINALS_TAB_HPP_

namespace e_regex::terminals
{
    template<>
    struct terminal<pack_string<'\\', 't'>>
        : public exact_matcher<pack_string<'\t'>>
    {
            static constexpr auto expression = static_string {"\\t"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_NODES_TERMINALS_TAB_HPP_*/

#ifndef E_REGEX_TERMINALS_WORD_CHARACTERS_HPP_
#define E_REGEX_TERMINALS_WORD_CHARACTERS_HPP_

namespace e_regex::terminals
{
    using word_characters_admission_set = merge_admitted_sets_t<
        merge_admitted_sets_t<admitted_set_range_t<char, 'A', 'Z'>,
                              admitted_set_range_t<char, 'a', 'z'>>,
        merge_admitted_sets_t<admitted_set_range_t<char, '0', '9'>,
                              admitted_set<char, '_'>>>;

    template<>
    struct terminal<pack_string<'\\', 'w'>>
        : public terminal_common<terminal<pack_string<'\\', 'w'>>, word_characters_admission_set>
    {
            static constexpr auto expression = static_string {"\\w"};

            static constexpr __attribute__((always_inline)) auto
                match_(auto& result) -> auto&
            {
                const auto& current = *result.actual_iterator_end;

                result = (current >= 'A' && current <= 'Z')
                         || (current >= 'a' && current <= 'z')
                         || (current >= '0' && current <= '9')
                         || (current == '_');

                if (result)
                {
                    result.actual_iterator_end++;
                }

                return result;
            }
    };

    template<>
    struct terminal<pack_string<'\\', 'W'>>
        : public negated_terminal<terminal<pack_string<'\\', 'w'>>, word_characters_admission_set>
    {
            static constexpr auto expression = static_string {"\\W"};
    };
} // namespace e_regex::terminals

#endif /* E_REGEX_TERMINALS_WORD_CHARACTERS_HPP_*/

#endif /* E_REGEX_TERMINALS_HPP_*/

namespace e_regex::nodes
{
    template<typename matcher>
    struct admitted_first_chars_getter
    {
            using type = typename matcher::admitted_first_chars;
    };

    template<>
    struct admitted_first_chars_getter<void>
    {
            using type = admitted_set<char>;
    };

    template<typename matcher, typename... children>
    struct simple : public base<matcher, children...>
    {
            static constexpr auto expression = []() {
                if constexpr (sizeof...(children) <= 1)
                {
                    return matcher::expression
                           + get_children_expression<children...>();
                }
                else
                {
                    return matcher::expression + "(?:"
                           + get_children_expression<children...>()
                           + ')';
                }
            }();

            using admission_set = std::conditional_t<
                matcher::meta.minimum_match_size == 0,
                typename extract_admission_set<matcher, children...>::type,
                typename extract_admission_set<matcher>::type>;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = policy::NONE,
                .minimum_match_size
                = matcher::meta.minimum_match_size
                  + min(children::meta.minimum_match_size...),
                .maximum_match_size
                = matcher::meta.maximum_match_size
                  + max(children::meta.minimum_match_size...),
            };

            template<typename... injected_children>
            using optimize
                = simple<typename matcher::template optimize<injected_children...>,
                         typename children::template optimize<>...>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& res) -> auto&
            {
                auto begin = res.actual_iterator_end;

                matcher::match(res);

                if (res)
                {
                    if (!dfs<std::tuple<children...>,
                             std::tuple<injected_children...>>(res))
                    {
                        res.actual_iterator_end = begin;
                    }
                }

                return res;
            }
    };

    template<typename... children>
    struct simple<void, children...> : public base<void, children...>
    {
            using child
                = std::tuple_element_t<0, std::tuple<children...>>;

            static constexpr auto expression
                = get_children_expression<children...>();

            using admission_set = std::conditional_t<
                child::meta.minimum_match_size == 0,
                typename extract_admission_set<children...>::type,
                typename extract_admission_set<child>::type>;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = policy::NONE,
                .minimum_match_size
                = min(children::meta.minimum_match_size...),
                .maximum_match_size
                = max(children::meta.minimum_match_size...),
            };

            template<typename... injected_children>
            using optimize
                = simple<void, typename children::template optimize<>...>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& res) -> auto&
            {
                return dfs<std::tuple<children...>,
                           std::tuple<injected_children...>>(res);
            }
    };

    template<typename child>
    struct simple<void, child> : public base<void, child>
    {
            static constexpr auto expression
                = get_children_expression<child>();

            using admission_set =
                typename extract_admission_set<child>::type;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_            = policy::NONE,
                .minimum_match_size = child::meta.minimum_match_size,
                .maximum_match_size = child::meta.minimum_match_size,
            };

            template<typename... injected_children>
            using optimize =
                typename child::template optimize<injected_children...>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& res) -> auto&
            {
                return child::template match<injected_children...>(res);
            }
    };

    template<typename matcher>
    struct simple<matcher> : public base<matcher>
    {
            static constexpr auto expression = matcher::expression;

            using admission_set =
                typename extract_admission_set<matcher>::type;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = policy::NONE,
                .minimum_match_size = matcher::meta.minimum_match_size,
                .maximum_match_size = matcher::meta.maximum_match_size,
            };

            template<typename... injected_children>
            using optimize =
                typename matcher::template optimize<injected_children...>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& res) -> auto&
            {
                return matcher::match(res);
            }
    };

} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_BASIC_HPP_*/

#ifndef UTILITIES_NUMBER_TO_PACK_STRING_HPP
#define UTILITIES_NUMBER_TO_PACK_STRING_HPP

#include <concepts>

namespace e_regex
{
    consteval bool nonzero(auto number)
    {
        return number != 0;
    };

    template<auto number, typename current_string>
        requires std::integral<decltype(number)>
    struct number_to_pack_string;

    template<auto number, char... current>
        requires(!nonzero(number))
    struct number_to_pack_string<number, pack_string<current...>>
    {
            using type = pack_string<current...>;
    };

    template<auto number, char... current>
        requires(nonzero(number))
    struct number_to_pack_string<number, pack_string<current...>>
    {
            static constexpr auto number_10_shifted = number / 10;
            static constexpr auto current_unit      = number - (number_10_shifted * 10);

            using type =
                typename number_to_pack_string<number_10_shifted,
                                               pack_string<(current_unit + '0'), current...>>::type;
    };

    template<>
    struct number_to_pack_string<0, pack_string<>>
    {
            using type = pack_string<'0'>;
    };

    template<auto number>
    using number_to_pack_string_t = typename number_to_pack_string<number, pack_string<>>::type;
}// namespace e_regex

#endif /* UTILITIES_NUMBER_TO_PACK_STRING_HPP */

namespace e_regex::nodes
{
    template<typename matcher,
             unsigned repetitions_min,
             unsigned repetitions_max = std::numeric_limits<unsigned>::max(),
             typename... children>
    struct possessive : public base<matcher, children...>
    {
            // If matcher is optional (aka repetitions_min==0),
            // admission set must include children too
            using admission_set = std::conditional_t<
                repetitions_min == 0,
                typename extract_admission_set<matcher, children...>::type,
                typename decltype(matcher::meta)::admission_set>;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = e_regex::policy::POSSESSIVE,
                .minimum_match_size
                = matcher::meta.minimum_match_size * repetitions_min
                  + min(children::meta.minimum_match_size...),
                .maximum_match_size
                = repetitions_max == std::numeric_limits<unsigned>::max()
                      ? std::numeric_limits<unsigned>::max()
                      : matcher::meta.maximum_match_size * repetitions_max
                            + max(children::meta.maximum_match_size...),
            };

            static constexpr auto expression = []() {
                auto quantifier = []() {
                    if constexpr (repetitions_min == 0
                                  && repetitions_max
                                         == std::numeric_limits<unsigned>::max())
                    {
                        return static_string {"*+"};
                    }
                    else if constexpr (repetitions_min == 1
                                       && repetitions_max
                                              == std::numeric_limits<
                                                  unsigned>::max())
                    {
                        return static_string {"++"};
                    }
                    else if constexpr (repetitions_min == 0
                                       && repetitions_max == 1)
                    {
                        return static_string {"?+"};
                    }
                    else if constexpr (repetitions_max
                                       == std::numeric_limits<unsigned>::max())
                    {
                        return '{'
                               + number_to_pack_string_t<repetitions_min>::string
                               + ",}+";
                    }
                    else
                    {
                        return '{'
                               + number_to_pack_string_t<repetitions_min>::string
                               + ','
                               + number_to_pack_string_t<repetitions_max>::string
                               + "}+";
                    }
                }();

                auto self = matcher::expression;

                if constexpr (sizeof...(children) <= 1)
                {
                    return self + quantifier
                           + get_children_expression<children...>();
                }
                else
                {
                    return self + quantifier + "(?:"
                           + +get_children_expression<children...>()
                           + ')';
                }
            }();

            template<typename... injected_children>
            using optimize
                = possessive<typename matcher::template optimize<>,
                             repetitions_min,
                             repetitions_max,
                             typename children::template optimize<>...>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                for (unsigned i = 0; i < repetitions_max; ++i)
                {
                    matcher::template match<injected_children...>(result);

                    if (!result)
                    {
                        if (i < repetitions_min)
                        {
                            // Failed too early
                            return result;
                        }

                        // Failed but repetitions_min was satisfied,
                        // run dfs
                        result.accepted = true;
                        break;
                    }
                }

                return dfs<std::tuple<children...>>(result);
            }
    };

} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_POSSESSIVE_HPP_*/

namespace e_regex::nodes
{
    template<typename matcher,
             unsigned repetitions_min,
             unsigned repetitions_max = std::numeric_limits<unsigned>::max(),
             typename... children>
    struct greedy : public base<matcher, children...>
    {
            // If matcher is optional (aka repetitions_min==0),
            // admission set must include children too
            using admission_set = std::conditional_t<
                repetitions_min == 0,
                typename extract_admission_set<matcher, children...>::type,
                typename decltype(matcher::meta)::admission_set>;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = e_regex::policy::GREEDY,
                .minimum_match_size
                = matcher::meta.minimum_match_size * repetitions_min
                  + min(children::meta.minimum_match_size...),
                .maximum_match_size
                = repetitions_max == std::numeric_limits<unsigned>::max()
                      ? std::numeric_limits<unsigned>::max()
                      : matcher::meta.maximum_match_size * repetitions_max
                            + max(children::meta.maximum_match_size...),
            };

            static constexpr unsigned groups
                = group_getter<matcher>::value
                  + sum(group_getter<children>::value...);

            static constexpr auto expression = []() {
                auto quantifier = []() {
                    if constexpr (repetitions_min == 0
                                  && repetitions_max
                                         == std::numeric_limits<unsigned>::max())
                    {
                        return static_string {"*"};
                    }
                    else if constexpr (repetitions_min == 1
                                       && repetitions_max
                                              == std::numeric_limits<
                                                  unsigned>::max())
                    {
                        return static_string {"+"};
                    }
                    else if constexpr (repetitions_min == 0
                                       && repetitions_max == 1)
                    {
                        return static_string {"?"};
                    }
                    else if constexpr (repetitions_max
                                       == std::numeric_limits<unsigned>::max())
                    {
                        return static_string {"{"}
                               + number_to_pack_string_t<repetitions_min>::string
                               + static_string {",}"};
                    }
                    else
                    {
                        return static_string {"{"}
                               + number_to_pack_string_t<repetitions_min>::string
                               + ','
                               + number_to_pack_string_t<repetitions_max>::string
                               + '}';
                    }
                }();

                auto self = matcher::expression;

                if constexpr (sizeof...(children) <= 1)
                {
                    return self + quantifier
                           + get_children_expression<children...>();
                }
                else
                {
                    return self + quantifier + "(?:"
                           + get_children_expression<children...>()
                           + ')';
                }
            }();

            // If this node has no intersections with its children,
            // backtracking is not needed
            template<typename... injected_children>
            using optimize = std::conditional_t<
                admitted_sets_intersection_t<
                    admission_set,
                    typename extract_admission_set<children..., injected_children...>::type>::empty,
                possessive<typename matcher::template optimize<>,
                           repetitions_min,
                           repetitions_max,
                           typename children::template optimize<>...>,
                greedy<typename matcher::template optimize<>,
                       repetitions_min,
                       repetitions_max,
                       typename children::template optimize<>...>>;

            template<typename... injected_children>
            static constexpr auto __attribute__((always_inline))
            backtrack(auto& result) -> auto&
            {
                if constexpr (sizeof...(children) > 0)
                {
                    return dfs<std::tuple<children...>>(result);
                }
                else
                {
                    auto bak = result.actual_iterator_end;

                    dfs<std::tuple<injected_children...>>(result);
                    result.actual_iterator_end = bak;

                    return result;
                }
            }

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                if constexpr (std::is_same_v<matcher, void>)
                {
                    return dfs<std::tuple<children...>>(result);
                }
                else if constexpr (repetitions_min == 0
                                   && repetitions_max == 1
                                   && matcher::meta.minimum_match_size
                                          == matcher::meta.maximum_match_size)
                {
                    // Optional node with a terminal matcher
                    matcher::match(result);

                    if (!backtrack<injected_children...>(result)
                        && result.actual_iterator_end
                               > result.actual_iterator_start)
                    {
                        result.actual_iterator_end
                            -= matcher::meta.minimum_match_size;
                        backtrack<injected_children...>(result);
                    }

                    return result;
                }
                else
                {
                    const auto begin = result.actual_iterator_end;

                    if constexpr (repetitions_min > 0)
                    {
                        for (unsigned i = 0; i < repetitions_min; ++i)
                        {
                            if (!matcher::match(result))
                            {
                                return result;
                            }
                        }
                    }

                    // Iterate while this matcher accepts
                    auto       repetitions = repetitions_min;
                    const auto start = result.actual_iterator_end;

                    while (repetitions < repetitions_max)
                    {
                        if (!matcher::match(result))
                        {
                            // Restore last good match
                            result = true;
                            break;
                        }

                        repetitions++;
                    }

                    // Now backtrack
                    while (repetitions > repetitions_min)
                    {
                        if (backtrack<injected_children...>(result))
                        {
                            return result;
                        }

                        for (auto iter = result.actual_iterator_end - 1;
                             iter >= start;
                             --iter)
                        {
                            result.actual_iterator_end = iter;

                            if (matcher::match(result))
                            {
                                result.actual_iterator_end = iter;
                                break;
                            }
                        }

                        --repetitions;
                    }

                    result = repetitions >= repetitions_min;

                    if (result)
                    {
                        return dfs<std::tuple<children...>>(result);
                    }

                    result.actual_iterator_end = begin;
                    return result;
                }
            }
    };
} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_GREEDY_HPP_*/

#ifndef E_REGEX_NODES_GROUP_HPP_
#define E_REGEX_NODES_GROUP_HPP_

#include <algorithm>

namespace e_regex::nodes
{
    template<typename matcher, auto group_index, typename... children>
    struct group
    {
            static constexpr auto expression
                = '(' + matcher::expression + ')'
                  + get_children_expression<children...>();

            using admission_set =
                typename extract_admission_set<matcher>::type;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = e_regex::policy::NONE,
                .minimum_match_size = matcher::meta.minimum_match_size,
                .maximum_match_size = matcher::meta.maximum_match_size};

            static constexpr auto next_group_index = group_index + 1;

            template<typename... injected_children>
            using optimize
                = group<typename matcher::template optimize<children..., injected_children...>,
                        group_index,
                        typename children::template optimize<>...>;

            static constexpr unsigned groups
                = group_getter<matcher>::value
                  + sum(group_getter<children>::value...) + 1;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                auto begin = result.actual_iterator_end;

                // This node's children matter in backtracking
                matcher::template match<children...>(result);

                if (result)
                {
                    result.match_groups[group_index]
                        = literal_string_view {
                            begin, result.actual_iterator_end};

                    return dfs<std::tuple<children...>,
                               std::tuple<children..., injected_children...>>(
                        result);
                }

                return result;
            }
    };
} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_GROUP_HPP_*/

#ifndef E_REGEX_NODES_LAZY_HPP_
#define E_REGEX_NODES_LAZY_HPP_

#include <cstddef>
#include <limits>

namespace e_regex::nodes
{
    template<typename matcher,
             unsigned repetitions_min,
             unsigned repetitions_max = std::numeric_limits<unsigned>::max(),
             typename... children>
    struct lazy : public base<matcher, children...>
    {
            // If matcher is optional (aka repetitions_min==0),
            // admission set must include children too
            using admission_set = std::conditional_t<
                repetitions_min == 0,
                typename extract_admission_set<matcher, children...>::type,
                typename decltype(matcher::meta)::admission_set>;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = e_regex::policy::LAZY,
                .minimum_match_size
                = matcher::meta.minimum_match_size * repetitions_min
                  + min(children::meta.minimum_match_size...),
                .maximum_match_size
                = repetitions_max == std::numeric_limits<unsigned>::max()
                      ? std::numeric_limits<unsigned>::max()
                      : matcher::meta.maximum_match_size * repetitions_max
                            + max(children::meta.maximum_match_size...),
            };

            static constexpr auto expression = []() {
                auto quantifier = []() {
                    if constexpr (repetitions_min == 0
                                  && repetitions_max
                                         == std::numeric_limits<unsigned>::max())
                    {
                        return static_string {"*?"};
                    }
                    else if constexpr (repetitions_min == 1
                                       && repetitions_max
                                              == std::numeric_limits<
                                                  unsigned>::max())
                    {
                        return static_string {"+?"};
                    }
                    else if constexpr (repetitions_min == 0
                                       && repetitions_max == 1)
                    {
                        return static_string {"??"};
                    }
                    else if constexpr (repetitions_max
                                       == std::numeric_limits<unsigned>::max())
                    {
                        return '{'
                               + number_to_pack_string_t<repetitions_min>::string
                               + ",}?";
                    }
                    else
                    {
                        return "{"
                               + number_to_pack_string_t<repetitions_min>::string
                               + ","
                               + number_to_pack_string_t<repetitions_max>::string
                               + "}?";
                    }
                }();

                auto self = matcher::expression;

                if constexpr (sizeof...(children) <= 1)
                {
                    return self + quantifier
                           + get_children_expression<children...>();
                }
                else
                {
                    return self + quantifier + "(?:"
                           + get_children_expression<children...>()
                           + ')';
                }
            }();

            // If this node has no intersections with its children,
            // backtracking is not needed
            template<typename... injected_children>
            using optimize = std::conditional_t<
                admitted_sets_intersection_t<
                    admission_set,
                    typename extract_admission_set<children..., injected_children...>::type>::empty,
                possessive<typename matcher::template optimize<>,
                           repetitions_min,
                           repetitions_max,
                           typename children::template optimize<>...>,
                lazy<typename matcher::template optimize<>,
                     repetitions_min,
                     repetitions_max,
                     typename children::template optimize<>...>>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                auto start = result.actual_iterator_end;

                if constexpr (std::is_same_v<matcher, void>)
                {
                    return dfs<std::tuple<children...>>(result);
                }
                else
                {
                    if constexpr (repetitions_min != 0)
                    {
                        for (unsigned i = 0; i < repetitions_min; ++i)
                        {
                            if (!matcher::match(result))
                            {
                                return result;
                            }
                        }
                    }

                    unsigned matches = repetitions_min;

                    while (result.actual_iterator_end < result.query.end()
                           && matches < repetitions_max)
                    {
                        if constexpr (sizeof...(children) > 0)
                        {
                            dfs<std::tuple<children...>>(result);

                            if (result)
                            {
                                return result;
                            }
                        }
                        else
                        {
                            auto bak = result.actual_iterator_end;
                            dfs<std::tuple<injected_children...>>(result);
                            result.actual_iterator_end = bak;
                        }

                        if (!result)
                        {
                            matcher::match(result);
                            matches++;

                            if (!result)
                            {
                                result.actual_iterator_end = start;
                                return result;
                            }
                        }
                        else
                        {
                            break;
                        }
                    }

                    return dfs<std::tuple<children...>>(result);
                }
            }
    };
} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_LAZY_HPP_*/

#ifndef E_REGEX_NODES_NEGATED_HPP_
#define E_REGEX_NODES_NEGATED_HPP_

#include <utility>

namespace e_regex::nodes
{
    template<typename matcher>
    struct negated_node : public base<matcher>
    {
            static constexpr auto expression
                = "[^" + matcher::expression + "]";

            using admission_set = admitted_set_complement_t<
                typename extract_admission_set<matcher>::type>;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = policy::NONE,
                .minimum_match_size = matcher::meta.minimum_match_size,
                .maximum_match_size = matcher::meta.minimum_match_size,
            };

            template<typename... injected_children>
            using optimize
                = negated_node<typename matcher::template optimize<>>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& result) -> auto&
            {
                if (result.actual_iterator_end >= result.query.end())
                {
                    result.accepted = false;
                    return result;
                }

                matcher::match(result);
                result.accepted = !result.accepted;

                if (result)
                {
                    result.actual_iterator_end++;
                }

                return result;
            }
    };

    // TODO missing get_expression
} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_NEGATED_HPP_*/

#ifndef E_REGEX_NODES_REPEATED_HPP_
#define E_REGEX_NODES_REPEATED_HPP_

#include <cstddef>

namespace e_regex::nodes
{
    template<typename matcher, unsigned repetitions, typename... children>
    struct repeated : public base<matcher, children...>
    {
            using admission_set =
                typename extract_admission_set<matcher>::type;

            static constexpr auto meta = e_regex::meta<admission_set> {
                .policy_ = e_regex::policy::NONE,
                .minimum_match_size
                = matcher::meta.minimum_match_size * repetitions,
                .maximum_match_size
                = matcher::meta.maximum_match_size * repetitions,
            };

            static constexpr auto expression = []() {
                auto self = matcher::expression + '{'
                            + number_to_pack_string_t<repetitions>::string
                            + '}';

                if constexpr (sizeof...(children) <= 1)
                {
                    return self + get_children_expression<children...>();
                }
                else
                {
                    return self + "(?:"
                           + get_children_expression<children...>()
                           + ')';
                }
            }();

            template<typename... injected_children>
            using optimize
                = repeated<typename matcher::template optimize<>,
                           repetitions,
                           typename children::template optimize<>...>;

            template<typename... injected_children>
            static constexpr __attribute__((always_inline)) auto
                match(auto& res) -> auto&
            {
                for (unsigned i = 0; i < repetitions; ++i)
                {
                    if (!matcher::match(res))
                    {
                        return res;
                    }
                }

                return dfs<std::tuple<children...>>(res);
            }
    };
} // namespace e_regex::nodes

#endif /* E_REGEX_NODES_REPEATED_HPP_*/

#endif /* NODES_HPP */

#ifndef OPERATORS_HPP
#define OPERATORS_HPP

#ifndef E_REGEX_OPERATORS_BRACES_HPP_
#define E_REGEX_OPERATORS_BRACES_HPP_

#include <limits>

#ifndef E_REGEX_OPERATORS_COMMON_HPP_
#define E_REGEX_OPERATORS_COMMON_HPP_

#include <tuple>

#ifndef HEURISTICS_COMMON_HPP
#define HEURISTICS_COMMON_HPP

#include <tuple>

namespace e_regex
{
    template<typename node, typename child>
    struct add_child;

    template<template<typename, typename...> typename matcher, typename match, typename... children, typename child>
    struct add_child<matcher<match, children...>, child>
    {
            using type = matcher<match, children..., child>;
    };

    template<template<typename, auto, auto, typename...> typename quantified_matcher,
             auto min,
             auto max,
             typename match,
             typename... children,
             typename child>
    struct add_child<quantified_matcher<match, min, max, children...>, child>
    {
            using type = quantified_matcher<match, min, max, children..., child>;
    };

    template<template<typename, auto, typename...> typename quantified_matcher,
             auto data,
             typename match,
             typename... children,
             typename child>
    struct add_child<quantified_matcher<match, data, children...>, child>
    {
            using type = quantified_matcher<match, data, children..., child>;
    };

    template<typename child>
    struct add_child<void, child>
    {
            using type = child;
    };

    template<typename node, typename child>
    using add_child_t = typename add_child<node, child>::type;
}// namespace e_regex

#endif /* HEURISTICS_COMMON_HPP */

namespace e_regex
{
    template<typename last_node, typename tokens, auto group_index>
    struct tree_builder_helper;

    template<typename last_node, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<>, group_index>
    {
            // Base case

            using tree = last_node;
    };

    template<typename last_node, typename head, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<head, tail...>, group_index>
    {
            // Simple case, iterate
            using new_node = typename tree_builder_helper<
                nodes::simple<terminals::terminal<head>>,
                std::tuple<tail...>,
                group_index>::tree;

            using tree = add_child_t<last_node, new_node>;
    };
} // namespace e_regex

#endif /* E_REGEX_OPERATORS_COMMON_HPP_*/

#ifndef UTILITIES_EXTRACT_DELIMITED_CONTENT
#define UTILITIES_EXTRACT_DELIMITED_CONTENT

#include <tuple>

#ifndef UTILITIES_TUPLE_CAT
#define UTILITIES_TUPLE_CAT

#include <tuple>

namespace e_regex
{
    template<typename T1, typename T2>
    struct tuple_cat;

    template<typename... T1, typename... T2>
    struct tuple_cat<std::tuple<T1...>, std::tuple<T2...>>
    {
            using type = std::tuple<T1..., T2...>;
    };

    template<typename... T1, typename T2>
    struct tuple_cat<std::tuple<T1...>, T2>
    {
            using type = std::tuple<T1..., T2>;
    };

    template<typename T1, typename T2>
    using tuple_cat_t = typename tuple_cat<T1, T2>::type;
}// namespace e_regex

#endif /* UTILITIES_TUPLE_CAT */

namespace e_regex
{
    template<char open, char closing, unsigned skip_counter, typename current, typename string>
    struct extract_delimited_content;

    template<char open, char closing, typename current, typename... tail_>
    struct extract_delimited_content<open, closing, 0, current, std::tuple<pack_string<closing>, tail_...>>
    {
            // Base case, closing delimiter found
            using result    = current;
            using remaining = std::tuple<tail_...>;
    };

    template<char open, char closing, typename current>
    struct extract_delimited_content<open, closing, 0, current, std::tuple<>>
    {
            // Base case, malformed token string
            using result    = std::tuple<>;
            using remaining = result;
    };

    template<char open, char closing, unsigned skip_counter, typename current, typename... tail>
    struct extract_delimited_content<open, closing, skip_counter, current, std::tuple<pack_string<closing>, tail...>>
        : public extract_delimited_content<open,
                                           closing,
                                           skip_counter - 1,
                                           tuple_cat_t<current, pack_string<closing>>,
                                           std::tuple<tail...>>
    {
            // Closing delimiter found, but it has to be skipped
    };

    template<char open, char closing, unsigned skip_counter, typename current, typename... tail>
    struct extract_delimited_content<open, closing, skip_counter, current, std::tuple<pack_string<open>, tail...>>
        : public extract_delimited_content<open,
                                           closing,
                                           skip_counter + 1,
                                           tuple_cat_t<current, pack_string<open>>,
                                           std::tuple<tail...>>
    {
            // Open delimiter found, increase skip counter
    };

    template<char open, char closing, unsigned skip_counter, typename current, typename head, typename... tail>
    struct extract_delimited_content<open, closing, skip_counter, current, std::tuple<head, tail...>>
        : public extract_delimited_content<open, closing, skip_counter, tuple_cat_t<current, head>, std::tuple<tail...>>
    {
            // Iterate characters
    };

    template<char open, char closing, typename string>
    using extract_delimited_content_t
        = extract_delimited_content<open, closing, 0, std::tuple<>, string>;
}// namespace e_regex

#endif /* UTILITIES_EXTRACT_DELIMITED_CONTENT */

#ifndef E_REGEX_UTILITIES_PACK_STRING_TO_NUMBER_HPP_
#define E_REGEX_UTILITIES_PACK_STRING_TO_NUMBER_HPP_

namespace e_regex
{
    template<typename string, unsigned index = string::size - 1>
    struct pack_string_to_number;

    template<char head, char... tail, unsigned index>
    struct pack_string_to_number<pack_string<head, tail...>, index>
    {
            static consteval auto ten_power(unsigned exp) -> unsigned
            {
                unsigned res = 1;

                while (exp-- != 0)
                {
                    res *= 10;
                }

                return res;
            }

            static inline const constinit unsigned value
                = ((head - '0') * ten_power(index))
                  + pack_string_to_number<pack_string<tail...>, index - 1>::value;
    };

    template<unsigned index>
    struct pack_string_to_number<pack_string<>, index>
    {
            static constexpr unsigned value = 0;
    };
} // namespace e_regex

#endif /* E_REGEX_UTILITIES_PACK_STRING_TO_NUMBER_HPP_*/

namespace e_regex
{
    template<typename matcher, typename data, typename policy_>
    struct quantified_node_builder;

    template<typename matcher, typename first, typename policy>
    struct quantified_node_builder<matcher, std::tuple<first>, policy>
    {
            // Exact quantifier e.g. a{3}

            using type
                = nodes::repeated<matcher, pack_string_to_number<first>::value>;
    };

    // {n,} quantifiers
    template<typename matcher, typename first>
    struct quantified_node_builder<matcher,
                                   std::tuple<first, pack_string<','>>,
                                   pack_string<'?'>>
    {
            using type
                = nodes::lazy<matcher, pack_string_to_number<first>::value>;
    };

    template<typename matcher, typename first>
    struct quantified_node_builder<matcher,
                                   std::tuple<first, pack_string<','>>,
                                   pack_string<'+'>>
    {
            using type
                = nodes::possessive<matcher, pack_string_to_number<first>::value>;
    };

    template<typename matcher, typename first, typename policy>
    struct quantified_node_builder<matcher, std::tuple<first, pack_string<','>>, policy>
    {
            using type
                = nodes::greedy<matcher, pack_string_to_number<first>::value>;
    };

    // {n, n} quantifiers
    template<typename matcher, typename first, typename second>
    struct quantified_node_builder<matcher,
                                   std::tuple<first, pack_string<','>, second>,
                                   pack_string<'?'>>
    {
            using type
                = nodes::lazy<matcher,
                              pack_string_to_number<first>::value,
                              pack_string_to_number<second>::value>;
    };

    template<typename matcher, typename first, typename second>
    struct quantified_node_builder<matcher,
                                   std::tuple<first, pack_string<','>, second>,
                                   pack_string<'+'>>
    {
            using type
                = nodes::possessive<matcher,
                                    pack_string_to_number<first>::value,
                                    pack_string_to_number<second>::value>;
    };

    template<typename matcher, typename first, typename second, typename policy>
    struct quantified_node_builder<matcher,
                                   std::tuple<first, pack_string<','>, second>,
                                   policy>
    {
            using type
                = nodes::greedy<matcher,
                                pack_string_to_number<first>::value,
                                pack_string_to_number<second>::value>;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'{'>, tail...>, group_index>
    {
            // { found
            using substring
                = extract_delimited_content_t<'{', '}', std::tuple<tail...>>;

            // Get first character after }, can be + or ? for
            // specifying the policy
            using remaining_head
                = first_type<typename substring::remaining>;
            using policy = typename remaining_head::type;

            // If a policy is specified, remaining is everything after
            // remaining_head, otherwise it is everything after }
            using remaining = std::conditional_t<
                std::is_same_v<typename remaining_head::type, pack_string<'+'>>
                    || std::is_same_v<typename remaining_head::type, pack_string<'?'>>,
                typename remaining_head::remaining,
                typename substring::remaining>;

            using new_node =
                typename quantified_node_builder<last_node,
                                                 typename substring::result,
                                                 policy>::type;

            using tree =
                typename tree_builder_helper<new_node, remaining, group_index>::tree;
    };
} // namespace e_regex

#endif /* E_REGEX_OPERATORS_BRACES_HPP_*/

#ifndef E_REGEX_OPERATORS_END_ANCHOR_HPP_
#define E_REGEX_OPERATORS_END_ANCHOR_HPP_

#include <tuple>

namespace e_regex
{
    template<typename last_node, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'$'>>, group_index>
    {
            // End anchor found

            using new_node = nodes::simple<terminals::anchors::end>;

            using tree = add_child_t<last_node, new_node>;
    };
} // namespace e_regex

#endif /* E_REGEX_OPERATORS_END_ANCHOR_HPP_*/

#ifndef E_REGEX_OPERATORS_HEX_HPP_
#define E_REGEX_OPERATORS_HEX_HPP_

namespace e_regex
{
    template<char C>
    concept hex = C >= '0' && C <= 'F';

    template<char... digits>
    struct hex_to_bin;

    template<char first, char second, char... tail>
    struct hex_to_bin<first, second, tail...>
    {
            static_assert(hex<first> && hex<second>,
                          "Hex characters must be included between 0 and F");

            static constexpr char current
                = ((first - '0') << 4) | (second - '0');

            using result
                = merge_pack_strings_t<pack_string<current>,
                                       typename hex_to_bin<tail...>::result>;
    };

    template<char first, char second, char third>
    struct hex_to_bin<first, second, third>
    {
            static_assert(hex<first> && hex<second> && hex<third>,
                          "Hex characters must be included between 0 and F");

            static constexpr char current
                = ((first - '0') << (4)) | (second - '0');
            static constexpr char current_second = (third - '0') << 4;

            using result = pack_string<current>;
    };

    template<char first>
    struct hex_to_bin<first>
    {
            static_assert(hex<first>,
                          "Hex characters must be included between 0 and F");

            static constexpr char current = first - '0';

            using result = pack_string<current>;
    };

    template<>
    struct hex_to_bin<>
    {
            using result = pack_string<>;
    };

    template<typename digits>
    struct hex_tuple_to_bin;

    template<char... digits>
    struct hex_tuple_to_bin<std::tuple<pack_string<digits...>>>
    {
            using result = typename hex_to_bin<digits...>::result;
    };

    template<typename last_node, char first_nibble, char second_nibble, typename... tail, auto group_index>
        requires hex<first_nibble> && hex<second_nibble>
    struct tree_builder_helper<last_node,
                               std::tuple<pack_string<'\\', 'x'>,
                                          pack_string<first_nibble>,
                                          pack_string<second_nibble>,
                                          tail...>,
                               group_index>
    {
            using value =
                typename hex_to_bin<first_nibble, second_nibble>::result;

            using new_node = typename tree_builder_helper<
                nodes::simple<terminals::exact_matcher<value>>,
                std::tuple<tail...>,
                group_index>::tree;
            using tree = add_child_t<last_node, new_node>;
    };

    template<typename last_node, char nibble, typename... tail, auto group_index>
        requires hex<nibble>
    struct tree_builder_helper<
        last_node,
        std::tuple<pack_string<'\\', 'x'>, pack_string<nibble>, tail...>,
        group_index>
    {
            using value = typename hex_to_bin<nibble>::result;

            using new_node = typename tree_builder_helper<
                nodes::simple<terminals::exact_matcher<value>>,
                std::tuple<tail...>,
                group_index>::tree;
            using tree = add_child_t<last_node, new_node>;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<
        last_node,
        std::tuple<pack_string<'\\', 'x'>, pack_string<'{'>, tail...>,
        group_index>
    {
            using substring
                = extract_delimited_content_t<'{', '}', std::tuple<tail...>>;

            using value =
                typename hex_tuple_to_bin<typename substring::result>::result;

            using new_node = typename tree_builder_helper<
                nodes::simple<terminals::exact_matcher<value>>,
                typename substring::remaining,
                group_index>::tree;

            using tree = add_child_t<last_node, new_node>;
    };
} // namespace e_regex

#endif /* E_REGEX_OPERATORS_HEX_HPP_*/

#ifndef E_REGEX_OPERATORS_OCTAL_HPP_
#define E_REGEX_OPERATORS_OCTAL_HPP_

namespace e_regex
{
    template<char C>
    concept octal = C >= '0' && C <= '8';

    template<char... digits>
    struct octal_to_bin;

    template<char first, char second, char third, char... tail>
    struct octal_to_bin<first, second, third, tail...>
    {
            static_assert(octal<first> && octal<second> && octal<third>,
                          "Octal characters must be included between 0 and 8");

            static constexpr char current = ((first - '0') << 6)
                                            | ((second - '0') << 3)
                                            | (third - '0');

            using result
                = merge_pack_strings_t<pack_string<current>,
                                       typename octal_to_bin<tail...>::result>;
    };

    template<>
    struct octal_to_bin<>
    {
            using result = pack_string<>;
    };

    template<typename digits>
    struct octal_tuple_to_bin;

    template<char... digits>
    struct octal_tuple_to_bin<std::tuple<pack_string<digits...>>>
    {
            using result = typename octal_to_bin<digits...>::result;
    };

    template<typename last_node,
             char first_nibble,
             char second_nibble,
             char third_nibble,
             typename... tail,
             auto group_index>
        requires octal<first_nibble> && octal<second_nibble>
                 && octal<third_nibble>
    struct tree_builder_helper<last_node,
                               std::tuple<pack_string<'\\', first_nibble>,
                                          pack_string<second_nibble>,
                                          pack_string<third_nibble>,
                                          tail...>,
                               group_index>
    {
            using value =
                typename octal_to_bin<first_nibble, second_nibble, third_nibble>::result;

            using new_node = typename tree_builder_helper<
                nodes::simple<terminals::exact_matcher<value>>,
                std::tuple<tail...>,
                group_index>::tree;
            using tree = add_child_t<last_node, new_node>;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<
        last_node,
        std::tuple<pack_string<'\\', 'o'>, pack_string<'{'>, tail...>,
        group_index>
    {
            using substring
                = extract_delimited_content_t<'{', '}', std::tuple<tail...>>;

            using value =
                typename octal_tuple_to_bin<typename substring::result>::result;

            using new_node = typename tree_builder_helper<
                nodes::simple<terminals::exact_matcher<value>>,
                typename substring::remaining,
                group_index>::tree;

            using tree = add_child_t<last_node, new_node>;
    };
} // namespace e_regex

#endif /* E_REGEX_OPERATORS_OCTAL_HPP_*/

#ifndef OPERATORS_OPTIONAL
#define OPERATORS_OPTIONAL

namespace e_regex
{
    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'?'>, tail...>, group_index>
    {
            // greedy ? operator found
            using new_node = nodes::greedy<last_node, 0, 1>;

            using tree = typename tree_builder_helper<new_node, std::tuple<tail...>, group_index>::tree;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'?'>, pack_string<'?'>, tail...>, group_index>
    {
            // lazy ? operator found
            using new_node = nodes::lazy<last_node, 0, 1>;

            using tree = typename tree_builder_helper<new_node, std::tuple<tail...>, group_index>::tree;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'?'>, pack_string<'+'>, tail...>, group_index>
    {
            // possessive ? operator found
            using new_node = nodes::possessive<last_node, 0, 1>;

            using tree = typename tree_builder_helper<new_node, std::tuple<tail...>, group_index>::tree;
    };
}// namespace e_regex

#endif /* OPERATORS_OPTIONAL */

#ifndef OPERATORS_PLUS
#define OPERATORS_PLUS

#include <limits>

namespace e_regex
{
    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'+'>, tail...>, group_index>
    {
            // greedy + operator found
            using new_node = nodes::greedy<last_node, 1>;

            using tree = typename tree_builder_helper<new_node, std::tuple<tail...>, group_index>::tree;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'+'>, pack_string<'?'>, tail...>, group_index>
    {
            // lazy + operator found
            using new_node = nodes::lazy<last_node, 1>;

            using tree = typename tree_builder_helper<new_node, std::tuple<tail...>, group_index>::tree;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'+'>, pack_string<'+'>, tail...>, group_index>
    {
            // lazy + operator found
            using new_node = nodes::possessive<last_node, 1>;

            using tree = typename tree_builder_helper<new_node, std::tuple<tail...>, group_index>::tree;
    };
}// namespace e_regex

#endif /* OPERATORS_PLUS */

#ifndef E_REGEX_OPERATORS_ROUND_BRACKETS_HPP_
#define E_REGEX_OPERATORS_ROUND_BRACKETS_HPP_

#ifndef E_REGEX_UTILITIES_SPLIT_HPP_
#define E_REGEX_UTILITIES_SPLIT_HPP_

#include <tuple>

#ifndef E_REGEX_UTILITIES_BALANCED_HPP_
#define E_REGEX_UTILITIES_BALANCED_HPP_

#ifndef UTILITIES_STATIC_STACK
#define UTILITIES_STATIC_STACK

#include <type_traits>
namespace e_regex
{
    template<typename T, T... data>
    struct static_stack
    {
            template<T d>
            using push = static_stack<T, d, data...>;

            static constexpr bool empty = true;
    };

    template<typename T, T last, T... data>
    struct static_stack<T, last, data...>
    {
            template<T d>
            using push = static_stack<T, d, last, data...>;

            using pop = static_stack<T, data...>;

            static constexpr bool        empty = false;
            static constexpr const auto &head  = last;
    };

    template<typename Data, typename T>
    struct is_static_stack_ : public std::false_type
    {
    };

    template<typename T, T... data>
    struct is_static_stack_<T, static_stack<T, data...>> : public std::true_type
    {
    };

    template<typename T, typename Data>
    concept is_static_stack = is_static_stack_<Data, T>::value;

}// namespace e_regex

#endif

namespace e_regex
{
    template<is_static_stack<char> stack, typename... strings>
    struct balanced : public std::false_type
    {
            // Base negative case, did not match any positive case
    };

    template<typename stack>
        requires(stack::empty)
    struct balanced<stack> : public std::true_type
    {
            // Base positive case, empty stack and no more tokens
    };

    template<typename stack, typename string, typename... strings>
    struct balanced<stack, string, strings...>
        : public balanced<stack, strings...>
    {
            // This token is not a parentheses
    };

    template<typename stack, typename... strings>
    struct balanced<stack, pack_string<'('>, strings...>
        : public balanced<typename stack::template push<'('>, strings...>
    {
            // ( found
    };

    template<typename stack, typename... strings>
        requires(!stack::empty && stack::head == '(')
    struct balanced<stack, pack_string<')'>, strings...>
        : public balanced<typename stack::pop, strings...>
    {
            // ) found and a ( is on top of the stack
    };

    template<typename stack, typename... strings>
    struct balanced<stack, pack_string<'['>, strings...>
        : public balanced<typename stack::template push<'['>, strings...>
    {
            // [ found
    };

    template<typename stack, typename... strings>
        requires(!stack::empty && stack::head == '[')
    struct balanced<stack, pack_string<']'>, strings...>
        : public balanced<typename stack::pop, strings...>
    {
            // ] found and a [ is on top of the stack
    };

    template<typename stack, typename... strings>
    struct balanced<stack, pack_string<'{'>, strings...>
        : public balanced<typename stack::template push<'{'>, strings...>
    {
            // { found
    };

    template<typename stack, typename... strings>
        requires(!stack::empty && stack::head == '{')
    struct balanced<stack, pack_string<'}'>, strings...>
        : public balanced<typename stack::pop, strings...>
    {
            // } found and a { is on top of the stack
    };

    template<typename... strings>
    static constexpr auto balanced_v
        = balanced<static_stack<char>, strings...>::value;

} // namespace e_regex

#endif /* E_REGEX_UTILITIES_BALANCED_HPP_*/

#ifndef E_REGEX_UTILITIES_CONCEPTS_HPP_
#define E_REGEX_UTILITIES_CONCEPTS_HPP_

#include <tuple>
#include <type_traits>

namespace e_regex
{
    namespace __private
    {
        template<typename T>
        struct is_tuple : public std::false_type
        {
        };

        template<typename... T>
        struct is_tuple<std::tuple<T...>> : public std::true_type
        {
        };
    }// namespace __private

    template<typename T>
    concept tuple = __private::is_tuple<T>::value;
}// namespace e_regex

#endif /* E_REGEX_UTILITIES_CONCEPTS_HPP_*/

#ifndef E_REGEX_UTILITIES_REVERSE_HPP_
#define E_REGEX_UTILITIES_REVERSE_HPP_

#include <tuple>

namespace e_regex
{
    template<typename T, tuple current = std::tuple<>>
    struct reverse;

    template<typename T, typename... tail, typename... currents>
    struct reverse<std::tuple<T, tail...>, std::tuple<currents...>>
    {
            using type =
                typename reverse<std::tuple<tail...>,
                                 std::tuple<T, currents...>>::type;
    };

    template<typename... currents>
    struct reverse<std::tuple<>, std::tuple<currents...>>
    {
            using type = std::tuple<currents...>;
    };

    template<tuple tuple>
    using reverse_t = typename reverse<tuple>::type;
} // namespace e_regex

#endif /* E_REGEX_UTILITIES_REVERSE_HPP_*/

namespace e_regex
{
    /* Split a token tuple by the given separator token, parentheses
     * will remain balanced even after splitting.
     */
    template<char separator, tuple tokens, tuple current = std::tuple<std::tuple<>>>
    struct split;

    template<char separator, typename... tail, typename... current_tokens, typename... currents>
        requires balanced_v<tail...> && balanced_v<current_tokens...>
    struct split<separator,
                 std::tuple<pack_string<separator>, tail...>,
                 std::tuple<std::tuple<current_tokens...>, currents...>>
    {
            // Separator found
            using current
                = std::tuple<std::tuple<>, std::tuple<current_tokens...>, currents...>;
            using type =
                typename split<separator, std::tuple<tail...>, current>::type;
    };

    template<char separator, typename head, typename... tail, typename... current_tokens, typename... currents>
    struct split<separator,
                 std::tuple<head, tail...>,
                 std::tuple<std::tuple<current_tokens...>, currents...>>
    {
            using current
                = std::tuple<std::tuple<current_tokens..., head>, currents...>;
            using type =
                typename split<separator, std::tuple<tail...>, current>::type;
    };

    template<char separator, typename current>
    struct split<separator, std::tuple<>, current>
    {
            using type = reverse_t<current>;
    };

    template<char separator, tuple tokens>
    using split_t = typename split<separator, tokens>::type;
} // namespace e_regex

#endif /* E_REGEX_UTILITIES_SPLIT_HPP_*/

namespace e_regex
{
    template<typename parsed, typename branches, auto group_index>
    struct branched;

    template<typename... parsed, typename subregex, typename... subregexes, auto group_index>
    struct branched<std::tuple<parsed...>, std::tuple<subregex, subregexes...>, group_index>
    {
            using subtree =
                typename tree_builder_helper<void, subregex, group_index>::tree;

            using tree =
                typename branched<std::tuple<parsed..., subtree>,
                                  std::tuple<subregexes...>,
                                  group_index>::tree;
    };

    template<typename... parsed, auto group_index>
    struct branched<std::tuple<parsed...>, std::tuple<>, group_index>
    {
            using tree = nodes::simple<void, parsed...>;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'('>, tail...>, group_index>
    {
            // ( found
            using substring
                = extract_delimited_content_t<'(', ')', std::tuple<tail...>>;

            using subregex =
                typename branched<std::tuple<>,
                                  split_t<'|', typename substring::result>,
                                  group_index + 1>::tree;

            using node = nodes::group<subregex, group_index>;

            using new_node =
                typename tree_builder_helper<node,
                                             typename substring::remaining,
                                             node::next_group_index>::tree;

            using tree = add_child_t<last_node, new_node>;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<
        last_node,
        std::tuple<pack_string<'('>, pack_string<'?'>, pack_string<':'>, tail...>,
        group_index>
    {
            // Non capturing group found
            using substring
                = extract_delimited_content_t<'(', ')', std::tuple<tail...>>;

            using subregex = nodes::simple<
                typename branched<std::tuple<>,
                                  split_t<'|', typename substring::result>,
                                  group_index>::tree>;

            using new_node =
                typename tree_builder_helper<subregex,
                                             typename substring::remaining,
                                             subregex::next_group_index>::tree;

            using tree = add_child_t<last_node, new_node>;
    };
} // namespace e_regex

#endif /* E_REGEX_OPERATORS_ROUND_BRACKETS_HPP_*/

#ifndef E_REGEX_OPERATORS_SQUARE_BRACKETS_HPP_
#define E_REGEX_OPERATORS_SQUARE_BRACKETS_HPP_

namespace e_regex
{
    namespace __private
    {
        template<typename node, typename child>
        struct add_branch;

        template<typename identifier>
        struct add_branch<nodes::simple<void>, terminals::exact_matcher<identifier>>
        {
                using type = terminals::exact_matcher<identifier>;
        };

        template<template<typename, typename...> typename matcher,
                 typename... children,
                 typename match,
                 typename child>
        struct add_branch<matcher<match, children...>, child>
        {
                using type = matcher<match, children..., child>;
        };

        template<typename node, typename child>
        using add_branch_t = typename add_branch<node, child>::type;
    } // namespace __private

    template<typename last_node, typename tokens, auto group_index>
    struct square_bracker_tree_builder_helper;

    template<typename last_node, auto group_index>
    struct square_bracker_tree_builder_helper<last_node, std::tuple<>, group_index>
    {
            // Base case

            using tree = last_node;
    };

    template<typename last_node, typename head, typename... tail, auto group_index>
    struct square_bracker_tree_builder_helper<last_node, std::tuple<head, tail...>, group_index>
    {
            // Simple case, iterate

            using new_node = __private::add_branch_t<
                last_node,
                nodes::simple<terminals::exact_matcher<head>>>;
            using tree =
                typename square_bracker_tree_builder_helper<new_node,
                                                            std::tuple<tail...>,
                                                            group_index>::tree;
    };

    template<typename last_node, char identifier, typename... tail, auto group_index>
    struct square_bracker_tree_builder_helper<
        last_node,
        std::tuple<pack_string<'\\', identifier>, tail...>,
        group_index>
    {
            // Simple case, iterate

            using new_node = __private::add_branch_t<
                last_node,
                nodes::simple<terminals::terminal<pack_string<'\\', identifier>>>>;
            using tree =
                typename square_bracker_tree_builder_helper<new_node,
                                                            std::tuple<tail...>,
                                                            group_index>::tree;
    };

    template<typename last_node, typename start, typename end, typename... tail, auto group_index>
    struct square_bracker_tree_builder_helper<
        last_node,
        std::tuple<start, pack_string<'-'>, end, tail...>,
        group_index>
    {
            // Range found

            using new_node = __private::add_branch_t<
                last_node,
                nodes::simple<terminals::range_terminal<start, end>>>;
            using tree =
                typename square_bracker_tree_builder_helper<new_node,
                                                            std::tuple<tail...>,
                                                            group_index>::tree;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'['>, tail...>, group_index>
    {
            // [ found
            using substring
                = extract_delimited_content_t<'[', ']', std::tuple<tail...>>;

            using subregex
                = nodes::simple<typename square_bracker_tree_builder_helper<
                    nodes::simple<void>,
                    typename substring::result,
                    group_index>::tree>;

            using new_node =
                typename tree_builder_helper<subregex,
                                             typename substring::remaining,
                                             subregex::next_group_index>::tree;

            using tree = add_child_t<last_node, new_node>;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<
        last_node,
        std::tuple<pack_string<'['>, pack_string<'^'>, tail...>,
        group_index>
    {
            // [ found
            using substring
                = extract_delimited_content_t<'[', ']', std::tuple<tail...>>;

            using subregex = typename square_bracker_tree_builder_helper<
                nodes::simple<void>,
                typename substring::result,
                group_index>::tree;

            // if subregex_group_index is 0 the subregex does not
            // contain any group

            using new_node = typename tree_builder_helper<
                nodes::simple<nodes::negated_node<subregex>>,
                typename substring::remaining,
                subregex::next_group_index>::tree;

            using tree = add_child_t<last_node, new_node>;
    };

} // namespace e_regex

#endif /* E_REGEX_OPERATORS_SQUARE_BRACKETS_HPP_*/

#ifndef E_REGEX_OPERATORS_STAR_HPP_
#define E_REGEX_OPERATORS_STAR_HPP_

namespace e_regex
{
    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<last_node, std::tuple<pack_string<'*'>, tail...>, group_index>
    {
            // greedy * operator found
            using new_node = nodes::greedy<last_node, 0>;
            using tree =
                typename tree_builder_helper<new_node,
                                             std::tuple<tail...>,
                                             group_index>::tree;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<
        last_node,
        std::tuple<pack_string<'*'>, pack_string<'?'>, tail...>,
        group_index>
    {
            // lazy * operator found
            using new_node = nodes::lazy<last_node, 0>;

            using tree =
                typename tree_builder_helper<new_node,
                                             std::tuple<tail...>,
                                             group_index>::tree;
    };

    template<typename last_node, typename... tail, auto group_index>
    struct tree_builder_helper<
        last_node,
        std::tuple<pack_string<'*'>, pack_string<'+'>, tail...>,
        group_index>
    {
            // possessive * operator found
            using new_node = nodes::possessive<last_node, 0>;

            using tree =
                typename tree_builder_helper<new_node,
                                             std::tuple<tail...>,
                                             group_index>::tree;
    };
} // namespace e_regex

#endif /* E_REGEX_OPERATORS_STAR_HPP_*/

#ifndef OPERATORS_START_ANCHOR
#define OPERATORS_START_ANCHOR

namespace e_regex
{
    template<typename... tail, auto group_index>
    struct tree_builder_helper<void, std::tuple<pack_string<'^'>, tail...>, group_index>
    {
            // Start anchor found

            using tree = typename tree_builder_helper<nodes::simple<terminals::anchors::start>,
                                                      std::tuple<tail...>,
                                                      group_index>::tree;
    };
}// namespace e_regex

#endif /* OPERATORS_START_ANCHOR */

#endif /* OPERATORS_HPP */

#ifndef E_REGEX_TOKENIZER_HPP_
#define E_REGEX_TOKENIZER_HPP_

#include <tuple>

namespace e_regex
{
    template<typename matches, char... string>
    struct extract_braces_numbers;

    template<char... current, char... tail>
    struct extract_braces_numbers<std::tuple<pack_string<current...>>, ',', tail...>
        : public extract_braces_numbers<
              tuple_cat_t<std::tuple<pack_string<current...>>,
                          std::tuple<pack_string<','>>>,
              tail...>
    {
            // ',' found, first number done
    };

    template<char... current, char head, char... tail>
    struct extract_braces_numbers<std::tuple<pack_string<current...>>, head, tail...>
        : public extract_braces_numbers<std::tuple<pack_string<current..., head>>,
                                        tail...>
    {
            // Populating first number
    };

    // Probably gcc bug -> ambiguous template instantiation if compact
    template<typename first, typename second, char... current, char... tail>
    struct extract_braces_numbers<std::tuple<first, second, pack_string<current...>>,
                                  '}',
                                  tail...>
    {
            // '}' found, finishing
            using numbers   = std::tuple<pack_string<'{'>,
                                       first,
                                       second,
                                       pack_string<current...>,
                                       pack_string<'}'>>;
            using remaining = pack_string<tail...>;
    };

    template<typename first, typename second, char... tail>
    struct extract_braces_numbers<std::tuple<first, second>, '}', tail...>
    {
            // '}' found, finishing
            using numbers
                = std::tuple<pack_string<'{'>, first, second, pack_string<'}'>>;
            using remaining = pack_string<tail...>;
    };

    template<char... current, char... tail>
    struct extract_braces_numbers<std::tuple<pack_string<current...>>, '}', tail...>
    {
            // '}' found, finishing
            using numbers   = std::tuple<pack_string<'{'>,
                                       pack_string<current...>,
                                       pack_string<'}'>>;
            using remaining = pack_string<tail...>;
    };

    template<typename first, typename second, char head, char... tail>
    struct extract_braces_numbers<std::tuple<first, second>, head, tail...>
        : public extract_braces_numbers<std::tuple<first, second, pack_string<head>>,
                                        tail...>
    {
            // Populating second number
    };

    template<char... current, typename first, typename second, char head, char... tail>
    struct extract_braces_numbers<std::tuple<first, second, pack_string<current...>>,
                                  head,
                                  tail...>
        : public extract_braces_numbers<
              std::tuple<first, second, pack_string<current..., head>>,
              tail...>
    {
            // Populating second number
    };

    template<char... string>
    using extract_braces_numbers_t
        = extract_braces_numbers<std::tuple<pack_string<>>, string...>;

    template<typename current, typename string>
    struct tokenizer;

    template<typename current, char head, char... tail>
    struct tokenizer<current, pack_string<head, tail...>>
    {
            // Simple iteration
            using current_
                = tuple_cat_t<current, std::tuple<pack_string<head>>>;
            using tokens =
                typename tokenizer<current_, pack_string<tail...>>::tokens;
    };

    template<typename current, char head, char... tail>
    struct tokenizer<current, pack_string<'\\', head, tail...>>
    {
            // Escaped character
            using current_
                = tuple_cat_t<current, std::tuple<pack_string<'\\', head>>>;
            using tokens =
                typename tokenizer<current_, pack_string<tail...>>::tokens;
    };

    template<typename current, char... tail>
    struct tokenizer<current, pack_string<'{', tail...>>
    {
            // Number inside braces
            using numbers = extract_braces_numbers_t<tail...>;
            using current_
                = tuple_cat_t<current, typename numbers::numbers>;
            using tokens =
                typename tokenizer<current_, typename numbers::remaining>::tokens;
    };

    template<typename current>
    struct tokenizer<current, pack_string<>>
    {
            // Base case
            using tokens = current;
    };

    template<typename string>
    using tokenizer_t =
        typename tokenizer<std::tuple<>, string>::tokens;
} // namespace e_regex

#endif /* E_REGEX_TOKENIZER_HPP_*/

namespace e_regex
{
    template<typename parsed, typename branches, auto group_index>
    struct tree_builder_branches;

    template<typename... parsed, typename subregex, typename... subregexes, auto group_index>
    struct tree_builder_branches<std::tuple<parsed...>,
                                 std::tuple<subregex, subregexes...>,
                                 group_index>
    {
            using subtree =
                typename tree_builder_helper<void, subregex, group_index>::tree;

            using tree =
                typename tree_builder_branches<std::tuple<parsed..., subtree>,
                                               std::tuple<subregexes...>,
                                               subtree::next_group_index>::tree;
    };

    template<typename... parsed, auto group_index>
    struct tree_builder_branches<std::tuple<parsed...>, std::tuple<>, group_index>
    {
            using tree = nodes::simple<void, parsed...>;
    };

    template<typename regex>
    struct tree_builder;

    template<char... regex>
    struct tree_builder<pack_string<regex...>>
        : public tree_builder_branches<
              std::tuple<>,
              split_t<'|', tokenizer_t<pack_string<regex...>>>,
              0>
    {};

} // namespace e_regex

#endif /* E_REGEX_TREE_BUILDER_HPP_*/

namespace e_regex
{
    namespace __private
    {
        template<typename T, typename N = void>
        struct optimizer
            : public optimizer<N, typename N::template optimize<>>
        {};

        template<typename T>
        struct optimizer<T, void>
            : public optimizer<T, typename T::template optimize<>>
        {};

        template<typename T>
        struct optimizer<T, T>
        {
                using type = T;
        };
    } // namespace __private

    template<static_string expression>
    struct regex
    {
            using ast = typename __private::optimizer<
                typename tree_builder<build_pack_string_t<expression>>::tree>::type;

            static constexpr __attribute__((always_inline)) auto
                match(literal_string_view<> data)
            {
                return match_result<ast> {data};
            }

            constexpr __attribute__((always_inline)) auto
                operator()(literal_string_view<> data) const
            {
                return match(data);
            }

            static __attribute__((always_inline)) constexpr auto
                get_expression()
            {
                return ast::expression.to_view();
            }

            template<typename other>
            static constexpr __attribute__((always_inline)) auto
                is_independent() -> bool
            {
                using intersection = admitted_sets_intersection_t<
                    typename nodes::extract_admission_set<ast>::type,
                    typename nodes::extract_admission_set<typename other::ast>::type>;

                return intersection::empty;
            }

            template<typename T>
            static constexpr __attribute__((always_inline)) auto
                is_independent(T /*other*/) -> bool
            {
                return is_independent<T>();
            }

            static constexpr auto& groups
                = nodes::group_getter<ast>::value;
    };

} // namespace e_regex

#endif /* E_REGEX_REGEX_HPP_*/

#ifndef E_REGEX_TOKENIZATION_TOKENIZER_HPP_
#define E_REGEX_TOKENIZATION_TOKENIZER_HPP_

#include <tuple>

#ifndef E_REGEX_TOKENIZATION_ITERATOR_HPP_
#define E_REGEX_TOKENIZATION_ITERATOR_HPP_

#include <algorithm>
#include <array>
#include <concepts>
#include <iostream>
#include <tuple>
#include <type_traits>

namespace e_regex::tokenization
{
    namespace __private
    {
        template<typename Regex, typename Token_Type>
        struct token_matcher
        {
                Token_Type                  type;
                [[no_unique_address]] Regex matcher;

                constexpr token_matcher(Token_Type type, Regex /*regex*/)
                    : type {type}
                {}
        };

        template<token_definition auto... tokens>
        struct separator_splitter;

        template<is_token_c auto token, token_definition auto... tail>
        struct separator_splitter<token, tail...>
        {
                using rec = separator_splitter<tail...>;

                static constexpr auto tokens = std::tuple_cat(
                    std::make_tuple(token_matcher {
                        token.type,
                        // Adding ^ to alway match the beginning of
                        // current
                        e_regex::regex<static_string {"^"} + token.matcher> {}}),
                    rec::tokens);
                static constexpr auto separators = rec::separators;
        };

        template<is_separator_c auto token, token_definition auto... tail>
        struct separator_splitter<token, tail...>
        {
                using rec = separator_splitter<tail...>;

                static constexpr auto tokens     = rec::tokens;
                static constexpr auto separators = std::tuple_cat(
                    std::make_tuple(
                        e_regex::regex<static_string {"^"} + token.matcher> {}),
                    rec::separators);
        };

        template<>
        struct separator_splitter<>
        {
                static constexpr auto tokens     = std::tuple<> {};
                static constexpr auto separators = std::tuple<> {};
        };
    } // namespace __private

    template<token_definition auto... _tokens>
        requires(sizeof...(_tokens) > 0)
    class iterator
    {
        private:
            using data = __private::separator_splitter<_tokens...>;

        public:
            using token_type_t
                = decltype(std::get<0>(data::tokens).type);

            static_assert(
                std::apply(
                    [](auto... args) {
                        return (
                            ...
                            && std::same_as<token_type_t, decltype(args.type)>);
                    },
                    data::tokens),
                "Tokens must have the same class type");

            struct token_t
            {
                    literal_string_view<> value;
                    token_type_t          type;
            };

        private:
            literal_string_view<> current;
            literal_string_view<> original_input;
            token_t               last_match;
            bool                  valid = true;

            [[nodiscard]] constexpr auto match_separator() -> bool
            {
                auto matches = std::apply(
                    [this](auto... regex) {
                        return std::array {regex(current).to_view()...};
                    },
                    data::separators);

                auto best_match = std::max_element(
                    matches.begin(),
                    matches.end(),
                    [](literal_string_view<> first,
                       literal_string_view<> second) {
                        return first.size() < second.size();
                    });

                if (best_match->size() > 0)
                {
                    current = literal_string_view<> {
                        best_match->end(), current.end()};
                    return true;
                }

                return false;
            }

            [[nodiscard]] constexpr auto match_token() -> bool
            {
                auto matches = std::apply(
                    [this](auto... token) {
                        return std::array {token_t {
                            .value = token.matcher(current).to_view(),
                            .type  = token.type}...};
                    },
                    data::tokens);

                auto best_match = std::max_element(
                    matches.begin(),
                    matches.end(),
                    [](token_t& first, token_t& second) {
                        return first.value.size() < second.value.size();
                    });

                if (best_match->value.size() > 0)
                {
                    current = literal_string_view<> {
                        best_match->value.end(), current.end()};
                    last_match = *best_match;
                    return true;
                }

                return false;
            }

        public:
            constexpr explicit iterator(literal_string_view<> input)
                : current {input}, original_input {input}
            {
                if (!input.empty())
                {
                    operator++();
                }
            }

            constexpr iterator(const iterator& other) noexcept = default;
            constexpr auto operator=(const iterator& other)
                -> iterator&                              = default;
            constexpr iterator(iterator&& other) noexcept = default;
            constexpr auto operator=(iterator&& other) noexcept
                -> iterator&               = default;
            constexpr ~iterator() noexcept = default;

            constexpr auto begin() const -> iterator
            {
                return iterator {original_input};
            }

            constexpr auto operator++() noexcept -> iterator&
            {
                if (!match_separator()
                    && original_input.begin_ != current.begin())
                {
                    // Invalidate only if a separator is missing
                    // after  a token, aka: this is not the first
                    // match
                    invalidate();
                }
                else if (!match_token())
                {
                    invalidate();
                }

                return *this;
            }

            constexpr auto end() const -> iterator
            {
                return iterator {{}}; // Invalid by definition
            }

            constexpr auto operator==(const iterator& other) const noexcept -> bool
            {
                return (current == other.current
                        && last_match.value == other.last_match.value);
            }

            constexpr auto operator!=(const iterator& other) const noexcept -> bool
            {
                return !operator==(other);
            }

            constexpr auto operator->() const noexcept
            {
                return &last_match;
            }

            constexpr auto operator*() const noexcept
            {
                return last_match;
            }

            constexpr operator bool() const noexcept
            {
                return !last_match.value.empty();
            }

            constexpr void invalidate() noexcept
            {
                current    = {};
                last_match = {};
            }
    };
} // namespace e_regex::tokenization

#endif /* E_REGEX_TOKENIZATION_ITERATOR_HPP_*/

namespace e_regex::tokenization
{
    template<token_definition auto... _tokens>
    class tokenizer
    {
        public:
            static constexpr auto match(literal_string_view<> input)
            {
                return tokenization::iterator<_tokens...> {input};
            }

            constexpr auto operator()(literal_string_view<> input) const
            {
                return match(input);
            }
    };
} // namespace e_regex::tokenization
#endif /* E_REGEX_TOKENIZATION_TOKENIZER_HPP_*/

#endif /* E_REGEX_E_REGEX_HPP_*/
