#ifndef TEMPLATES_H
#define TEMPLATES_H

#include <array>
#include <climits>
#include <deque>
#include <forward_list>
#include <list>
#include <map>
#include <queue>
#include <set>
#include <stack>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>

template <typename T>
struct EnumMetaInfo
{
    static std::array<std::string, 0> Info()
    {
        return std::array<std::string, 0>{};
    }
};

template <typename T>
struct StructMetaInfo
{
    static std::tuple<> Info()
    {
        return std::make_tuple();
    }
};

namespace serialization
{
    namespace details
    {
        // void_t
        template <typename... T>
        struct make_void
        {
            using type = void;
        };

        // unwrap_decay_t

        template <class T>
        struct unwarp_refwrapper
        {
            using type = T;
        };

        template <class T>
        struct unwarp_refwrapper<std::reference_wrapper<T>>
        {
            using type = T &;
        };

        // index_sequence

        template <class T, T... Ints>
        class integer_sequence
        {
        public:
            using value_type = T;
            static_assert(std::is_integral<value_type>::value, "not integral type");
            static constexpr std::size_t size() noexcept
            {
                return sizeof...(Ints);
            }
        };

        template <class T, T Begin, T End, bool>
        struct IntSeqImpl
        {
            using TValue = T;
            static_assert(std::is_integral<TValue>::value, "not integral type");
            static_assert(Begin >= 0 && Begin < End, "unexpected argument (Begin<0 || Begin<=End)");

            template <class, class>
            struct IntSeqCombiner;

            template <TValue... Ints0, TValue... Ints1>
            struct IntSeqCombiner<integer_sequence<TValue, Ints0...>, integer_sequence<TValue, Ints1...>>
            {
                using TResult = integer_sequence<TValue, Ints0..., Ints1...>;
            };

            using TResult = typename IntSeqCombiner<
                typename IntSeqImpl<TValue, Begin, Begin + (End - Begin) / 2, (End - Begin) / 2 == 1>::TResult,
                typename IntSeqImpl<TValue, Begin + (End - Begin) / 2, End, (End - Begin + 1) / 2 == 1>::TResult>::TResult;
        };

        template <class T, T Begin>
        struct IntSeqImpl<T, Begin, Begin, false>
        {
            using TValue = T;
            static_assert(std::is_integral<TValue>::value, "not integral type");
            static_assert(Begin >= 0, "unexpected argument (Begin<0)");
            using TResult = integer_sequence<TValue>;
        };

        template <class T, T Begin, T End>
        struct IntSeqImpl<T, Begin, End, true>
        {
            using TValue = T;
            static_assert(std::is_integral<TValue>::value, "not integral type");
            static_assert(Begin >= 0, "unexpected argument (Begin<0)");
            using TResult = integer_sequence<TValue, Begin>;
        };

        template <class T, T N>
        using make_integer_sequence = typename IntSeqImpl<T, 0, N, (N - 0) == 1>::TResult;

    } // namespace details

    template <typename... T>
    using void_t = typename details::make_void<T...>::type;

    template <typename... T>
    using decay_t = typename std::decay<T...>::type;

    template <bool b, typename T = void>
    using enable_if_t = typename std::enable_if<b, T>::type;

    template <typename T>
    using remove_const_t = typename std::remove_const<T>::type;

    template <typename T>
    using remove_reference_t = typename std::remove_reference<T>::type;

    template <typename T>
    using remove_cv_t = typename std::remove_cv<T>::type;

    template <typename T>
    using underlying_type_t = typename std::underlying_type<T>::type;

    template <class T>
    using unwarp_decay_t = typename details::unwarp_refwrapper<typename std::decay<T>::type>::type;

    template <std::size_t... Ints>
    using index_sequence = details::integer_sequence<std::size_t, Ints...>;

    template <std::size_t N>
    using make_index_sequence = details::make_integer_sequence<std::size_t, N>;

    template <class... T>
    using index_sequence_for = make_index_sequence<sizeof...(T)>;

    namespace details
    {
        template <typename T>
        struct is_stl_list : std::false_type
        {
        };
        template <typename Args, size_t N>
        struct is_stl_list<std::array<Args, N>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_list<std::vector<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_list<std::deque<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_list<std::list<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_list<std::forward_list<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_list<std::stack<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_list<std::queue<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_list<std::priority_queue<Args...>> : std::true_type
        {
        };

        template <typename T>
        struct is_stl_map : std::false_type
        {
        };
        template <typename... Args>
        struct is_stl_map<std::map<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_map<std::multimap<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_map<std::unordered_map<Args...>> : std::true_type
        {
        };
        template <typename... Args>
        struct is_stl_map<std::unordered_multimap<Args...>> : std::true_type
        {
        };

        template <typename T, typename = void_t<>>
        struct is_signed_char_container : std::false_type
        {
        };
        template <typename T>
        struct is_signed_char_container<T, typename std::enable_if<std::is_same<typename T::value_type, char>::value>::type>
            : std::true_type
        {
        };
        template <typename T, typename = void_t<>>
        struct is_unsigned_char_container : std::false_type
        {
        };
        template <typename T>
        struct is_unsigned_char_container<T,
                                          typename std::enable_if<std::is_same<typename T::value_type, unsigned char>::value>::type> : std::true_type
        {
        };

        template <typename, typename = void_t<>>
        struct is_overloaded_operator : public std::false_type
        {
        };
        template <typename T>
        struct is_overloaded_operator<T, void_t<decltype(*(std::ostream *)nullptr << std::declval<T>())>>
            : public std::true_type
        {
        };

        template <typename R, typename P, size_t N, size_t... I>
        constexpr std::array<R, N> to_array_impl(P (&a)[N], index_sequence<I...>) noexcept
        {
            return {{a[I]...}};
        }

        template <typename R, typename P, size_t N, size_t... I>
        constexpr std::array<R, N> to_array_impl(P (&&a)[N], index_sequence<I...>) noexcept
        {
            return {{std::move(a[I])...}};
        }

    } // namespace details

    template <typename T, size_t N>
    constexpr std::array<T, N> to_array(T (&a)[N])
    {
        return details::to_array_impl<remove_cv_t<T>, T, N>(a, make_index_sequence<N>{});
    }

    template <typename T, size_t N>
    constexpr std::array<T, N> to_array(T (&&a)[N]) noexcept
    {
        return details::to_array_impl<remove_cv_t<T>, T, N>(std::move(a), make_index_sequence<N>{});
    }

    // STL list
    template <typename T>
    struct ISTList
    {
        static constexpr bool const value = details::is_stl_list<typename std::decay<T>::type>::value;
    };
    // STL map
    template <typename T>
    struct ISTLmap
    {
        static constexpr bool const value = details::is_stl_map<typename std::decay<T>::type>::value;
    };
    // containers have char
    template <typename T>
    struct IsSCharContainer
    {
        static constexpr bool const value = details::is_signed_char_container<T>::value;
    };
    // containers have unsigned
    template <typename T>
    struct IsUCharContainer
    {
        static constexpr bool const value = details::is_unsigned_char_container<T>::value;
    };
    // containers have overloaded operator <<
    template <typename T>
    struct IsOverloadedOperator
    {
        static constexpr bool const value = details::is_overloaded_operator<T>::value;
    };

    // reflection

    template <typename T, typename Fields, typename F, size_t... Is>
    void reflect_foreach(T &&obj, Fields &&fields, F &&f, index_sequence<Is...>)
    {
        (void)std::initializer_list<size_t>{
            (f(std::get<0>(std::get<Is>(fields)), obj.*std::get<1>(std::get<Is>(fields))), Is)...};
    }

    template <typename T, typename F>
    void reflect_foreach(T &&obj, F &&f)
    {
        auto fields = StructMetaInfo<decay_t<T>>::Info();
        reflect_foreach(std::forward<T>(obj), fields, std::forward<F>(f),
                        make_index_sequence<std::tuple_size<decltype(fields)>::value>{});
    }

    template <typename T>
    struct IsReflectedObject
    {
        static constexpr bool const value = std::tuple_size<decltype(StructMetaInfo<T>::Info())>::value != 0;
    };

    // enumeration

    template <typename E>
    std::string enum_to_string(const E &e)
    {
        using base_type = underlying_type_t<E>;
        auto const &string_list = EnumMetaInfo<E>::Info();
        const auto index = static_cast<base_type>(e);
        const auto max_size = string_list.max_size();
        // todo : anyway to do static check. assert(index >= max_size, "Error, enum value is not in range");
        if (index >= static_cast<base_type>(max_size))
        {
            return std::string{};
        }
        return string_list[index];
    }

    template <typename E>
    E enum_from_string(const std::string &s)
    {
        auto const &string_list = EnumMetaInfo<E>::Info();
        const auto max_size = string_list.max_size();
        size_t n = 0;
        while (n < max_size && string_list[n] != s)
        {
            ++n;
        }
        // todo: error handling
        return n == max_size ? E{} : static_cast<E>(n);
    }

} // namespace reclog

#define REFLECT(Struct, ...)                                 \
    template <>                                              \
    struct StructMetaInfo<Struct>                            \
    {                                                        \
        static decltype(std::make_tuple(__VA_ARGS__)) Info() \
        {                                                    \
            return std::make_tuple(__VA_ARGS__);             \
        }                                                    \
    };

#define ENUM_STRINGS(E, ...)                                                         \
    static_assert(std::is_enum<E>::value, "Not an enumeration type");                \
                                                                                     \
    template <>                                                                      \
    struct EnumMetaInfo<E>                                                           \
    {                                                                                \
        static decltype(serialization::to_array<const char *>({__VA_ARGS__})) Info() \
        {                                                                            \
            return serialization::to_array<const char *>({__VA_ARGS__});             \
        }                                                                            \
    };                                                                               \
    inline std::ostream &operator<<(std::ostream &os, const E &e)                    \
    {                                                                                \
        os << serialization::enum_to_string(e);                                      \
        return os;                                                                   \
    }                                                                                \
    inline std::istream &operator>>(std::istream &is, E &e)                          \
    {                                                                                \
        std::string s;                                                               \
        is >> s;                                                                     \
        e = serialization::enum_from_string<E>(s);                                   \
        return is;                                                                   \
    }

#define FIELD(classname, field) std::make_tuple(#field, &classname::field)

#endif