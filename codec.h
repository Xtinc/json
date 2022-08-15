#ifndef CODEC_H
#define CODEC_H

#include "json.h"
#include <limits>
#include <cstring>
#include <sstream>

namespace serialization
{
    namespace details
    {
        // Major type
        constexpr uint64_t ucPInt = 0u;
        constexpr uint64_t ucNInt = 1u << 5;
        constexpr uint64_t ucbStr = 2u << 5;
        constexpr uint64_t uctStr = 3u << 5;
        constexpr uint64_t ucArray = 4u << 5;
        constexpr uint64_t ucMap = 5u << 5;
        constexpr uint64_t ucSemantic = 6u << 5;
        constexpr uint64_t ucfloat = 7u << 5;
        constexpr uint64_t ucsimple = 7u << 5;
        constexpr uint64_t ucMaskH = 0xe0u; // get the first 3 bits

        // details type

        constexpr uint64_t ucLength1 = 24u;
        constexpr uint64_t ucLength2 = 25u;
        constexpr uint64_t ucLength4 = 26u;
        constexpr uint64_t ucLength8 = 27u;

        constexpr uint64_t ucFalse = 20u;
        constexpr uint64_t ucTrue = 21u;
        constexpr uint64_t ucNull = 22u;
        constexpr uint64_t ucUndefined = 23u;
        constexpr uint64_t ucSingleFloat = 26u;
        constexpr uint64_t ucDoubleFloat = 27u;

        constexpr uint64_t ucDataTime = 0u;
        // constexpr uint64_t ucEpochTime = 1u;
        constexpr uint64_t ucMaskL = 0x1fu; // the last 5 bits.
    }                                       // namespace details

    class CborStream
    {
    private:
        template <typename Tuple, std::size_t I, typename = void>
        struct encode_each_in_tuple_helper
        {
            static void apply(Tuple &&tp, CborStream &cbs, size_t &len)
            {
                len += cbs.EncodeData(std::get<I>(std::forward<Tuple>(tp)));
                encode_each_in_tuple_helper<Tuple, I + 1u>::apply(std::forward<Tuple>(tp), cbs, len);
            }
        };

        template <typename Tuple, std::size_t I>
        struct encode_each_in_tuple_helper<Tuple, I,
                                           enable_if_t<I == std::tuple_size<typename std::decay<Tuple>::type>::value>>
        {
            static void apply(Tuple &&, CborStream &, size_t &len) {}
        };

        template <typename F>
        struct reflect_recur_func;

        template <typename T, typename = void>
        struct encode_each_in_reflected_helper
        {
            static void apply(T &&obj, CborStream &cbs, size_t &len, const char *fieldName = "")
            {
                // prevent compiler rearrange
                len += cbs.EncodeData(fieldName);
                len += cbs.EncodeData(std::forward<T>(obj));
            }
        };

        template <typename T>
        struct encode_each_in_reflected_helper<T, enable_if_t<is_reflected_object<decay_t<T>>::value>>
        {
            static void apply(T &&obj, CborStream &cbs, size_t &len, const char *fieldName = "")
            {
                if (strlen(fieldName) != 0)
                {
                    len += cbs.EncodeData(fieldName);
                }
                using ReflectedType = decltype(StructMetaInfo<decay_t<T>>::Info());
                len += cbs.EncodeTagAndValue(details::ucMap, std::tuple_size<ReflectedType>::value);
                reflect_foreach(std::forward<T>(obj), reflect_recur_func<T>(cbs, len));
            }
        };

        template <typename F>
        struct reflect_recur_func
        {
        public:
            reflect_recur_func(CborStream &_cbs, size_t &_len) : cbs(_cbs), len(_len) {}

            template <typename Value>
            void operator()(const char *fieldName, Value &&value)
            {
                encode_each_in_reflected_helper<Value>::apply(value, cbs, len, fieldName);
            }

        private:
            CborStream &cbs;
            size_t &len;
        };

    public:
        CborStream() : m_iErr(0), m_pIter(m_vBuf.begin()), m_pEnd(m_vBuf.end()) {}

        template <typename T>
        CborStream &operator<<(const T &t)
        {
            EncodeData(t);
            m_pIter = m_vBuf.begin();
            m_pEnd = m_vBuf.end();
            return *this;
        }

        template <typename T>
        CborStream &operator>>(T &t)
        {
            m_iErr = 0;
            auto len = DecodeData(t, m_iErr);
            if (m_iErr == 0)
            {
                m_vBuf.erase(0, len);
            }
            m_pIter = m_vBuf.begin();
            m_pEnd = m_vBuf.end();
            return *this;
        }

        std::string GetData() const
        {
            return m_vBuf;
        }

        /* shouldn't be used unless u know what it means.
        void InsertRawDictHeader(int s)
        {
            // assert s is in range in unsigned int.
            // first byte type.
            unsigned int ele_num = s;
            auto len = GetLength(ele_num);
            switch (len)
            {
            case 4:
                m_vBuf.insert(0, 1, static_cast<char>(details::ucMap + details::ucLength4));
                break;
            case 2:
                m_vBuf.insert(0, 1, static_cast<char>(details::ucMap + details::ucLength4));
                break;
            case 1:
                m_vBuf.insert(0, 1, static_cast<char>(details::ucMap + details::ucLength4));
                break;
            case 0:
                m_vBuf.insert(0, 1, static_cast<char>(details::ucMap + ele_num));
                return;
            default:
                // throw Exception("too long");
                break;
            }

            switch (len)
            {
            case 4:
                m_vBuf.insert(1, 1, (ele_num >> 24) & 0xffU);
                m_vBuf.insert(2, 1, (ele_num >> 16) & 0xffU);
            case 2:
                m_vBuf.insert(1, 1, (ele_num >> 8) & 0xffU);
            case 1:
                m_vBuf.insert(1, 1, ele_num & 0xffU);
            }
            m_pIter = m_vBuf.begin();
            m_pEnd = m_vBuf.end();
        }*/

        int GetErrNum() const
        {
            return m_iErr;
        }

        void ClearData()
        {
            m_vBuf.clear();
            m_pIter = m_vBuf.begin();
            m_pEnd = m_vBuf.end();
        }

    private:
        int m_iErr;
        std::string m_vBuf;

        std::string::iterator m_pIter;
        std::string::iterator m_pEnd;

        template <typename Type>
        enable_if_t<std::is_unsigned<Type>::value, std::size_t> GetLength(Type val)
        {
            if (val < 24)
            {
                return 0;
            }
            else if (val <= std::numeric_limits<unsigned char>::max())
            {
                return 1;
            }
            else if (val <= std::numeric_limits<unsigned short>::max())
            {
                return 2;
            }
            else if (val <= UINT32_MAX)
            {
                return 4;
            }
            else
            {
                return 8;
            }
        }

        size_t EncodeDirectly(uint64_t tag, uint64_t additional)
        {
            m_vBuf.push_back(static_cast<char>(tag + additional));
            return 1;
        }

        template <typename Type>
        enable_if_t<std::is_unsigned<Type>::value, std::size_t> EncodeTagAndValue(uint64_t tag, const Type &t)
        {
            auto len = GetLength(t);
            m_vBuf.reserve(m_vBuf.size() + len + 1);

            switch (len)
            {
            case 8:
                EncodeDirectly(tag, details::ucLength8);
                break;
            case 4:
                EncodeDirectly(tag, details::ucLength4);
                break;
            case 2:
                EncodeDirectly(tag, details::ucLength2);
                break;
            case 1:
                EncodeDirectly(tag, details::ucLength1);
                break;
            case 0:
                return EncodeDirectly(tag, t);
            default:
                // throw Exception("too long");
                break;
            }

            switch (len)
            {
            // no break here , I need different entry point for different type.
            case 8:
                m_vBuf.push_back(((uint64_t)t >> 56) & 0xffU);
                m_vBuf.push_back(((uint64_t)t >> 48) & 0xffU);
                m_vBuf.push_back(((uint64_t)t >> 40) & 0xffU);
                m_vBuf.push_back(((uint64_t)t >> 32) & 0xffU);
            case 4:
                m_vBuf.push_back((t >> 24) & 0xffU);
                m_vBuf.push_back((t >> 16) & 0xffU);
            case 2:
                m_vBuf.push_back((t >> 8) & 0xffU);
            case 1:
                m_vBuf.push_back(t & 0xffU);
            default:
                break;
            }

            return 1 + len;
        }

        size_t DecodeDirectly(uint64_t &tag, uint64_t &additional, int &err)
        {
            err = 0;
            if (m_pIter == m_pEnd)
            {
                err = 1;
                // todo : how to deal with these errcode?
                return 0;
            }
            auto uOct = *(m_pIter++);
            tag = uOct & details::ucMaskH;
            additional = uOct & details::ucMaskL;
            return 1;
        }

        template <typename Type>
        enable_if_t<std::is_unsigned<Type>::value, std::size_t> DecodeTagAndValue(uint64_t &tag, Type &t, int &err)
        {
            err = 0;
            if (m_pIter == m_pEnd)
            {
                err = 1;
                return 0;
            }
            auto additional = details::ucUndefined;
            auto len = DecodeDirectly(tag, additional, err);
            if (additional < details::ucLength1)
            {
                t = additional;
                return len;
            }
            t = 0u;
            switch (additional)
            {
            case details::ucLength8:
                if (std::distance(m_pIter, m_pEnd) < 8)
                {
                    err = 1;
                    return 0;
                }
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++))) << 56;
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++))) << 48;
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++))) << 40;
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++))) << 32;
                len += 4;
            case details::ucLength4:
                if (std::distance(m_pIter, m_pEnd) < 4)
                {
                    err = 1;
                    return 0;
                }
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++))) << 24;
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++))) << 16;
                len += 2;
            case details::ucLength2:
                if (std::distance(m_pIter, m_pEnd) < 2)
                {
                    err = 1;
                    return 0;
                }
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++))) << 8;
                len++;
            case details::ucLength1:
                if (std::distance(m_pIter, m_pEnd) < 1)
                {
                    err = 1;
                    return 0;
                }
                t |= static_cast<Type>(reinterpret_cast<const unsigned char &>(*(m_pIter++)));
                len++;
                return len;
            default:
                break;
            }
            err = 1; // bad additional value.
            return 0;
        }
        // above are tools.

        size_t EncodeData(const bool &t)
        {
            return EncodeDirectly(details::ucsimple, t ? details::ucTrue : details::ucFalse);
        }

        size_t DecodeData(bool &t, int &err)
        {
            err = 0;
            auto tag = details::ucUndefined;
            auto value = details::ucUndefined;
            auto len = DecodeTagAndValue(tag, value, err);
            if (tag == details::ucsimple)
            {
                if (value == details::ucTrue)
                {
                    t = true;
                    return len;
                }
                else if (value == details::ucFalse)
                {
                    t = false;
                    return len;
                }
                else
                {
                    err = 1; // not boolean
                    return 0;
                }
            }
            err = 1; // not simple type
            return 0;
        }

        size_t EncodeData(const char *t)
        {
            auto bytes = strlen(t);
            auto len = EncodeTagAndValue(details::uctStr, bytes);
            m_vBuf.insert(std::end(m_vBuf), t, t + bytes);
            return len + bytes;
        }

        size_t EncodeData(const float &t)
        {
            static_assert(sizeof(float) == 4, "sizeof(float) expected to be 4");
            auto len = EncodeDirectly(details::ucfloat, details::ucSingleFloat);
            const char *p = reinterpret_cast<const char *>(&t);
            float ft = 0.0f;
            //
            for (auto i = 1u; i <= sizeof(ft); ++i)
            {
                m_vBuf.push_back(p[sizeof(ft) - i]);
            }
            return len + sizeof(ft);
        }

        size_t DecodeData(float &t, int &err)
        {
            static_assert(sizeof(float) == 4, "sizeof(float) expected to be 4");
            err = 0;
            auto tag = details::ucUndefined;
            auto value = details::ucUndefined;
            auto len = DecodeDirectly(tag, value, err);
            if (tag != details::ucfloat)
            {
                err = 1;
                return 0; // not floating-point
            }
            if (value != details::ucSingleFloat)
            {
                err = 1;
                return 0; // not single-precision floating-point
            }
            if (std::distance(m_pIter, m_pEnd) < static_cast<int>(sizeof(float)))
            {
                err = 1;
                return 0; // not enough input
            }

            char *p = reinterpret_cast<char *>(&t);
            float ft = 0.0f;
            for (auto i = 1u; i <= sizeof(ft); ++i)
            {
                p[sizeof(ft) - i] = *(m_pIter++);
            }
            return len + sizeof(ft);
        }

        size_t EncodeData(const double &t)
        {
            static_assert(sizeof(double) == 8, "sizeof(double) expected to be 8");
            auto len = EncodeDirectly(details::ucfloat, details::ucDoubleFloat);
            const char *p = reinterpret_cast<const char *>(&t);
            double ft = 0.0;
            //
            for (auto i = 1u; i <= sizeof(ft); ++i)
            {
                m_vBuf.push_back(p[sizeof(ft) - i]);
            }
            return len + sizeof(ft);
        }

        size_t DecodeData(double &t, int &err)
        {
            static_assert(sizeof(double) == 8, "sizeof(double) expected to be 8");
            err = 0;
            auto tag = details::ucUndefined;
            auto value = details::ucUndefined;
            auto len = DecodeDirectly(tag, value, err);
            if (tag != details::ucfloat)
            {
                err = 1;
                return 0; // not floating-point
            }
            if (value != details::ucDoubleFloat)
            {
                err = 1;
                return 0; // not single-precision floating-point
            }
            if (std::distance(m_pIter, m_pEnd) < static_cast<int>(sizeof(double)))
            {
                err = 1;
                return 0; // not enough input
            }

            char *p = reinterpret_cast<char *>(&t);
            // double ft = 0.0;
            for (auto i = 1u; i <= sizeof(double); ++i)
            {
                p[sizeof(double) - i] = *(m_pIter++);
            }
            return len + sizeof(double);
        }

        size_t EncodeData(const Json &t)
        {
            size_t len = 0;
            switch (t.type())
            {
            case Json::Type::FLOAT:
                len += EncodeData(t.number_value());
                break;
            case Json::Type::INTEGER:
                len += EncodeData(t.int_value());
                break;
            case Json::Type::STRING:
                len += EncodeData(t.string_value());
                break;
            case Json::Type::BOOL:
                len += EncodeData(t.bool_value());
                break;
            case Json::Type::ARRAY:
                len += EncodeData(t.array_items());
                break;
            case Json::Type::OBJECT:
                len += EncodeData(t.object_items());
                break;
            default:
                break;
            }
            return len;
        }

        template <typename Type>
        enable_if_t<std::is_enum<Type>::value, std::size_t> EncodeData(const Type &t)
        {
            return EncodeData(enum_to_string(t));
        }

        template <typename T, typename U>
        size_t EncodeData(const std::pair<T, U> &t)
        {
            auto len = EncodeData(t.first) + EncodeData(t.second);
            return len;
        }

        template <typename... Types>
        size_t EncodeData(const std::tuple<Types...> &t)
        {
            using StdTuple = const std::tuple<Types...> &;
            auto len = EncodeTagAndValue(details::ucMap, std::tuple_size<std::tuple<Types...>>::value);
            encode_each_in_tuple_helper<StdTuple, 0u>::apply(t, *this, len);
            return len;
        }

        template <typename Type>
        enable_if_t<std::is_unsigned<Type>::value, std::size_t> EncodeData(const Type &t)
        {
            return EncodeTagAndValue(details::ucPInt, t);
        }

        // check const?
        template <typename Type>
        enable_if_t<std::is_unsigned<Type>::value, std::size_t> DecodeData(Type &t, int &err)
        {
            err = 0;
            auto tag = details::ucUndefined;
            auto len = DecodeTagAndValue(tag, t, err);
            if (tag != details::ucPInt)
            {
                err = 1; // type incorrect.
                return 0;
            }
            return len;
        }

        template <typename Type>
        enable_if_t<std::is_signed<Type>::value, std::size_t> EncodeData(const Type &t)
        {
            return t >= 0 ? EncodeTagAndValue(details::ucPInt, (uint64_t)t)
                          : EncodeTagAndValue(details::ucNInt, (uint64_t)(-t - 1));
        }

        template <typename Type>
        enable_if_t<std::is_signed<Type>::value, std::size_t> DecodeData(Type &t, int &err)
        {
            err = 0;
            auto tag = details::ucUndefined;
            uint64_t val = 0;
            auto len = DecodeTagAndValue(tag, val, err);
            switch (tag)
            {
            case details::ucPInt:
                t = static_cast<Type>(val);
                break;
            case details::ucNInt:
                t = -1 - static_cast<Type>(val);
                break;
            default:
                err = 1;
                return 0;
            }
            return len;
        }

        template <typename Type>
        enable_if_t<is_signed_container<Type>::value, std::size_t> EncodeData(const Type &t)
        {
            auto len = EncodeTagAndValue(details::uctStr, t.size());
            m_vBuf.insert(std::end(m_vBuf), std::begin(t), std::end(t));
            return len + t.size();
        }

        template <typename Type>
        enable_if_t<is_signed_container<Type>::value, std::size_t> DecodeData(Type &t, int &err)
        {
            err = 0;
            auto tag = details::ucUndefined;
            auto value = details::ucUndefined;
            auto len = DecodeTagAndValue(tag, value, err);
            if (tag != details::uctStr)
            {
                err = 1; // type error!
                return 0;
            }
            auto dist = std::distance(m_pIter, m_pEnd);
            if (dist < static_cast<decltype(dist)>(value))
            {
                err = 1;
                return 0; // not enough input
            }
            t.insert(std::end(t), m_pIter, m_pIter + value);
            std::advance(m_pIter, value);
            return len + value;
        }

        template <typename Type>
        enable_if_t<is_unsigned_container<Type>::value, std::size_t> EncodeData(const Type &t)
        {
            auto len = EncodeTagAndValue(details::ucbStr, t.size());
            m_vBuf.insert(std::end(m_vBuf), std::begin(t), std::end(t));
            return len + t.size();
        }

        template <typename Type>
        enable_if_t<is_unsigned_container<Type>::value, std::size_t> DecodeData(Type &t, int &err)
        {
            err = 0;
            auto tag = details::ucUndefined;
            auto value = details::ucUndefined;
            auto len = DecodeTagAndValue(tag, value, err);
            if (tag != details::ucbStr)
            {
                err = 1; // type error!
                return 0;
            }
            auto dist = std::distance(m_pIter, m_pEnd);
            if (dist < static_cast<decltype(dist)>(value))
            {
                err = 1;
                return 0; // not enough input
            }
            t.insert(std::end(t), m_pIter, m_pIter + value);
            std::advance(m_pIter, value);
            return len + value;
        }

        template <typename Type>
        enable_if_t<!is_signed_container<Type>::value && !is_unsigned_container<Type>::value &&
                        is_stl_array_like<Type>::value,
                    std::size_t>
        EncodeData(const Type &t)
        {
            auto len = EncodeTagAndValue(details::ucArray, t.size());
            for (const auto &i : t)
            {
                len += EncodeData(i);
            }
            return len;
        }

        template <typename Type>
        enable_if_t<!is_signed_container<Type>::value && !is_unsigned_container<Type>::value &&
                        is_stl_array_like<Type>::value,
                    std::size_t>
        DecodeData(Type &t, int &err)
        {
            err = 0;
            auto tag = details::ucUndefined;
            auto value = details::ucUndefined;
            auto len = DecodeTagAndValue(tag, value, err);
            if (tag != details::ucArray)
            {
                err = 1; // type error!
                return 0;
            }
            for (auto i = 0u; i < value; ++i)
            {
                typename Type::value_type ele{};
                len += DecodeData(ele, err);
                t.push_back(ele);
            }
            return len;
        }

        template <typename Type>
        enable_if_t<!is_signed_container<Type>::value && !is_unsigned_container<Type>::value &&
                        is_stl_map_like<Type>::value,
                    std::size_t>
        EncodeData(const Type &t)
        {
            auto len = EncodeTagAndValue(details::ucMap, t.size());
            for (const auto &i : t)
            {
                len = len + EncodeData(i.first) + EncodeData(i.second);
            }
            return len;
        }

        template <typename Type>
        enable_if_t<!is_signed_container<Type>::value && !is_unsigned_container<Type>::value &&
                        is_stl_map_like<Type>::value,
                    std::size_t>
        DecodeData(Type &t, int &err)
        {
            err = 0;
            auto tag = details::ucUndefined;
            auto value = details::ucUndefined;
            auto len = DecodeTagAndValue(tag, value, err);
            if (tag != details::ucMap)
            {
                err = 1; // type error!
                return 0;
            }
            for (auto i = 0u; i < value; ++i)
            {
                typename Type::key_type kt{};
                typename Type::mapped_type mt{};
                len += DecodeData(kt, err);
                len += DecodeData(mt, err);
                t.emplace(kt, mt);
            }
            return len;
        }

        template <typename Type>
        enable_if_t<!std::is_fundamental<Type>::value && !is_signed_container<Type>::value &&
                    !is_unsigned_container<Type>::value &&
                    is_reflected_object<Type>::value,
                    std::size_t>
        EncodeData(const Type &t)
        {
            size_t len = 0;
            encode_each_in_reflected_helper<const Type &>::apply(t, *this, len);
            return len;
        }

        template <typename Type>
        enable_if_t<
                !std::is_fundamental<Type>::value && !std::is_enum<Type>::value && !is_signed_container<Type>::value &&
                !is_unsigned_container<Type>::value && !is_reflected_object<Type>::value &&
                is_overloaded_operator<Type>::value,
            std::size_t>
        EncodeData(const Type &t)
        {
            std::stringstream ss;
            ss << t;
            return EncodeData(ss.str());
        }
    };

    inline Json json_from_string(const std::string &in, std::string &err)
    {
        return Json::parse(in, err, JsonFormat::STRING_COMMENTS);
    }

    inline Json json_from_binary(const std::string &in, std::string &err)
    {
        return Json::parse(in, err, JsonFormat::BINARY_EXTENTED);
    }

    template <typename T, enable_if_t<is_reflected_object<T>::value, int> = 0>
    Json json_from_object(const T &t, std::string &err)
    {
        CborStream cbs;
        cbs << t;
        return Json::parse(cbs.GetData(), err, JsonFormat::BINARY_STANDARD);
    }
} // namespace reclog

#endif