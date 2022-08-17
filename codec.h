#ifndef CODEC_H
#define CODEC_H

#include <limits>
#include <cstring>
#include <sstream>
#include <vector>
#include <map>
#include <cmath>
#include "templates.h"

namespace serialization
{
    namespace details
    {
        class JsonValue;

        constexpr int NESTED_DEPTH = 1000;

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

        // Helper for representing null
        struct NullStruct
        {
            bool operator==(NullStruct) const { return true; }

            bool operator<(NullStruct) const { return false; }
        };

        inline std::string esc(char c)
        {
            char buf[12];
            if (static_cast<uint8_t>(c) >= 0x20 && static_cast<uint8_t>(c) <= 0x7f)
            {
                snprintf(buf, sizeof buf, "'%c' (%d)", c, c);
            }
            else
            {
                snprintf(buf, sizeof buf, "(%d)", c);
            }
            return buf;
        }

        inline bool in_range(long x, long lower, long upper)
        {
            return (x >= lower && x <= upper);
        }
    }

    class Json
    {
    public:
        enum Format
        {
            STRING_STANDARD,
            STRING_COMMENTS,
            BINARY_STANDARD,
            BINARY_EXTENTED
        };

        enum Type
        {
            NUL,
            FLOAT,
            INTEGER,
            BOOL,
            STRING,
            ARRAY,
            OBJECT
        };

        using array = std::vector<Json>;
        using object = std::map<std::string, Json>;

        Json() noexcept;

        Json(const Json &json);

        Json(Json &&json) noexcept;

        Json(std::nullptr_t) noexcept;

        Json(double value);

        Json(int value);

        Json(bool value);

        Json(const std::string &value);

        Json(std::string &&value);

        Json(const char *value);

        Json(const array &values);

        Json(array &&values);

        Json(const object &values);

        Json(object &&values);

        // Implicit constructor: anything with a to_json() function.
        template <class T, class = decltype(&T::to_json)>
        Json(const T &t) : Json(t.to_json()) {}

        template <class M, enable_if_t<
                               std::is_constructible<std::string, decltype(std::declval<M>().begin()->first)>::value &&
                                   std::is_constructible<Json, decltype(std::declval<M>().begin()->second)>::value,
                               int> = 0>
        Json(const M &m) : Json(object(m.begin(), m.end())) {}

        template <class V, enable_if_t<
                               std::is_constructible<Json, decltype(*std::declval<V>().begin())>::value,
                               int> = 0>
        Json(const V &v) : Json(array(v.begin(), v.end())) {}

        // This prevents Json(some_pointer) from accidentally producing a bool
        Json(void *) = delete;

        Json &operator=(const Json &json);

        Json &operator=(Json &&json) noexcept;

        // member function

        Type type() const;

        bool is_null() const { return type() == NUL; }

        bool is_number() const { return type() == FLOAT; }

        bool is_bool() const { return type() == BOOL; }

        bool is_string() const { return type() == STRING; }

        bool is_array() const { return type() == ARRAY; }

        bool is_object() const { return type() == OBJECT; }

        double number_value() const;

        int int_value() const;

        bool bool_value() const;

        const std::string &string_value() const;

        const array &array_items() const;

        const object &object_items() const;

        const Json &operator[](size_t i) const;

        const Json &at(size_t i) const;

        const Json &operator[](const std::string &key) const;

        const Json &at(const std::string &key) const;

        Json &at(const std::string &key);

        Json &at(size_t i);

        Json &operator[](const std::string &key);

        Json &operator[](size_t i);

        // Serialize.
        void stringify(std::string &out, Json::Format strategy = STRING_STANDARD) const;

        std::string stringify(Json::Format strategy = Json::Format::STRING_STANDARD) const
        {
            std::string out;
            stringify(out, strategy);
            return out;
        }

        // De-serialize. If parse fails, return Json() and assign an error message to err.
        static Json parse(const std::string &in,
                          std::string &err,
                          Json::Format strategy = Json::Format::STRING_STANDARD);

        static Json parse(const char *in,
                          std::string &err,
                          Json::Format strategy = Json::Format::STRING_STANDARD)
        {
            if (in)
            {
                return parse(std::string(in), err, strategy);
            }
            else
            {
                err = "null input";
                return nullptr;
            }
        }

        static std::vector<Json> parse_multi(
            const std::string &in,
            std::string::size_type &parser_stop_pos,
            std::string &err,
            Json::Format strategy = Json::Format::STRING_STANDARD);

        static inline std::vector<Json> parse_multi(
            const std::string &in,
            std::string &err,
            Json::Format strategy = Json::Format::STRING_STANDARD)
        {
            std::string::size_type parser_stop_pos;
            return parse_multi(in, parser_stop_pos, err, strategy);
        }

        bool operator==(const Json &rhs) const;

        bool operator<(const Json &rhs) const;

        bool operator!=(const Json &rhs) const { return !(*this == rhs); }

        bool operator<=(const Json &rhs) const { return !(rhs < *this); }

        bool operator>(const Json &rhs) const { return (rhs < *this); }

        bool operator>=(const Json &rhs) const { return !(*this < rhs); }

        // Return true if this is a JSON object and, for each item in types, has a field of
        // the given type. If not, return false and set err to a descriptive message.
        using shape = std::initializer_list<std::pair<std::string, Type>>;

        bool has_shape(const shape &types, std::string &err) const;

    private:
        std::unique_ptr<details::JsonValue> m_ptr;
    };

    namespace details
    {
        // Serialization

        inline static void dump(NullStruct, std::string &out)
        {
            out += "null";
        }

        inline static void dump(double value, std::string &out)
        {
            if (std::isfinite(value))
            {
                char buf[32];
                snprintf(buf, sizeof buf, "%g", value);
                out += buf;
            }
            else
            {
                out += "null";
            }
        }

        inline static void dump(int value, std::string &out)
        {
            char buf[32];
            snprintf(buf, sizeof buf, "%d", value);
            out += buf;
        }

        inline static void dump(bool value, std::string &out)
        {
            out += value ? "true" : "false";
        }

        inline static void dump(const std::string &value, std::string &out)
        {
            out += '"';
            for (size_t i = 0; i < value.length(); i++)
            {
                const char ch = value[i];
                if (ch == '\\')
                {
                    out += "\\\\";
                }
                else if (ch == '"')
                {
                    out += "\\\"";
                }
                else if (ch == '\b')
                {
                    out += "\\b";
                }
                else if (ch == '\f')
                {
                    out += "\\f";
                }
                else if (ch == '\n')
                {
                    out += "\\n";
                }
                else if (ch == '\r')
                {
                    out += "\\r";
                }
                else if (ch == '\t')
                {
                    out += "\\t";
                }
                else if (static_cast<uint8_t>(ch) <= 0x1f)
                {
                    char buf[8];
                    snprintf(buf, sizeof buf, "\\u%04x", ch);
                    out += buf;
                }
                else if (static_cast<uint8_t>(ch) == 0xe2 && static_cast<uint8_t>(value[i + 1]) == 0x80 &&
                         static_cast<uint8_t>(value[i + 2]) == 0xa8)
                {
                    out += "\\u2028";
                    i += 2;
                }
                else if (static_cast<uint8_t>(ch) == 0xe2 && static_cast<uint8_t>(value[i + 1]) == 0x80 &&
                         static_cast<uint8_t>(value[i + 2]) == 0xa9)
                {
                    out += "\\u2029";
                    i += 2;
                }
                else
                {
                    out += ch;
                }
            }
            out += '"';
        }

        inline static void dump(const Json::array &values, std::string &out)
        {
            bool first = true;
            out += "[";
            for (const auto &value : values)
            {
                if (!first)
                    out += ", ";
                value.stringify(out);
                first = false;
            }
            out += "]";
        }

        inline static void dump(const Json::object &values, std::string &out)
        {
            bool first = true;
            out += "{";
            for (const auto &kv : values)
            {
                if (!first)
                    out += ", ";
                dump(kv.first, out);
                out += ": ";
                kv.second.stringify(out);
                first = false;
            }
            out += "}";
        }

        struct Statics
        {
            const Json empty_json{};
            const std::string empty_string{};
            const std::vector<Json> empty_vector{};
            const std::map<std::string, Json> empty_map{};

            Statics() = default;
        };
        static Statics null_object;

        class JsonValue
        {
        protected:
            friend class serialization::Json;

            friend class JsonInt;

            friend class JsonDouble;

            virtual Json::Type type() const = 0;

            virtual bool equals(const JsonValue *other) const = 0;

            virtual bool less(const JsonValue *other) const = 0;

            virtual void dump(std::string &out) const = 0;

            virtual double number_value() const { return 0; }

            virtual int int_value() const { return 0; }

            virtual bool bool_value() const { return false; }

            virtual const std::string &string_value() const { return null_object.empty_string; }

            virtual const Json::array &array_items() const { return null_object.empty_vector; }

            // array
            virtual const Json &operator[](size_t i) const { return null_object.empty_json; }

            virtual Json &operator[](size_t i) { throw std::out_of_range("non-const reference to null json object"); }

            virtual const Json &at(size_t i) const { return null_object.empty_json; }

            virtual Json &at(size_t i) { throw std::out_of_range("non-const reference to null json object"); }

            // map
            virtual const Json::object &object_items() const { return null_object.empty_map; }

            virtual Json &operator[](const std::string &key) { throw std::out_of_range("non-const reference to null json object"); }

            virtual const Json &at(const std::string &key) const { return null_object.empty_json; }

            virtual Json &at(const std::string &key) { throw std::out_of_range("non-const reference to null json object"); }
        };

        // deal with Json Value

        template <Json::Type tag, typename T>
        class Value : public JsonValue
        {
        protected:
            // Constructors
            explicit Value(const T &value) : m_value(value) {}

            explicit Value(T &&value) : m_value(std::move(value)) {}

            Json::Type type() const override
            {
                return tag;
            }

            bool equals(const JsonValue *other) const override
            {
                return m_value == static_cast<const Value<tag, T> *>(other)->m_value;
            }

            bool less(const JsonValue *other) const override
            {
                return m_value < static_cast<const Value<tag, T> *>(other)->m_value;
            }

            void dump(std::string &out) const override { serialization::details::dump(m_value, out); }

        protected:
            T m_value;
        };

        class JsonDouble final : public Value<Json::FLOAT, double>
        {
            double number_value() const override { return m_value; }

            int int_value() const override { return static_cast<int>(m_value); }

            bool equals(const JsonValue *other) const override { return m_value == other->number_value(); }

            bool less(const JsonValue *other) const override { return m_value < other->number_value(); }

        public:
            explicit JsonDouble(double value) : Value(value) {}
        };

        class JsonInt final : public Value<Json::FLOAT, int>
        {
            double number_value() const override { return m_value; }

            int int_value() const override { return m_value; }

            bool equals(const JsonValue *other) const override { return m_value == other->number_value(); }

            bool less(const JsonValue *other) const override { return m_value < other->number_value(); }

        public:
            explicit JsonInt(int value) : Value(value) {}
        };

        class JsonBoolean final : public Value<Json::BOOL, bool>
        {
            bool bool_value() const override { return m_value; }

        public:
            explicit JsonBoolean(bool value) : Value(value) {}
        };

        class JsonString final : public Value<Json::STRING, std::string>
        {
            const std::string &string_value() const override { return m_value; }

        public:
            explicit JsonString(const std::string &value) : Value(value) {}

            explicit JsonString(std::string &&value) : Value(std::move(value)) {}
        };

        class JsonArray final : public Value<Json::ARRAY, Json::array>
        {
            const Json::array &array_items() const override { return m_value; }

            const Json &operator[](size_t i) const override
            {
                return m_value[i];
            }

            Json &operator[](size_t i) override
            {
                return m_value[i];
            }

            const Json &at(size_t i) const override
            {
                return m_value.at(i);
            }

            Json &at(size_t i) override
            {
                return m_value.at(i);
            }

        public:
            explicit JsonArray(const Json::array &value) : Value(value) {}

            explicit JsonArray(Json::array &&value) : Value(move(value)) {}
        };

        class JsonObject final : public Value<Json::OBJECT, Json::object>
        {
            const Json::object &object_items() const override { return m_value; }

            Json &operator[](const std::string &key) override
            {
                return m_value[key];
            }

            const Json &at(const std::string &key) const override
            {
                return m_value.at(key);
            }

            Json &at(const std::string &key) override
            {
                return m_value.at(key);
            }

        public:
            explicit JsonObject(const Json::object &value) : Value(value) {}

            explicit JsonObject(Json::object &&value) : Value(move(value)) {}
        };

        class JsonNull final : public Value<Json::NUL, NullStruct>
        {
        public:
            JsonNull() : Value({}) {}
        };

        // parser
        class StringParser
        {
        public:
            StringParser(const std::string &content, std::string &error_msg,
                         const Json::Format &parser_type = Json::Format::STRING_STANDARD)
                : str(content), i(0), err(error_msg), failed(false), strategy(parser_type) {}

            /* parse_json()
             *
             * Parse a JSON object.
             */
            Json parse_json(int depth)
            {
                if (depth > NESTED_DEPTH)
                {
                    return fail("exceeded maximum nesting depth");
                }

                char ch = get_next_token();
                if (failed)
                {
                    return {};
                }

                if (ch == '-' || (ch >= '0' && ch <= '9'))
                {
                    i--;
                    return parse_number();
                }

                if (ch == 't')
                {
                    return expect("true", true);
                }

                if (ch == 'f')
                {
                    return expect("false", false);
                }

                if (ch == 'n')
                {
                    return expect("null", Json());
                }

                if (ch == '"')
                {
                    return parse_string();
                }

                if (ch == '{')
                {
                    std::map<std::string, Json> GetData;
                    ch = get_next_token();
                    if (ch == '}')
                        return GetData;

                    for (;;)
                    {
                        if (ch != '"')
                            return fail("expected '\"' in object, got " + esc(ch));

                        std::string key = parse_string();
                        if (failed)
                            return {};

                        ch = get_next_token();
                        if (ch != ':')
                            return fail("expected ':' in object, got " + esc(ch));

                        GetData[std::move(key)] = parse_json(depth + 1);
                        if (failed)
                            return {};

                        ch = get_next_token();
                        if (ch == '}')
                            break;
                        if (ch != ',')
                            return fail("expected ',' in object, got " + esc(ch));

                        ch = get_next_token();
                    }
                    return GetData;
                }

                if (ch == '[')
                {
                    std::vector<Json> GetData;
                    ch = get_next_token();
                    if (ch == ']')
                        return GetData;

                    for (;;)
                    {
                        i--;
                        GetData.push_back(parse_json(depth + 1));
                        if (failed)
                            return {};

                        ch = get_next_token();
                        if (ch == ']')
                            break;
                        if (ch != ',')
                            return fail("expected ',' in list, got " + esc(ch));

                        ch = get_next_token();
                        (void)ch;
                    }
                    return GetData;
                }

                return fail("expected value, got " + esc(ch));
            }

            friend class serialization::Json;

        private:
            /* State
             */
            const std::string &str;
            size_t i;
            std::string &err;
            bool failed;
            const Json::Format strategy;

            /* fail(msg, err_ret = Json())
             *
             * Mark this parse as failed.
             */
            Json fail(std::string &&msg)
            {
                return fail(move(msg), Json());
            }

            template <typename T>
            T fail(std::string &&msg, T &&err_ret)
            {
                if (!failed)
                {
                    err = std::move(msg);
                }
                failed = true;
                return err_ret;
            }

            /* consume_garbage()
             *
             * Advance until the current character is non-whitespace and non-comment.
             */
            void consume_garbage()
            {
                consume_whitespace();
                if (strategy == Json::Format::STRING_COMMENTS)
                {
                    bool comment_found = false;
                    do
                    {
                        comment_found = consume_comment();
                        if (failed)
                            return;
                        consume_whitespace();
                    } while (comment_found);
                }
            }

            /* consume_whitespace()
             *
             * Advance until the current character is non-whitespace.
             */
            void consume_whitespace()
            {
                while (str[i] == ' ' || str[i] == '\r' || str[i] == '\n' || str[i] == '\t')
                    i++;
            }

            /* consume_comment()
             *
             * Advance comments (c-style inline and multiline).
             */
            bool consume_comment()
            {
                bool comment_found = false;
                if (str[i] == '/')
                {
                    i++;
                    if (i == str.size())
                        return fail("unexpected end of input after start of comment", false);
                    if (str[i] == '/')
                    { // inline comment
                        i++;
                        // advance until next line, or end of input
                        while (i < str.size() && str[i] != '\n')
                        {
                            i++;
                        }
                        comment_found = true;
                    }
                    else if (str[i] == '*')
                    { // multiline comment
                        i++;
                        if (i > str.size() - 2)
                            return fail("unexpected end of input inside multi-line comment", false);
                        // advance until closing tokens
                        while (!(str[i] == '*' && str[i + 1] == '/'))
                        {
                            i++;
                            if (i > str.size() - 2)
                                return fail(
                                    "unexpected end of input inside multi-line comment", false);
                        }
                        i += 2;
                        comment_found = true;
                    }
                    else
                        return fail("malformed comment", false);
                }
                return comment_found;
            }

            /* get_next_token()
             *
             * Return the next non-whitespace character. If the end of the input is reached,
             * flag an error and return 0.
             */
            char get_next_token()
            {
                consume_garbage();
                if (failed)
                {
                    return (char)0;
                }
                if (i == str.size())
                {
                    return fail("unexpected end of input", (char)0);
                }
                return str[i++];
            }

            /* encode_utf8(pt, out)
             *
             * Encode pt as UTF-8 and add it to out.
             */
            static void encode_utf8(long pt, std::string &out)
            {
                if (pt < 0)
                    return;

                if (pt < 0x80)
                {
                    out += static_cast<char>(pt);
                }
                else if (pt < 0x800)
                {
                    out += static_cast<char>((pt >> 6) | 0xC0);
                    out += static_cast<char>((pt & 0x3F) | 0x80);
                }
                else if (pt < 0x10000)
                {
                    out += static_cast<char>((pt >> 12) | 0xE0);
                    out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
                    out += static_cast<char>((pt & 0x3F) | 0x80);
                }
                else
                {
                    out += static_cast<char>((pt >> 18) | 0xF0);
                    out += static_cast<char>(((pt >> 12) & 0x3F) | 0x80);
                    out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
                    out += static_cast<char>((pt & 0x3F) | 0x80);
                }
            }

            /* parse_string()
             *
             * Parse a string, starting at the current position.
             */
            std::string parse_string()
            {
                std::string out;
                long last_escaped_codepoint = -1;
                while (true)
                {
                    if (i == str.size())
                        return fail("unexpected end of input in string", "");

                    char ch = str[i++];

                    if (ch == '"')
                    {
                        encode_utf8(last_escaped_codepoint, out);
                        return out;
                    }

                    if (in_range(ch, 0, 0x1f))
                        return fail("unescaped " + esc(ch) + " in string", "");

                    // The usual case: non-escaped characters
                    if (ch != '\\')
                    {
                        encode_utf8(last_escaped_codepoint, out);
                        last_escaped_codepoint = -1;
                        out += ch;
                        continue;
                    }

                    // Handle escapes
                    if (i == str.size())
                        return fail("unexpected end of input in string", "");

                    ch = str[i++];

                    if (ch == 'u')
                    {
                        // Extract 4-byte escape sequence
                        std::string esc = str.substr(i, 4);
                        // Explicitly check length of the substring. The following loop
                        // relies on std::string returning the terminating NUL when
                        // accessing str[length]. Checking here reduces brittleness.
                        if (esc.length() < 4)
                        {
                            return fail("bad \\u escape: " + esc, "");
                        }
                        for (size_t j = 0; j < 4; j++)
                        {
                            if (!in_range(esc[j], 'a', 'f') && !in_range(esc[j], 'A', 'F') && !in_range(esc[j], '0', '9'))
                                return fail("bad \\u escape: " + esc, "");
                        }

                        long codepoint = strtol(esc.data(), nullptr, 16);

                        // JSON specifies that characters outside the BMP shall be encoded as a pair
                        // of 4-hex-digit \u escapes encoding their surrogate pair components. Check
                        // whether we're in the middle of such a beast: the previous codepoint was an
                        // escaped lead (high) surrogate, and this is a trail (low) surrogate.
                        if (in_range(last_escaped_codepoint, 0xD800, 0xDBFF) && in_range(codepoint, 0xDC00, 0xDFFF))
                        {
                            // Reassemble the two surrogate pairs into one astral-plane character, per
                            // the UTF-16 algorithm.
                            encode_utf8((((last_escaped_codepoint - 0xD800) << 10) | (codepoint - 0xDC00)) + 0x10000, out);
                            last_escaped_codepoint = -1;
                        }
                        else
                        {
                            encode_utf8(last_escaped_codepoint, out);
                            last_escaped_codepoint = codepoint;
                        }

                        i += 4;
                        continue;
                    }

                    encode_utf8(last_escaped_codepoint, out);
                    last_escaped_codepoint = -1;

                    if (ch == 'b')
                    {
                        out += '\b';
                    }
                    else if (ch == 'f')
                    {
                        out += '\f';
                    }
                    else if (ch == 'n')
                    {
                        out += '\n';
                    }
                    else if (ch == 'r')
                    {
                        out += '\r';
                    }
                    else if (ch == 't')
                    {
                        out += '\t';
                    }
                    else if (ch == '"' || ch == '\\' || ch == '/')
                    {
                        out += ch;
                    }
                    else
                    {
                        return fail("invalid escape character " + esc(ch), "");
                    }
                }
            }

            /* parse_number()
             *
             * Parse a double.
             */
            Json parse_number()
            {
                size_t start_pos = i;

                if (str[i] == '-')
                    i++;

                // Integer part
                if (str[i] == '0')
                {
                    i++;
                    if (in_range(str[i], '0', '9'))
                        return fail("leading 0s not permitted in numbers");
                }
                else if (in_range(str[i], '1', '9'))
                {
                    i++;
                    while (in_range(str[i], '0', '9'))
                        i++;
                }
                else
                {
                    return fail("invalid " + esc(str[i]) + " in number");
                }

                if (str[i] != '.' && str[i] != 'e' && str[i] != 'E' &&
                    (i - start_pos) <= static_cast<size_t>(std::numeric_limits<int>::digits10))
                {
                    return std::atoi(str.c_str() + start_pos);
                }

                // Decimal part
                if (str[i] == '.')
                {
                    i++;
                    if (!in_range(str[i], '0', '9'))
                        return fail("at least one digit required in fractional part");

                    while (in_range(str[i], '0', '9'))
                        i++;
                }

                // Exponent part
                if (str[i] == 'e' || str[i] == 'E')
                {
                    i++;

                    if (str[i] == '+' || str[i] == '-')
                        i++;

                    if (!in_range(str[i], '0', '9'))
                        return fail("at least one digit required in exponent");

                    while (in_range(str[i], '0', '9'))
                        i++;
                }

                return std::strtod(str.c_str() + start_pos, nullptr);
            }

            /* expect(str, res)
             *
             * Expect that 'str' starts at the character that was just read. If it does, advance
             * the input and return res. If not, flag an error.
             */
            Json expect(const std::string &expected, Json res)
            {
                assert(i != 0);
                i--;
                if (str.compare(i, expected.length(), expected) == 0)
                {
                    i += expected.length();
                    return res;
                }
                else
                {
                    return fail("parse error: expected " + expected + ", got " + str.substr(i, expected.length()));
                }
            }
        };

        class CborParser
        {
        public:
            CborParser(const std::string &in, std::string &err_msg)
                : status(DecodeStatus::START), curidx(0), curlen(0), instant_num(0), str(in), err(err_msg)
            {
            }

            Json parser_json(int depth)
            {
                if (depth > NESTED_DEPTH)
                {
                    return fail("exceeded maximum nesting depth");
                }
                switch (status)
                {
                case DecodeStatus::START:
                    parser_start();
                    break;
                case DecodeStatus::TYPE:
                    parser_type();
                    break;
                case DecodeStatus::STRING_SIZE:
                    parser_string_size(DecodeStatus::STRING_DATA);
                    break;
                case DecodeStatus::BYTES_SIZE:
                    parser_string_size(DecodeStatus::BYTES_DATA);
                    break;
                case DecodeStatus::OBJECT_SIZE:
                    parser_container_size(depth + 1, DecodeStatus::OBJECT_DATA);
                    break;
                case DecodeStatus::ARRAY_SIZE:
                    parser_container_size(depth + 1, DecodeStatus::ARRAY_DATA);
                    break;
                case DecodeStatus::PINT:
                    return parser_pint();
                case DecodeStatus::NINT:
                    return parser_nint();
                case DecodeStatus::FLOAT:
                    return parser_float();
                case DecodeStatus::DOUBLE:
                    return parser_double();
                case DecodeStatus::BOOL:
                    status = DecodeStatus::TYPE;
                    return (instant_num == 0x15);
                case DecodeStatus::NIL:
                    status = DecodeStatus::TYPE;
                    return {};
                case DecodeStatus::OBJECT_DATA:
                    return parser_object(depth + 1);
                case DecodeStatus::ARRAY_DATA:
                    return parser_array(depth + 1);
                case DecodeStatus::STRING_DATA:
                case DecodeStatus::BYTES_DATA:
                    return parser_string();
                case DecodeStatus::ERROR:
                default:
                    return {};
                }
                return parser_json(depth + 1);
            }

            friend class serialization::Json;

        private:
            enum DecodeStatus
            {
                START,
                TYPE,
                PINT,
                NINT,
                FLOAT,
                DOUBLE,
                BYTES_SIZE,
                BYTES_DATA,
                STRING_SIZE,
                STRING_DATA,
                ARRAY_SIZE,
                ARRAY_DATA,
                OBJECT_SIZE,
                OBJECT_DATA,
                TAG,
                SPECIAL,
                BOOL,
                NIL,
                ERROR
            } status;
            size_t curidx;
            size_t curlen;
            int instant_num;
            const std::string &str;
            std::string &err;

        private:
            Json fail(std::string &&msg)
            {
                return fail(move(msg), Json());
            }

            template <typename T>
            T fail(std::string &&msg, T &&err_ret)
            {
                if (status != DecodeStatus::ERROR)
                {
                    err = std::move(msg);
                }
                status = DecodeStatus::ERROR;
                return err_ret;
            }

            bool has_bytes(size_t n)
            {
                return curidx + n <= str.size();
            }

            unsigned char get_byte()
            {
                return str.at(curidx++);
            }

            unsigned short get_uint16()
            {
                unsigned short value =
                    static_cast<unsigned short>(get_byte()) << 8 | static_cast<unsigned short>(get_byte());
                return value;
            }

            unsigned int get_uint32()
            {
                unsigned int value = (static_cast<unsigned int>(get_byte()) << 24) |
                                     (static_cast<unsigned int>(get_byte()) << 16) |
                                     (static_cast<unsigned int>(get_byte()) << 8) |
                                     (static_cast<unsigned int>(get_byte()));
                return value;
            }

            unsigned long long get_uint64()
            {
                unsigned long long value = (static_cast<unsigned long long>(get_byte()) << 56) |
                                           (static_cast<unsigned long long>(get_byte()) << 48) |
                                           (static_cast<unsigned long long>(get_byte()) << 40) |
                                           (static_cast<unsigned long long>(get_byte()) << 32) |
                                           (static_cast<unsigned long long>(get_byte()) << 24) |
                                           (static_cast<unsigned long long>(get_byte()) << 16) |
                                           (static_cast<unsigned long long>(get_byte()) << 8) |
                                           (static_cast<unsigned long long>(get_byte()));
                return value;
            }

            float get_float()
            {
                // todo: related to byte_order
                float tmp = 0.0f;
                uint8_t value[4] = {0};
                value[3] = get_byte();
                value[2] = get_byte();
                value[1] = get_byte();
                value[0] = get_byte();
                memcpy(&tmp, &value[0], sizeof(float));
                return tmp;
            }

            double get_double()
            {
                double tmp = 0.0;
                uint8_t value[8] = {0};
                value[7] = get_byte();
                value[6] = get_byte();
                value[5] = get_byte();
                value[4] = get_byte();
                value[3] = get_byte();
                value[2] = get_byte();
                value[1] = get_byte();
                value[0] = get_byte();
                memcpy(&tmp, &value[0], sizeof(double));
                return tmp;
            }

        private:
            void parser_start()
            {
                if (!has_bytes(1))
                {
                    fail("not enough length!");
                }
                auto type = get_byte();
                auto major_type = type >> 5;
                auto minor_type = type & 0x1f;
                switch (major_type)
                {
                case 4:
                    status = DecodeStatus::ARRAY_SIZE;
                    decompose_type(minor_type, minor_type, status);
                    break;
                case 5:
                    status = DecodeStatus::OBJECT_SIZE;
                    decompose_type(minor_type, minor_type, status);
                    break;
                default:
                    fail("root object is not a json object");
                }
            }

            void parser_type()
            {
                if (!has_bytes(1))
                {
                    fail("not enough length!");
                    return;
                }
                auto type = get_byte();
                auto major_type = type >> 5;
                auto minor_type = type & 0x1f;
                switch (major_type)
                {
                case 0: // positive
                    status = DecodeStatus::PINT;
                    decompose_type(minor_type, minor_type, status);
                    break;
                case 1: // negative
                    status = DecodeStatus::NINT;
                    decompose_type(minor_type, minor_type, status);
                    break;
                case 2: // bytes
                    status = DecodeStatus::BYTES_SIZE;
                    decompose_type(minor_type, 0, DecodeStatus::BYTES_DATA, minor_type);
                    break;
                case 3: // string
                    status = DecodeStatus::STRING_SIZE;
                    decompose_type(minor_type, 0, DecodeStatus::STRING_DATA, minor_type);
                    break;
                case 4: // array
                    status = DecodeStatus::ARRAY_SIZE;
                    decompose_type(minor_type, minor_type, status);
                    break;
                case 5: // map
                    status = DecodeStatus::OBJECT_SIZE;
                    decompose_type(minor_type, minor_type, status);
                    break;
                case 6: // tag
                    status = DecodeStatus::TAG;
                    decompose_type(minor_type, minor_type, status);
                    break;
                case 7: // special
                    if (minor_type == 0x14 || minor_type == 0x15)
                    {
                        status = DecodeStatus::BOOL;
                        instant_num = minor_type;
                    }
                    else if (minor_type == 0x16)
                    {
                        status = DecodeStatus::NIL;
                        instant_num = minor_type;
                    }
                    else if (minor_type == 0x18)
                    {
                        status = DecodeStatus::SPECIAL;
                        curlen = 1;
                    }
                    else if (minor_type == 0x19)
                    { // 2 byte
                        curlen = 2;
                        status = DecodeStatus::SPECIAL;
                    }
                    else if (minor_type == 0x1A)
                    { // 4 byte
                        curlen = 4;
                        status = DecodeStatus::FLOAT;
                    }
                    else if (minor_type == 0x1B)
                    { // 8 byte
                        curlen = 8;
                        status = DecodeStatus::DOUBLE;
                    }
                    else
                    {
                        fail("invalid special type", "");
                    }
                    break;
                default:
                    fail("unknow major_type", "");
                    break;
                }
            }

            void parser_string_size(DecodeStatus st)
            {
                if (!has_bytes(curlen))
                {
                    fail("not enough length!", "");
                    return;
                }
                status = st;
                switch (curlen)
                {
                case 0:
                    curlen = instant_num;
                    break;
                case 1:
                    curlen = get_byte();
                    break;
                case 2:
                case 4:
                    curlen = get_uint16();
                    break;
                case 8:
                    fail("extra long bytes", "");
                    break;
                }
            }

            void parser_container_size(int depth, DecodeStatus st)
            {
                if (!has_bytes(curlen))
                {
                    fail("not enough length!", "");
                    return;
                }
                switch (curlen)
                {
                // instant number
                case 0:
                    break;
                    // u8
                case 1:
                    instant_num = get_byte();
                    break;
                    // u16
                case 2:
                    instant_num = get_uint16();
                    break;
                    // u32
                case 4:
                    // todo :overflow
                    {
                        auto temp = get_uint32();
                        if (temp <= (uint32_t)std::numeric_limits<int>::max())
                        {
                            instant_num = static_cast<int>(temp);
                        }
                        else
                        {
                            instant_num = fail("exceed integer range", std::numeric_limits<int>::max());
                        }
                    }
                    break;
                    // u64
                case 8:
                    instant_num = fail("exceed integer range", std::numeric_limits<int>::max());
                    break;
                }
                status = st;
            }

            int parser_pint()
            {
                if (!has_bytes(curlen))
                {
                    return fail("not enough length!", 0);
                }
                int result = 0;
                switch (curlen)
                {
                // instant number
                case 0:
                    result = instant_num;
                    break;
                    // u8
                case 1:
                    result = get_byte();
                    break;
                    // u16
                case 2:
                    result = get_uint16();
                    break;
                    // u32
                case 4:
                    // todo :overflow
                    {
                        auto temp = get_uint32();
                        if (temp <= (uint32_t)std::numeric_limits<int>::max())
                        {
                            result = static_cast<int>(temp);
                        }
                        else
                        {
                            result = fail("exceed integer range", std::numeric_limits<int>::max());
                        }
                    }
                    break;
                    // u64
                case 8:
                    result = fail("exceed integer range", std::numeric_limits<int>::max());
                    break;
                }
                status = DecodeStatus::TYPE;
                return result;
            }

            int parser_nint()
            {
                if (!has_bytes(curlen))
                {
                    return fail("not enough length!", 0);
                }
                int result = 0;
                switch (curlen)
                {
                // instant number
                case 0:
                    result = -1 - instant_num;
                    break;
                    // u8
                case 1:
                    result = -static_cast<int>(get_byte()) - 1;
                    break;
                    // u16
                case 2:
                    result = -static_cast<int>(get_uint16()) - 1;
                    break;
                    // u32
                case 4:
                    // todo :overflow
                    {
                        auto temp = get_uint32();
                        if (temp <= (uint32_t)std::numeric_limits<int>::max())
                        {
                            result = -static_cast<int>(temp) - 1;
                        }
                        else
                        {
                            result = fail("exceed integer range", std::numeric_limits<int>::min());
                        }
                    }
                    break;
                    // u64
                case 8:
                    result = fail("exceed integer range", std::numeric_limits<int>::min());
                    break;
                }
                status = DecodeStatus::TYPE;
                return result;
            }

            float parser_float()
            {
                if (!has_bytes(curlen))
                {
                    return fail("not enough length!", 0.0f);
                }
                status = DecodeStatus::TYPE;
                return get_float();
            }

            double parser_double()
            {
                if (!has_bytes(curlen))
                {
                    return fail("not enough length!", 0.0);
                }
                status = DecodeStatus::TYPE;
                return get_double();
            }

            std::string parser_string()
            {
                if (!has_bytes(curlen))
                {
                    return fail("not enough length!", "");
                }
                status = DecodeStatus::TYPE;
                auto slice = str.substr(curidx, curlen);
                curidx += curlen;
                return slice;
            }

            std::map<std::string, Json> parser_object(int depth)
            {
                status = DecodeStatus::TYPE;
                std::map<std::string, Json> GetData;
                auto len = instant_num;
                for (auto i = 0; i < len; ++i)
                {
                    auto key = parser_json(depth + 1);
                    if (key.type() != Json::STRING)
                    {
                        fail("Object key value is not string: " + key.stringify());
                        break;
                    }
                    GetData.insert(std::make_pair(key.string_value(), parser_json(depth + 1)));
                }
                return GetData;
            }

            std::vector<Json> parser_array(int depth)
            {
                status = DecodeStatus::TYPE;
                std::vector<Json> GetData;
                GetData.reserve(instant_num);
                auto len = instant_num;
                for (auto i = 0; i < len; ++i)
                {
                    GetData.push_back(parser_json(depth + 1));
                }
                return GetData;
            }

            void decompose_type(
                int minor_type, int instant_number, DecodeStatus set_status, size_t setlen = 0)
            {
                if (minor_type <= 0x17)
                {
                    curlen = setlen;
                    instant_num = instant_number;
                    status = set_status;
                }
                else if (minor_type == 0x18)
                { // 1 byte
                    curlen = 1;
                }
                else if (minor_type == 0x19)
                { // 2 byte
                    curlen = 2;
                }
                else if (minor_type == 0x1A)
                { // 4 byte
                    curlen = 4;
                }
                else if (minor_type == 0x1B)
                { // 8 byte
                    curlen = 8;
                }
                else
                {
                    fail("invalid minor type", "");
                }
            }
        };

    }

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

        // shouldn't be used unless u know what it means.
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
        }

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

    Json Json::parse(const std::string &in, std::string &err, Json::Format strategy)
    {
        if (strategy == Json::Format::BINARY_STANDARD)
        {
            details::CborParser parser(in, err);
            Json result = parser.parser_json(0);
            if (parser.status == details::CborParser::DecodeStatus::ERROR)
            {
                return {};
            }
            if (parser.curidx != in.size())
            {
                return parser.fail("unexpected trailing");
            }
            return result;
        }
        else
        {
            details::StringParser parser(in, err, strategy);
            Json result = parser.parse_json(0);

            // Check for any trailing garbage
            parser.consume_garbage();
            if (parser.failed)
            {
                return {};
            }
            if (parser.i != in.size())
            {
                return parser.fail("unexpected trailing " + details::esc(in[parser.i]));
            }
            return result;
        }
    }

    std::vector<Json> Json::parse_multi(const std::string &in,
                                        std::string::size_type &parser_stop_pos,
                                        std::string &err,
                                        Json::Format strategy)
    {
        parser_stop_pos = 0;
        std::vector<Json> json_vec;
        if (strategy == Json::Format::BINARY_STANDARD)
        {
            details::CborParser parser(in, err);
            while (parser.curidx != in.size() && err.empty())
            {
                json_vec.push_back(parser.parser_json(0));
                if (!err.empty())
                {
                    break;
                }
                parser_stop_pos = parser.curidx;
            }
        }
        else
        {
            details::StringParser parser(in, err, strategy);
            while (parser.i != in.size() && !parser.failed)
            {
                json_vec.push_back(parser.parse_json(0));
                if (parser.failed)
                {
                    break;
                }

                // Check for another object
                parser.consume_garbage();
                if (parser.failed)
                {
                    break;
                }
                parser_stop_pos = parser.i;
            }
        }
        return json_vec;
    }

    Json::Json() noexcept : m_ptr(make_unique<details::JsonNull>()) {}

    Json::Json(const Json &json)
    {
        switch (json.type())
        {
        case Type::ARRAY:
            m_ptr = make_unique<details::JsonArray>(json.array_items());
            break;
        case Type::BOOL:
            m_ptr = make_unique<details::JsonBoolean>(json.bool_value());
            break;
        case Type::FLOAT:
            m_ptr = make_unique<details::JsonDouble>(json.number_value());
            break;
        case Type::OBJECT:
            m_ptr = make_unique<details::JsonObject>(json.object_items());
            break;
        case Type::STRING:
            m_ptr = make_unique<details::JsonString>(json.string_value());
            break;
        case Type::NUL:
        default:
            m_ptr = make_unique<details::JsonNull>();
            break;
        }
    }

    Json::Json(Json &&json) noexcept : m_ptr(move(json.m_ptr)) {}

    Json::Json(std::nullptr_t) noexcept : m_ptr(make_unique<details::JsonNull>()) {}

    Json::Json(double value) : m_ptr(make_unique<details::JsonDouble>(value)) {}

    Json::Json(int value) : m_ptr(make_unique<details::JsonInt>(value)) {}

    Json::Json(bool value) : m_ptr(make_unique<details::JsonBoolean>(value)) {}

    Json::Json(const std::string &value) : m_ptr(make_unique<details::JsonString>(value)) {}

    Json::Json(std::string &&value) : m_ptr(make_unique<details::JsonString>(move(value))) {}

    Json::Json(const char *value) : m_ptr(make_unique<details::JsonString>(value)) {}

    Json::Json(const Json::array &values) : m_ptr(make_unique<details::JsonArray>(values)) {}

    Json::Json(Json::array &&values) : m_ptr(make_unique<details::JsonArray>(move(values))) {}

    Json::Json(const Json::object &values) : m_ptr(make_unique<details::JsonObject>(values)) {}

    Json::Json(Json::object &&values) : m_ptr(make_unique<details::JsonObject>(move(values))) {}

    Json::Type Json::type() const { return m_ptr->type(); }

    double Json::number_value() const { return m_ptr->number_value(); }

    int Json::int_value() const { return m_ptr->int_value(); }

    bool Json::bool_value() const { return m_ptr->bool_value(); }

    const std::string &Json::string_value() const { return m_ptr->string_value(); }

    const std::vector<Json> &Json::array_items() const { return m_ptr->array_items(); }

    const std::map<std::string, Json> &Json::object_items() const { return m_ptr->object_items(); }

    const Json &Json::operator[](size_t i) const { return (*m_ptr)[i]; }

    Json &Json::operator[](size_t i) { return (*m_ptr)[i]; }

    const Json &Json::at(size_t i) const { return (*m_ptr).at(i); }

    Json &Json::at(size_t i) { return (*m_ptr).at(i); }

    const Json &Json::operator[](const std::string &key) const { return (*m_ptr)[key]; }

    Json &Json::operator[](const std::string &key) { return (*m_ptr)[key]; }

    const Json &Json::at(const std::string &key) const { return (*m_ptr).at(key); }

    Json &Json::at(const std::string &key) { return (*m_ptr).at(key); }

    Json &Json::operator=(const Json &json)
    {
        switch (json.type())
        {
        case Type::ARRAY:
            m_ptr = make_unique<details::JsonArray>(json.array_items());
            break;
        case Type::BOOL:
            m_ptr = make_unique<details::JsonBoolean>(json.bool_value());
            break;
        case Type::FLOAT:
            m_ptr = make_unique<details::JsonDouble>(json.number_value());
            break;
        case Type::OBJECT:
            m_ptr = make_unique<details::JsonObject>(json.object_items());
            break;
        case Type::STRING:
            m_ptr = make_unique<details::JsonString>(json.string_value());
            break;
        case Type::NUL:
        default:
            m_ptr = make_unique<details::JsonNull>();
            break;
        }
        return *this;
    }

    Json &Json::operator=(Json &&json) noexcept
    {
        m_ptr = move(json.m_ptr);
        return *this;
    }

    bool Json::operator==(const Json &other) const
    {
        if (m_ptr == other.m_ptr)
        {
            return true;
        }
        if (m_ptr->type() != other.m_ptr->type())
        {
            return false;
        }

        return m_ptr->equals(other.m_ptr.get());
    }

    bool Json::operator<(const Json &other) const
    {
        if (m_ptr == other.m_ptr)
        {
            return false;
        }
        if (m_ptr->type() != other.m_ptr->type())
        {
            return m_ptr->type() < other.m_ptr->type();
        }

        return m_ptr->less(other.m_ptr.get());
    }

    bool Json::has_shape(const shape &types, std::string &err) const
    {
        if (!is_object())
        {
            err = "expected JSON object, got " + stringify();
            return false;
        }

        for (auto &item : types)
        {
            if ((*this).at(item.first).type() != item.second)
            {
                err = "bad type for " + item.first + " in " + stringify();
                return false;
            }
        }

        return true;
    }

    void Json::stringify(std::string &out, Json::Format strategy) const
    {
        switch (strategy)
        {
        case Json::Format::BINARY_STANDARD:
        case Json::Format::BINARY_EXTENTED:
        {
            CborStream cbs;
            cbs << *this;
            out = cbs.GetData();
        }
        break;
        default:
            m_ptr->dump(out);
            break;
        }
    }

    inline Json json_from_string(const std::string &in, std::string &err)
    {
        return Json::parse(in, err, Json::Format::STRING_COMMENTS);
    }

    inline Json json_from_binary(const std::string &in, std::string &err)
    {
        return Json::parse(in, err, Json::Format::BINARY_EXTENTED);
    }

    template <typename T, enable_if_t<is_reflected_object<T>::value, int> = 0>
    Json json_from_object(const T &t, std::string &err)
    {
        CborStream cbs;
        cbs << t;
        return Json::parse(cbs.GetData(), err, Json::Format::BINARY_STANDARD);
    }

} // namespace

#endif