#ifndef JSON_H
#define JSON_H

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <initializer_list>

namespace serialization
{
    enum JsonFormat
    {
        STRING_STANDARD,
        STRING_COMMENTS,
        BINARY_STANDARD,
        BINARY_EXTENTED
    };

    class JsonValue;

    class Json
    {
    public:
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

        template <class M, typename std::enable_if<
                               std::is_constructible<std::string, decltype(std::declval<M>().begin()->first)>::value && std::is_constructible<Json, decltype(std::declval<M>().begin()->second)>::value,
                               int>::type = 0>
        Json(const M &m) : Json(object(m.begin(), m.end())) {}

        template <class V, typename std::enable_if<
                               std::is_constructible<Json, decltype(*std::declval<V>().begin())>::value,
                               int>::type = 0>
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
        void stringify(std::string &out, JsonFormat strategy = JsonFormat::STRING_STANDARD) const;

        std::string stringify(JsonFormat strategy = JsonFormat::STRING_STANDARD) const
        {
            std::string out;
            stringify(out, strategy);
            return out;
        }

        // Parse. If parse fails, return Json() and assign an error message to err.
        static Json parse(const std::string &in,
                          std::string &err,
                          JsonFormat strategy = JsonFormat::STRING_STANDARD);

        static Json parse(const char *in,
                          std::string &err,
                          JsonFormat strategy = JsonFormat::STRING_STANDARD)
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
            JsonFormat strategy = JsonFormat::STRING_STANDARD);

        static inline std::vector<Json> parse_multi(
            const std::string &in,
            std::string &err,
            JsonFormat strategy = JsonFormat::STRING_STANDARD)
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
        std::unique_ptr<JsonValue> m_ptr;
    };

    class JsonValue
    {
    protected:
        friend class Json;
        friend class JsonInt;
        friend class JsonDouble;
        virtual Json::Type type() const = 0;
        virtual bool equals(const JsonValue *other) const = 0;
        virtual bool less(const JsonValue *other) const = 0;
        virtual void dump(std::string &out) const = 0;
        virtual double number_value() const;
        virtual int int_value() const;
        virtual bool bool_value() const;
        virtual const std::string &string_value() const;
        virtual const Json::array &array_items() const;
        // array
        virtual const Json &operator[](size_t i) const;
        virtual Json &operator[](size_t i);
        virtual const Json &at(size_t i) const;
        virtual Json &at(size_t i);
        // map
        virtual const Json::object &object_items() const;
        virtual Json &operator[](const std::string &key);
        virtual const Json &at(const std::string &key) const;
        virtual Json &at(const std::string &key);
    };
}

#endif
