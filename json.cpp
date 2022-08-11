#include "json.h"
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <limits>
#include <cstring>
#include <functional>

namespace serialization {
    constexpr int max_depth = 50;

    using std::initializer_list;
    using std::make_unique;
    using std::map;
    using std::move;
    using std::string;
    using std::vector;

    // Helper for representing null
    struct NullStruct {
        bool operator==(NullStruct) const { return true; }

        bool operator<(NullStruct) const { return false; }
    };

    // Serialization

    static void dump(NullStruct, string &out) {
        out += "null";
    }

    static void dump(double value, string &out) {
        if (std::isfinite(value)) {
            char buf[32];
            snprintf(buf, sizeof buf, "%.17g", value);
            out += buf;
        } else {
            out += "null";
        }
    }

    static void dump(int value, string &out) {
        char buf[32];
        snprintf(buf, sizeof buf, "%d", value);
        out += buf;
    }

    static void dump(bool value, string &out) {
        out += value ? "true" : "false";
    }

    static void dump(const string &value, string &out) {
        out += '"';
        for (size_t i = 0; i < value.length(); i++) {
            const char ch = value[i];
            if (ch == '\\') {
                out += "\\\\";
            } else if (ch == '"') {
                out += "\\\"";
            } else if (ch == '\b') {
                out += "\\b";
            } else if (ch == '\f') {
                out += "\\f";
            } else if (ch == '\n') {
                out += "\\n";
            } else if (ch == '\r') {
                out += "\\r";
            } else if (ch == '\t') {
                out += "\\t";
            } else if (static_cast<uint8_t>(ch) <= 0x1f) {
                char buf[8];
                snprintf(buf, sizeof buf, "\\u%04x", ch);
                out += buf;
            } else if (static_cast<uint8_t>(ch) == 0xe2 && static_cast<uint8_t>(value[i + 1]) == 0x80 &&
                       static_cast<uint8_t>(value[i + 2]) == 0xa8) {
                out += "\\u2028";
                i += 2;
            } else if (static_cast<uint8_t>(ch) == 0xe2 && static_cast<uint8_t>(value[i + 1]) == 0x80 &&
                       static_cast<uint8_t>(value[i + 2]) == 0xa9) {
                out += "\\u2029";
                i += 2;
            } else {
                out += ch;
            }
        }
        out += '"';
    }

    static void dump(const Json::array &values, string &out) {
        bool first = true;
        out += "[";
        for (const auto &value: values) {
            if (!first)
                out += ", ";
            value.stringify(out);
            first = false;
        }
        out += "]";
    }

    static void dump(const Json::object &values, string &out) {
        bool first = true;
        out += "{";
        for (const auto &kv: values) {
            if (!first)
                out += ", ";
            dump(kv.first, out);
            out += ": ";
            kv.second.stringify(out);
            first = false;
        }
        out += "}";
    }

    void Json::stringify(string &out) const {
        m_ptr->dump(out);
    }

    // deal with Json Value

    template<Json::Type tag, typename T>
    class Value : public JsonValue {
    protected:
        // Constructors
        explicit Value(const T &value) : m_value(value) {}

        explicit Value(T &&value) : m_value(move(value)) {}

        Json::Type type() const override {
            return tag;
        }

        bool equals(const JsonValue *other) const override {
            return m_value == static_cast<const Value<tag, T> *>(other)->m_value;
        }

        bool less(const JsonValue *other) const override {
            return m_value < static_cast<const Value<tag, T> *>(other)->m_value;
        }

        void dump(string &out) const override { serialization::dump(m_value, out); }

    protected:
        T m_value;
    };

    class JsonDouble final : public Value<Json::NUMBER, double> {
        double number_value() const override { return m_value; }

        int int_value() const override { return static_cast<int>(m_value); }

        bool equals(const JsonValue *other) const override { return m_value == other->number_value(); }

        bool less(const JsonValue *other) const override { return m_value < other->number_value(); }

    public:
        explicit JsonDouble(double value) : Value(value) {}
    };

    class JsonInt final : public Value<Json::NUMBER, int> {
        double number_value() const override { return m_value; }

        int int_value() const override { return m_value; }

        bool equals(const JsonValue *other) const override { return m_value == other->number_value(); }

        bool less(const JsonValue *other) const override { return m_value < other->number_value(); }

    public:
        explicit JsonInt(int value) : Value(value) {}
    };

    class JsonBoolean final : public Value<Json::BOOL, bool> {
        bool bool_value() const override { return m_value; }

    public:
        explicit JsonBoolean(bool value) : Value(value) {}
    };

    class JsonString final : public Value<Json::STRING, string> {
        const string &string_value() const override { return m_value; }

    public:
        explicit JsonString(const string &value) : Value(value) {}

        explicit JsonString(string &&value) : Value(move(value)) {}
    };

    class JsonArray final : public Value<Json::ARRAY, Json::array> {
        const Json::array &array_items() const override { return m_value; }

        const Json &operator[](size_t i) const override;

        Json &operator[](size_t i) override;

        const Json &at(size_t i) const override;

        Json &at(size_t i) override;

    public:
        explicit JsonArray(const Json::array &value) : Value(value) {}

        explicit JsonArray(Json::array &&value) : Value(move(value)) {}
    };

    class JsonObject final : public Value<Json::OBJECT, Json::object> {
        const Json::object &object_items() const override { return m_value; }

        Json &operator[](const string &key) override;

        const Json &at(const string &key) const override;

        Json &at(const string &key) override;

    public:
        explicit JsonObject(const Json::object &value) : Value(value) {}

        explicit JsonObject(Json::object &&value) : Value(move(value)) {}
    };

    class JsonNull final : public Value<Json::NUL, NullStruct> {
    public:
        JsonNull() : Value({}) {}
    };

    struct Statics {
        Json empty_json{};
        const string empty_string{};
        const vector<Json> empty_vector{};
        const map<string, Json> empty_map{};

        Statics() = default;
    };

    static Statics null_object;

    Json::Json() noexcept: m_ptr(make_unique<JsonNull>()) {}

    Json::Json(const Json &json) {
        switch (json.type()) {
            case Type::ARRAY:
                m_ptr = make_unique<JsonArray>(json.array_items());
                break;
            case Type::BOOL:
                m_ptr = make_unique<JsonBoolean>(json.bool_value());
                break;
            case Type::NUMBER:
                m_ptr = make_unique<JsonDouble>(json.number_value());
                break;
            case Type::OBJECT:
                m_ptr = make_unique<JsonObject>(json.object_items());
                break;
            case Type::STRING:
                m_ptr = make_unique<JsonString>(json.string_value());
                break;
            case Type::NUL:
            default:
                m_ptr = make_unique<JsonNull>();
                break;
        }
    }

    Json::Json(Json &&json) noexcept: m_ptr(move(json.m_ptr)) {}

    Json::Json(std::nullptr_t) noexcept: m_ptr(make_unique<JsonNull>()) {}

    Json::Json(double value) : m_ptr(make_unique<JsonDouble>(value)) {}

    Json::Json(int value) : m_ptr(make_unique<JsonInt>(value)) {}

    Json::Json(bool value) : m_ptr(make_unique<JsonBoolean>(value)) {}

    Json::Json(const string &value) : m_ptr(make_unique<JsonString>(value)) {}

    Json::Json(string &&value) : m_ptr(make_unique<JsonString>(move(value))) {}

    Json::Json(const char *value) : m_ptr(make_unique<JsonString>(value)) {}

    Json::Json(const Json::array &values) : m_ptr(make_unique<JsonArray>(values)) {}

    Json::Json(Json::array &&values) : m_ptr(make_unique<JsonArray>(move(values))) {}

    Json::Json(const Json::object &values) : m_ptr(make_unique<JsonObject>(values)) {}

    Json::Json(Json::object &&values) : m_ptr(make_unique<JsonObject>(move(values))) {}

    Json::Type Json::type() const { return m_ptr->type(); }

    double Json::number_value() const { return m_ptr->number_value(); }

    int Json::int_value() const { return m_ptr->int_value(); }

    bool Json::bool_value() const { return m_ptr->bool_value(); }

    const string &Json::string_value() const { return m_ptr->string_value(); }

    const vector<Json> &Json::array_items() const { return m_ptr->array_items(); }

    const map<string, Json> &Json::object_items() const { return m_ptr->object_items(); }

    const Json &Json::operator[](size_t i) const { return (*m_ptr)[i]; }

    Json &Json::operator[](size_t i) { return (*m_ptr)[i]; }

    const Json &Json::at(size_t i) const { return (*m_ptr).at(i); }

    Json &Json::at(size_t i) { return (*m_ptr).at(i); }

    const Json &Json::operator[](const string &key) const { return (*m_ptr)[key]; }

    Json &Json::operator[](const string &key) { return (*m_ptr)[key]; }

    const Json &Json::at(const string &key) const { return (*m_ptr).at(key); }

    Json &Json::at(const string &key) { return (*m_ptr).at(key); }

    double JsonValue::number_value() const { return 0; }

    int JsonValue::int_value() const { return 0; }

    bool JsonValue::bool_value() const { return false; }

    const string &JsonValue::string_value() const { return null_object.empty_string; }

    const vector<Json> &JsonValue::array_items() const { return null_object.empty_vector; }

    const map<string, Json> &JsonValue::object_items() const { return null_object.empty_map; }

    const Json &JsonValue::operator[](size_t) const { return null_object.empty_json; }

    Json &JsonValue::operator[](size_t) { return null_object.empty_json; }

    const Json &JsonValue::at(size_t) const { return null_object.empty_json; }

    Json &JsonValue::at(size_t) { return null_object.empty_json; }

    Json &JsonValue::operator[](const string &) { return null_object.empty_json; }

    const Json &JsonValue::at(const string &) const { return null_object.empty_json; }

    Json &JsonValue::at(const string &) { return null_object.empty_json; }

    Json &JsonObject::operator[](const string &key) {
        return m_value[key];
    }

    const Json &JsonObject::at(const string &key) const {
        auto iter = m_value.find(key);
        return (iter == m_value.end()) ? null_object.empty_json : iter->second;
    }

    Json &JsonObject::at(const string &key) {
        auto iter = m_value.find(key);
        return (iter == m_value.end()) ? null_object.empty_json : iter->second;
    }

    const Json &JsonArray::operator[](size_t i) const {
        return m_value[i];
    }

    Json &JsonArray::operator[](size_t i) {
        return m_value[i];
    }

    const Json &JsonArray::at(size_t i) const {
        return i >= m_value.size() ? null_object.empty_json : m_value.at(i);
    }

    Json &JsonArray::at(size_t i) {
        return i >= m_value.size() ? null_object.empty_json : m_value.at(i);
    }

    Json &Json::operator=(const Json &json) {
        switch (json.type()) {
            case Type::ARRAY:
                m_ptr = make_unique<JsonArray>(json.array_items());
                break;
            case Type::BOOL:
                m_ptr = make_unique<JsonBoolean>(json.bool_value());
                break;
            case Type::NUMBER:
                m_ptr = make_unique<JsonDouble>(json.number_value());
                break;
            case Type::OBJECT:
                m_ptr = make_unique<JsonObject>(json.object_items());
                break;
            case Type::STRING:
                m_ptr = make_unique<JsonString>(json.string_value());
                break;
            case Type::NUL:
            default:
                m_ptr = make_unique<JsonNull>();
                break;
        }
        return *this;
    }

    Json &Json::operator=(Json &&json) noexcept {
        m_ptr = move(json.m_ptr);
        return *this;
    }

    bool Json::operator==(const Json &other) const {
        if (m_ptr == other.m_ptr) {
            return true;
        }
        if (m_ptr->type() != other.m_ptr->type()) {
            return false;
        }

        return m_ptr->equals(other.m_ptr.get());
    }

    bool Json::operator<(const Json &other) const {
        if (m_ptr == other.m_ptr) {
            return false;
        }
        if (m_ptr->type() != other.m_ptr->type()) {
            return m_ptr->type() < other.m_ptr->type();
        }

        return m_ptr->less(other.m_ptr.get());
    }

    inline string esc(char c) {
        char buf[12];
        if (static_cast<uint8_t>(c) >= 0x20 && static_cast<uint8_t>(c) <= 0x7f) {
            snprintf(buf, sizeof buf, "'%c' (%d)", c, c);
        } else {
            snprintf(buf, sizeof buf, "(%d)", c);
        }
        return buf;
    }

    inline bool in_range(long x, long lower, long upper) {
        return (x >= lower && x <= upper);
    }

    // parser
    class StringParser {
    public:
        StringParser(const string &content, string &error_msg,
                     const JsonParseType &parser_type = JsonParseType::STRING_STANDARD)
                : str(content), i(0), err(error_msg), failed(false), strategy(parser_type) {}

        /* parse_json()
         *
         * Parse a JSON object.
         */
        Json parse_json(int depth) {
            if (depth > max_depth) {
                return fail("exceeded maximum nesting depth");
            }

            char ch = get_next_token();
            if (failed) {
                return {};
            }

            if (ch == '-' || (ch >= '0' && ch <= '9')) {
                i--;
                return parse_number();
            }

            if (ch == 't') {
                return expect("true", true);
            }

            if (ch == 'f') {
                return expect("false", false);
            }

            if (ch == 'n') {
                return expect("null", Json());
            }

            if (ch == '"') {
                return parse_string();
            }

            if (ch == '{') {
                map<string, Json> data;
                ch = get_next_token();
                if (ch == '}')
                    return data;

                for (;;) {
                    if (ch != '"')
                        return fail("expected '\"' in object, got " + esc(ch));

                    string key = parse_string();
                    if (failed)
                        return {};

                    ch = get_next_token();
                    if (ch != ':')
                        return fail("expected ':' in object, got " + esc(ch));

                    data[std::move(key)] = parse_json(depth + 1);
                    if (failed)
                        return {};

                    ch = get_next_token();
                    if (ch == '}')
                        break;
                    if (ch != ',')
                        return fail("expected ',' in object, got " + esc(ch));

                    ch = get_next_token();
                }
                return data;
            }

            if (ch == '[') {
                vector<Json> data;
                ch = get_next_token();
                if (ch == ']')
                    return data;

                for (;;) {
                    i--;
                    data.push_back(parse_json(depth + 1));
                    if (failed)
                        return {};

                    ch = get_next_token();
                    if (ch == ']')
                        break;
                    if (ch != ',')
                        return fail("expected ',' in list, got " + esc(ch));

                    ch = get_next_token();
                    (void) ch;
                }
                return data;
            }

            return fail("expected value, got " + esc(ch));
        }

        friend Json Json::parse(const string &in, string &err, JsonParseType strategy);

        friend vector<Json> Json::parse_multi(const string &in,
                                              std::string::size_type &parser_stop_pos,
                                              string &err,
                                              JsonParseType strategy);

    private:
        /* State
         */
        const string &str;
        size_t i;
        string &err;
        bool failed;
        const JsonParseType strategy;

        /* fail(msg, err_ret = Json())
         *
         * Mark this parse as failed.
         */
        Json fail(string &&msg) {
            return fail(move(msg), Json());
        }

        template<typename T>
        T fail(string &&msg, T &&err_ret) {
            if (!failed) {
                err = std::move(msg);
            }
            failed = true;
            return err_ret;
        }

        /* consume_garbage()
         *
         * Advance until the current character is non-whitespace and non-comment.
         */
        void consume_garbage() {
            consume_whitespace();
            if (strategy == JsonParseType::STRING_COMMENTS) {
                bool comment_found = false;
                do {
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
        void consume_whitespace() {
            while (str[i] == ' ' || str[i] == '\r' || str[i] == '\n' || str[i] == '\t')
                i++;
        }

        /* consume_comment()
         *
         * Advance comments (c-style inline and multiline).
         */
        bool consume_comment() {
            bool comment_found = false;
            if (str[i] == '/') {
                i++;
                if (i == str.size())
                    return fail("unexpected end of input after start of comment", false);
                if (str[i] == '/') { // inline comment
                    i++;
                    // advance until next line, or end of input
                    while (i < str.size() && str[i] != '\n') {
                        i++;
                    }
                    comment_found = true;
                } else if (str[i] == '*') { // multiline comment
                    i++;
                    if (i > str.size() - 2)
                        return fail("unexpected end of input inside multi-line comment", false);
                    // advance until closing tokens
                    while (!(str[i] == '*' && str[i + 1] == '/')) {
                        i++;
                        if (i > str.size() - 2)
                            return fail(
                                    "unexpected end of input inside multi-line comment", false);
                    }
                    i += 2;
                    comment_found = true;
                } else
                    return fail("malformed comment", false);
            }
            return comment_found;
        }

        /* get_next_token()
         *
         * Return the next non-whitespace character. If the end of the input is reached,
         * flag an error and return 0.
         */
        char get_next_token() {
            consume_garbage();
            if (failed)
                return (char) 0;
            if (i == str.size())
                return fail("unexpected end of input", (char) 0);

            return str[i++];
        }

        /* encode_utf8(pt, out)
         *
         * Encode pt as UTF-8 and add it to out.
         */
        static void encode_utf8(long pt, string &out) {
            if (pt < 0)
                return;

            if (pt < 0x80) {
                out += static_cast<char>(pt);
            } else if (pt < 0x800) {
                out += static_cast<char>((pt >> 6) | 0xC0);
                out += static_cast<char>((pt & 0x3F) | 0x80);
            } else if (pt < 0x10000) {
                out += static_cast<char>((pt >> 12) | 0xE0);
                out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
                out += static_cast<char>((pt & 0x3F) | 0x80);
            } else {
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
        string parse_string() {
            string out;
            long last_escaped_codepoint = -1;
            while (true) {
                if (i == str.size())
                    return fail("unexpected end of input in string", "");

                char ch = str[i++];

                if (ch == '"') {
                    encode_utf8(last_escaped_codepoint, out);
                    return out;
                }

                if (in_range(ch, 0, 0x1f))
                    return fail("unescaped " + esc(ch) + " in string", "");

                // The usual case: non-escaped characters
                if (ch != '\\') {
                    encode_utf8(last_escaped_codepoint, out);
                    last_escaped_codepoint = -1;
                    out += ch;
                    continue;
                }

                // Handle escapes
                if (i == str.size())
                    return fail("unexpected end of input in string", "");

                ch = str[i++];

                if (ch == 'u') {
                    // Extract 4-byte escape sequence
                    string esc = str.substr(i, 4);
                    // Explicitly check length of the substring. The following loop
                    // relies on std::string returning the terminating NUL when
                    // accessing str[length]. Checking here reduces brittleness.
                    if (esc.length() < 4) {
                        return fail("bad \\u escape: " + esc, "");
                    }
                    for (size_t j = 0; j < 4; j++) {
                        if (!in_range(esc[j], 'a', 'f') && !in_range(esc[j], 'A', 'F') && !in_range(esc[j], '0', '9'))
                            return fail("bad \\u escape: " + esc, "");
                    }

                    long codepoint = strtol(esc.data(), nullptr, 16);

                    // JSON specifies that characters outside the BMP shall be encoded as a pair
                    // of 4-hex-digit \u escapes encoding their surrogate pair components. Check
                    // whether we're in the middle of such a beast: the previous codepoint was an
                    // escaped lead (high) surrogate, and this is a trail (low) surrogate.
                    if (in_range(last_escaped_codepoint, 0xD800, 0xDBFF) && in_range(codepoint, 0xDC00, 0xDFFF)) {
                        // Reassemble the two surrogate pairs into one astral-plane character, per
                        // the UTF-16 algorithm.
                        encode_utf8((((last_escaped_codepoint - 0xD800) << 10) | (codepoint - 0xDC00)) + 0x10000, out);
                        last_escaped_codepoint = -1;
                    } else {
                        encode_utf8(last_escaped_codepoint, out);
                        last_escaped_codepoint = codepoint;
                    }

                    i += 4;
                    continue;
                }

                encode_utf8(last_escaped_codepoint, out);
                last_escaped_codepoint = -1;

                if (ch == 'b') {
                    out += '\b';
                } else if (ch == 'f') {
                    out += '\f';
                } else if (ch == 'n') {
                    out += '\n';
                } else if (ch == 'r') {
                    out += '\r';
                } else if (ch == 't') {
                    out += '\t';
                } else if (ch == '"' || ch == '\\' || ch == '/') {
                    out += ch;
                } else {
                    return fail("invalid escape character " + esc(ch), "");
                }
            }
        }

        /* parse_number()
         *
         * Parse a double.
         */
        Json parse_number() {
            size_t start_pos = i;

            if (str[i] == '-')
                i++;

            // Integer part
            if (str[i] == '0') {
                i++;
                if (in_range(str[i], '0', '9'))
                    return fail("leading 0s not permitted in numbers");
            } else if (in_range(str[i], '1', '9')) {
                i++;
                while (in_range(str[i], '0', '9'))
                    i++;
            } else {
                return fail("invalid " + esc(str[i]) + " in number");
            }

            if (str[i] != '.' && str[i] != 'e' && str[i] != 'E' &&
                (i - start_pos) <= static_cast<size_t>(std::numeric_limits<int>::digits10)) {
                return std::atoi(str.c_str() + start_pos);
            }

            // Decimal part
            if (str[i] == '.') {
                i++;
                if (!in_range(str[i], '0', '9'))
                    return fail("at least one digit required in fractional part");

                while (in_range(str[i], '0', '9'))
                    i++;
            }

            // Exponent part
            if (str[i] == 'e' || str[i] == 'E') {
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
        Json expect(const string &expected, Json res) {
            assert(i != 0);
            i--;
            if (str.compare(i, expected.length(), expected) == 0) {
                i += expected.length();
                return res;
            } else {
                return fail("parse error: expected " + expected + ", got " + str.substr(i, expected.length()));
            }
        }
    };

    class CborParser {
    public:
        CborParser(const string &in, string &err_msg)
                : status(DecodeStatus::START), curidx(0), curlen(0), instant_num(0), str(in), err(err_msg) {
        }

        Json parser_json(int depth) {
            if (depth > max_depth) {
                return fail("exceeded maximum nesting depth");
            }
            switch (status) {
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

        friend Json Json::parse(const string &in, string &err, JsonParseType strategy);

        friend vector<Json> Json::parse_multi(const string &in,
                                              std::string::size_type &parser_stop_pos,
                                              string &err,
                                              JsonParseType strategy);

    private:
        enum DecodeStatus {
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
        const string &str;
        string &err;

    private:
        Json fail(string &&msg) {
            return fail(move(msg), Json());
        }

        template<typename T>
        T fail(string &&msg, T &&err_ret) {
            if (status != DecodeStatus::ERROR) {
                err = std::move(msg);
            }
            status = DecodeStatus::ERROR;
            return err_ret;
        }

        bool has_bytes(size_t n) {
            return curidx + n <= str.size();
        }

        unsigned char get_byte() {
            return str.at(curidx++);
        }

        unsigned short get_uint16() {
            unsigned short value =
                    static_cast<unsigned short>(get_byte()) << 8 | static_cast<unsigned short>(get_byte());
            return value;
        }

        unsigned int get_uint32() {
            unsigned int value = (static_cast<unsigned int>(get_byte()) << 24) |
                                 (static_cast<unsigned int>(get_byte()) << 16) |
                                 (static_cast<unsigned int>(get_byte()) << 8) |
                                 (static_cast<unsigned int>(get_byte()));
            return value;
        }

        unsigned long long get_uint64() {
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

        float get_float() {
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

        double get_double() {
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
        void parser_start() {
            if (!has_bytes(1)) {
                fail("not enough length!");
            }
            auto type = get_byte();
            auto major_type = type >> 5;
            auto minor_type = type & 0x1f;
            switch (major_type) {
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

        void parser_type() {
            if (!has_bytes(1)) {
                fail("not enough length!");
                return;
            }
            auto type = get_byte();
            auto major_type = type >> 5;
            auto minor_type = type & 0x1f;
            switch (major_type) {
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
                    if (minor_type == 0x14 || minor_type == 0x15) {
                        status = DecodeStatus::BOOL;
                        instant_num = minor_type;
                    } else if (minor_type == 0x16) {
                        status = DecodeStatus::NIL;
                        instant_num = minor_type;
                    } else if (minor_type == 0x18) {
                        status = DecodeStatus::SPECIAL;
                        curlen = 1;
                    } else if (minor_type == 0x19) { // 2 byte
                        curlen = 2;
                        status = DecodeStatus::SPECIAL;
                    } else if (minor_type == 0x1A) { // 4 byte
                        curlen = 4;
                        status = DecodeStatus::FLOAT;
                    } else if (minor_type == 0x1B) { // 8 byte
                        curlen = 8;
                        status = DecodeStatus::DOUBLE;
                    } else {
                        fail("invalid special type", "");
                    }
                    break;
                default:
                    fail("unknow major_type", "");
                    break;
            }
        }

        void parser_string_size(DecodeStatus st) {
            if (!has_bytes(curlen)) {
                fail("not enough length!", "");
                return;
            }
            status = st;
            switch (curlen) {
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

        void parser_container_size(int depth, DecodeStatus st) {
            if (!has_bytes(curlen)) {
                fail("not enough length!", "");
                return;
            }
            switch (curlen) {
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
                    if (temp <= std::numeric_limits<int>::max()) {
                        instant_num = static_cast<int>(temp);
                    } else {
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

        int parser_pint() {
            if (!has_bytes(curlen)) {
                return fail("not enough length!", 0);
            }
            int result = 0;
            switch (curlen) {
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
                    if (temp <= std::numeric_limits<int>::max()) {
                        result = static_cast<int>(temp);
                    } else {
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

        int parser_nint() {
            if (!has_bytes(curlen)) {
                return fail("not enough length!", 0);
            }
            int result = 0;
            switch (curlen) {
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
                    if (temp <= std::numeric_limits<int>::max()) {
                        result = -static_cast<int>(temp) - 1;
                    } else {
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

        float parser_float() {
            if (!has_bytes(curlen)) {
                return fail("not enough length!", 0.0f);
            }
            status = DecodeStatus::TYPE;
            return get_float();
        }

        double parser_double() {
            if (!has_bytes(curlen)) {
                return fail("not enough length!", 0.0);
            }
            status = DecodeStatus::TYPE;
            return get_double();
        }

        string parser_string() {
            if (!has_bytes(curlen)) {
                return fail("not enough length!", "");
            }
            status = DecodeStatus::TYPE;
            auto slice = str.substr(curidx, curlen);
            curidx += curlen;
            return slice;
        }

        std::map<string, Json> parser_object(int depth) {
            status = DecodeStatus::TYPE;
            map<string, Json> data;
            auto len = instant_num;
            for (auto i = 0u; i < len; ++i) {
                auto key = parser_json(depth + 1);
                if (key.type() != Json::STRING) {
                    fail("Object key value is not string: " + key.stringify());
                    break;
                }
                data.insert(std::make_pair(key.string_value(), parser_json(depth + 1)));
            }
            return data;
        }

        std::vector<Json> parser_array(int depth) {
            status = DecodeStatus::TYPE;
            vector<Json> data;
            data.reserve(instant_num);
            auto len = instant_num;
            for (auto i = 0u; i < len; ++i) {
                data.push_back(parser_json(depth + 1));
            }
            return data;
        }

        void decompose_type(
                int minor_type, int instant_number, DecodeStatus set_status, size_t setlen = 0) {
            if (minor_type <= 0x17) {
                curlen = setlen;
                instant_num = instant_number;
                status = set_status;
            } else if (minor_type == 0x18) { // 1 byte
                curlen = 1;
            } else if (minor_type == 0x19) { // 2 byte
                curlen = 2;
            } else if (minor_type == 0x1A) { // 4 byte
                curlen = 4;
            } else if (minor_type == 0x1B) { // 8 byte
                curlen = 8;
            } else {
                fail("invalid minor type", "");
            }
        }
    };

    Json Json::parse(const string &in, string &err, JsonParseType strategy) {
        if (strategy == JsonParseType::BINARY_STANDARD) {
            CborParser parser(in, err);
            Json result = parser.parser_json(0);
            if (parser.status == CborParser::DecodeStatus::ERROR) {
                return {};
            }
            if (parser.curidx != in.size()) {
                return parser.fail("unexpected trailing");
            }
            return result;
        } else {
            StringParser parser(in, err, strategy);
            Json result = parser.parse_json(0);

            // Check for any trailing garbage
            parser.consume_garbage();
            if (parser.failed) {
                return {};
            }
            if (parser.i != in.size()) {
                return parser.fail("unexpected trailing " + esc(in[parser.i]));
            }
            return result;
        }
    }

    vector<Json> Json::parse_multi(const string &in,
                                   std::string::size_type &parser_stop_pos,
                                   string &err,
                                   JsonParseType strategy) {
        parser_stop_pos = 0;
        vector<Json> json_vec;
        if (strategy == JsonParseType::BINARY_STANDARD) {
            CborParser parser(in, err);
            while (parser.curidx != in.size() && err.empty()) {
                json_vec.push_back(parser.parser_json(0));
                if (!err.empty()) {
                    break;
                }
                parser_stop_pos = parser.curidx;
            }
        } else {
            StringParser parser(in, err, strategy);
            while (parser.i != in.size() && !parser.failed) {
                json_vec.push_back(parser.parse_json(0));
                if (parser.failed) {
                    break;
                }

                // Check for another object
                parser.consume_garbage();
                if (parser.failed) {
                    break;
                }
                parser_stop_pos = parser.i;
            }
        }
        return json_vec;
    }

    bool Json::has_shape(const shape &types, string &err) const {
        if (!is_object()) {
            err = "expected JSON object, got " + stringify();
            return false;
        }

        for (auto &item: types) {
            if ((*this).at(item.first).type() != item.second) {
                err = "bad type for " + item.first + " in " + stringify();
                return false;
            }
        }

        return true;
    }
} // namespace json