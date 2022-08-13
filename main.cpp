#include <cassert>
#include <string>
#include <cstring>
#include <iostream>
#include <fstream>
#include <list>
#include <map>
#include <deque>
#include <array>
#include <stack>
#include <forward_list>
#include <unordered_map>
#include <algorithm>
#include <type_traits>
#include <cmath>
#include <set>

#define TEST_ASSERT(b) assert(b)
#define EXPECT_STRING_EQUAL(a, b) assert(strcmp(a, b) == 0)
#define EXPECT_FLOAT_EQUAL(a, b) assert(fabs(a - b) < 1.0E-5)
#define EXPECT_EQ(a, b) assert(a == b)
#define EXPECT_CBOREQ(a, b) assert(strcmp(HexString(a).c_str(), b) == 0)
#define PREPARE_TEST cbs.ClearData();

#include "json.h"
#include "codec.h"

struct test_encoder_stream_io
{
    int a;
    int b;
    double c;
    std::string d;

    test_encoder_stream_io(int _a, int _b, double _c, std::string _d) : a(_a), b(_b), c(_c), d(_d) {}

    friend std::ostream &operator<<(std::ostream &os, const test_encoder_stream_io &ss)
    {
        os << ss.a << ss.b << ss.c << ss.d;
        return os;
    };
};

namespace TestEnum
{
    enum WeakEnum
    {
        A,
        B,
        END
    };

    enum class StrongEnum : int16_t
    {
        A,
        B
    };

    struct Foo
    {
        enum class NestedEnum
        {
            A,
            B,
            END
        };
    };
}

namespace TestReflect
{
    enum class box_type
    {
        ms,
        mr,
        co,
        none
    };

    class ReflectA
    {
    public:
        int a;
        box_type enn;
    };

    class ReflectB
    {
    public:
        ReflectA b;
    };

    class ReflectC
    {
    public:
        ReflectB c;
    };

    struct Point
    {
        int x, y;
        std::array<double, 5> dd;
        std::string str;
    };

    struct Rect
    {
        Point specialPoint;
        std::vector<Point> plist;
        uint32_t color;
    };

    class Box
    {
    public:
        Box() = default;
        virtual ~Box() = default;

    public:
        std::map<std::string, Rect> faces;
        box_type Boxtype{};
    };

} // namespace TestReflect

ENUM_STRINGS(TestReflect::box_type, "ms", "mr", "co", "none")

REFLECT(TestReflect::ReflectA, FIELD(TestReflect::ReflectA, a), FIELD(TestReflect::ReflectA, enn))
REFLECT(TestReflect::ReflectB, FIELD(TestReflect::ReflectB, b))
REFLECT(TestReflect::ReflectC, FIELD(TestReflect::ReflectC, c))

REFLECT(TestReflect::Point, FIELD(TestReflect::Point, x), FIELD(TestReflect::Point, y), FIELD(TestReflect::Point, dd),
        FIELD(TestReflect::Point, str))
REFLECT(TestReflect::Rect, FIELD(TestReflect::Rect, specialPoint), FIELD(TestReflect::Rect, plist),
        FIELD(TestReflect::Rect, color))
REFLECT(TestReflect::Box, FIELD(TestReflect::Box, faces), FIELD(TestReflect::Box, Boxtype))
ENUM_STRINGS(TestEnum::Foo::NestedEnum, "fa", "fb")
ENUM_STRINGS(TestEnum::StrongEnum, "sa", "sb")
ENUM_STRINGS(TestEnum::WeakEnum, "wa", "wb")

using namespace serialization;
using std::string;
using bytesArray = std::vector<uint8_t>;

#define CHECK_TRAIT(x) static_assert(x::value, #x)
CHECK_TRAIT(std::is_nothrow_constructible<Json>);
CHECK_TRAIT(std::is_nothrow_default_constructible<Json>);
CHECK_TRAIT(std::is_copy_constructible<Json>);
CHECK_TRAIT(std::is_nothrow_move_constructible<Json>);
CHECK_TRAIT(std::is_copy_assignable<Json>);
CHECK_TRAIT(std::is_nothrow_move_assignable<Json>);
CHECK_TRAIT(std::is_nothrow_destructible<Json>);

static_assert(serialization::is_stl_array_like<std::array<Json, 5>>::value, "serialization::is_stl_array_like<std::array<Json,5>>");
CHECK_TRAIT(!serialization::is_stl_array_like<Json[]>);
CHECK_TRAIT(serialization::is_stl_array_like<std::vector<Json>>);
CHECK_TRAIT(serialization::is_stl_array_like<std::list<Json>>);
CHECK_TRAIT(serialization::is_stl_array_like<std::deque<Json>>);
CHECK_TRAIT(serialization::is_stl_array_like<std::forward_list<Json>>);
CHECK_TRAIT(!serialization::is_stl_array_like<std::stack<Json>>);
static_assert(serialization::is_stl_map_like<std::map<Json, Json>>::value, "serialization::is_stl_map_like<std::map<Json,Json>>");

inline string HexString(const string &str)
{
    std::string res;
    char elemStr[4];
    unsigned char uCElem;
    for (size_t ii = 0; ii < str.size(); ii++)
    {
        uCElem = str.at(ii);
        sprintf(elemStr, "%02x ", uCElem);
        res.append(elemStr);
    }
    return res;
}

void jsonp_test()
{
    const string simple_test =
        R"({"k1":"v1", "k2":42, "k3":["a",123,true,false,null]})";

    string err;
    const auto json = Json::parse(simple_test, err);

    std::cout << "k1: " << json.at("k1").string_value() << "\n";
    std::cout << "k3: " << json.at("k3").stringify() << "\n";

    for (auto &k : json.at("k3").array_items())
    {
        std::cout << "    - " << k.stringify() << "\n";
    }

    string comment_test = R"({
      // comment /* with nested comment */
      "a": 1,
      // comment
      // continued
      "b": "text",
      /* multi
         line
         comment
        // line-comment-inside-multiline-comment
      */
      // and single-line comment
      // and single-line comment /* multiline inside single line */
      "c": [1, 2, 3]
      // and single-line comment at end of object
    })";

    string err_comment;
    auto json_comment = Json::parse(
        comment_test, err_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(!json_comment.is_null());
    TEST_ASSERT(err_comment.empty());
    TEST_ASSERT(json_comment["c"][1].int_value() == 2);

    comment_test = "{\"a\": 1}//trailing line comment";
    json_comment = Json::parse(
        comment_test, err_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(!json_comment.is_null());
    TEST_ASSERT(err_comment.empty());

    comment_test = "{\"a\": 1}/*trailing multi-line comment*/";
    json_comment = Json::parse(
        comment_test, err_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(!json_comment.is_null());
    TEST_ASSERT(err_comment.empty());

    string failing_comment_test = "{\n/* unterminated comment\n\"a\": 1,\n}";
    string err_failing_comment;
    auto json_failing_comment = Json::parse(
        failing_comment_test, err_failing_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(json_failing_comment.is_null());
    TEST_ASSERT(!err_failing_comment.empty());

    failing_comment_test = "{\n/* unterminated trailing comment }";
    json_failing_comment = Json::parse(
        failing_comment_test, err_failing_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(json_failing_comment.is_null());
    TEST_ASSERT(!err_failing_comment.empty());

    failing_comment_test = "{\n/ / bad comment }";
    json_failing_comment = Json::parse(
        failing_comment_test, err_failing_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(json_failing_comment.is_null());
    TEST_ASSERT(!err_failing_comment.empty());

    failing_comment_test = "{// bad comment }";
    json_failing_comment = Json::parse(
        failing_comment_test, err_failing_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(json_failing_comment.is_null());
    TEST_ASSERT(!err_failing_comment.empty());

    failing_comment_test = "{\n\"a\": 1\n}/";
    json_failing_comment = Json::parse(
        failing_comment_test, err_failing_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(json_failing_comment.is_null());
    TEST_ASSERT(!err_failing_comment.empty());

    failing_comment_test = "{/* bad\ncomment *}";
    json_failing_comment = Json::parse(
        failing_comment_test, err_failing_comment, JsonFormat::STRING_COMMENTS);
    TEST_ASSERT(json_failing_comment.is_null());
    TEST_ASSERT(!err_failing_comment.empty());

    std::list<int> l1{1, 2, 3};
    std::vector<int> l2{1, 2, 3};
    std::set<int> l3{1, 2, 3};
    TEST_ASSERT(Json(l1) == Json(l2));
    TEST_ASSERT(Json(l2) == Json(l3));

    std::map<string, string> m1{{"k1", "v1"}, {"k2", "v2"}};
    std::unordered_map<string, string> m2{{"k1", "v1"}, {"k2", "v2"}};
    TEST_ASSERT(Json(m1) == Json(m2));

    // Json literals
    const Json obj = Json::object({
        {"k1", "v1"},
        {"k2", 42.0},
        {"k3", Json::array({"a", 123.0, true, false, nullptr})},
    });

    std::cout << "obj: " << obj.stringify() << "\n";
    TEST_ASSERT(obj.stringify() == "{\"k1\": \"v1\", \"k2\": 42, \"k3\": [\"a\", 123, true, false, null]}");

    TEST_ASSERT(Json("a").number_value() == 0);
    TEST_ASSERT(Json("a").string_value() == "a");
    TEST_ASSERT(Json().number_value() == 0);

    TEST_ASSERT(obj == json);
    TEST_ASSERT(Json(42) == Json(42.0));
    TEST_ASSERT(Json(42) != Json(42.1));

    const string unicode_escape_test =
        R"([ "blah\ud83d\udca9blah\ud83dblah\udca9blah\u0000blah\u1234" ])";

    const char utf8[] = "blah"
                        "\xf0\x9f\x92\xa9"
                        "blah"
                        "\xed\xa0\xbd"
                        "blah"
                        "\xed\xb2\xa9"
                        "blah"
                        "\0"
                        "blah"
                        "\xe1\x88\xb4";

    Json uni = Json::parse(unicode_escape_test, err);
    TEST_ASSERT(uni[0].string_value().size() == (sizeof utf8) - 1);
    TEST_ASSERT(std::memcmp(uni[0].string_value().data(), utf8, sizeof utf8) == 0);

    {
        const std::string good_json = R"( {"k1" : "v1"})";
        const std::string bad_json1 = good_json + " {";
        const std::string bad_json2 = good_json + R"({"k2":"v2", "k3":[)";
        struct TestMultiParse
        {
            std::string input;
            std::string::size_type expect_parser_stop_pos;
            size_t expect_not_empty_elms_count;
            Json expect_parse_res;
        } tests[] = {
            {" {", 0, 0, {}},
            {good_json, good_json.size(), 1, Json(std::map<string, string>{{"k1", "v1"}})},
            {bad_json1, good_json.size() + 1, 1, Json(std::map<string, string>{{"k1", "v1"}})},
            {bad_json2, good_json.size(), 1, Json(std::map<string, string>{{"k1", "v1"}})},
            {"{}", 2, 1, Json::object{}},
        };
        for (const auto &tst : tests)
        {
            std::string::size_type parser_stop_pos;
            std::string errmsg;
            auto res = Json::parse_multi(tst.input, parser_stop_pos, errmsg);
            TEST_ASSERT(parser_stop_pos == tst.expect_parser_stop_pos);
            TEST_ASSERT(
                (size_t)std::count_if(res.begin(), res.end(),
                                      [](const Json &j)
                                      { return !j.is_null(); }) == tst.expect_not_empty_elms_count);
            if (!res.empty())
            {
                TEST_ASSERT(tst.expect_parse_res == res[0]);
            }
        }
    }

    Json my_json = Json::object{
        {"key1", "value1"},
        {"key2", false},
        {"key3", Json::array{1, 2, 3}},
    };
    std::string json_obj_str = my_json.stringify();
    std::cout << "json_obj_str: " << json_obj_str << "\n";
    TEST_ASSERT(json_obj_str == "{\"key1\": \"value1\", \"key2\": false, \"key3\": [1, 2, 3]}");

    class Point
    {
    public:
        int x;
        int y;
        Point(int x, int y) : x(x), y(y) {}
        Json to_json() const { return Json::array{x, y}; }
    };

    std::vector<Point> points = {{1, 2}, {10, 20}, {100, 200}};
    std::string points_json = Json(points).stringify();
    std::cout << "points_json: " << points_json << "\n";
    TEST_ASSERT(points_json == "[[1, 2], [10, 20], [100, 200]]");

    const char *multi_json_objs = R"(
   {
      "Xvalue": 1,
      "Var1": 1,
      "Var2": 2
   }
   {
      "Xvalue": 2,
      "Var1": 1,
      "Var2": 2
   }
   {
      "Xvalue": 3,
      "Var1": 1,
      "Var2": 2
   }
   {
      "Xvalue": 4,
      "Var1": 1,
      "Var2": 2
   }
   {
      "Xvalue": 5,
      "Var1": 1,
      "Var2": 2
   }
   {
      "Xvalue": 6,
      "Var1": 1,
      "Var2": 2
   }
   {
      "Xvalue": 17,
      "Var1": 1,
      "Var2": 2
   })";
    std::string err_msg;
    auto res = Json::parse_multi(multi_json_objs, err_msg);
    TEST_ASSERT(err_msg.empty());
    std::cout << "Multi objects:" << std::endl;
    for (auto &i : res)
    {
        std::cout << "Before:" << i.stringify() << std::endl;
        i["Var1"] = 4;
        i["Var2"] = Json(Json::array{1, 2, 3});
        TEST_ASSERT(i["Var1"].int_value() == 4);
        std::cout << "After:" << i.stringify() << std::endl;
    }
}

void cborp_test()
{
    auto bytes2string = [](const bytesArray &bytes) -> string
    {
        return {(char *)bytes.data(), bytes.size()};
    };

    string err;
    auto array_string = bytes2string({0x80});
    auto j = Json::parse(array_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::ARRAY);
    TEST_ASSERT(j.array_items().empty());

    array_string = bytes2string({0x81, 0xf6});
    j = Json::parse(array_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::ARRAY);
    TEST_ASSERT(j[0].is_null());

    array_string = bytes2string({0x85, 0x01, 0x02, 0x03, 0x04, 0x05});
    j = Json::parse(array_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::ARRAY);
    EXPECT_STRING_EQUAL(j.stringify().c_str(), "[1, 2, 3, 4, 5]");

    array_string = bytes2string({0x81, 0x81, 0x81, 0x80});
    j = Json::parse(array_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::ARRAY);
    EXPECT_STRING_EQUAL(j.stringify().c_str(), "[[[[]]]]");

    array_string = bytes2string({0x82, 0x67, 0x73, 0x74, 0x72, 0x69, 0x6E, 0x67, 0x41, 0x67, 0x73, 0x74, 0x72, 0x69, 0x6E, 0x67, 0x42});
    j = Json::parse(array_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::ARRAY);
    EXPECT_STRING_EQUAL(j.stringify().c_str(), R"(["stringA", "stringB"])");

    array_string = bytes2string({0x83, 0xFB, 0x40, 0x11, 0xD7, 0x0A, 0x3D, 0x70, 0xA3, 0xD7, 0xFB, 0xC1, 0x2E, 0x84, 0x7F, 0xCC, 0xCC, 0xCC, 0xCD, 0xFB, 0x3E, 0xB4, 0xB3, 0xFD, 0x59, 0x42, 0xCD, 0x96});
    j = Json::parse(array_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::ARRAY);
    EXPECT_FLOAT_EQUAL(j[0].number_value(), 4.46);
    EXPECT_FLOAT_EQUAL(j[1].number_value(), -999999.9);
    EXPECT_FLOAT_EQUAL(j[2].number_value(), 0.000001234);

    auto object_string = bytes2string({0xa0});
    j = Json::parse(object_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::OBJECT);
    TEST_ASSERT(j.object_items().empty());

    object_string = bytes2string({0xa1, 0x61, 0x61, 0xa1, 0x61, 0x62, 0xa1, 0x61, 0x63, 0xa0});
    j = Json::parse(object_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::OBJECT);
    EXPECT_STRING_EQUAL(j.stringify().c_str(), R"({"a": {"b": {"c": {}}}})");

    object_string = bytes2string({0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x82, 0x02, 0x03});
    j = Json::parse(object_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::OBJECT);
    EXPECT_STRING_EQUAL(j.stringify().c_str(), "{\"a\": 1, \"b\": [2, 3]}");

    object_string = bytes2string({0xa5, 0x61, 0x61, 0x61, 0x41, 0x61, 0x62, 0x61, 0x42, 0x61, 0x63, 0x61, 0x43, 0x61, 0x64, 0x61, 0x44, 0x61, 0x65, 0x61, 0x45});
    j = Json::parse(object_string, err, JsonFormat::BINARY_STANDARD);
    TEST_ASSERT(err.empty());
    TEST_ASSERT(j.type() == Json::OBJECT);
    EXPECT_STRING_EQUAL(j.stringify().c_str(), "{\"a\": \"A\", \"b\": \"B\", \"c\": \"C\", \"d\": \"D\", \"e\": \"E\"}");
}

void combo_test()
{
    for (const std::string &filename : {"1.json", "2.json", "3.json", "4.json", "5.json", "6.json"})
    {
        std::ifstream ifs(std::string("../../json.org/") + filename);
        string err;
        string str((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
        auto j_string = Json::parse(str, err);
        TEST_ASSERT(err.empty());
        ifs.close();
        ifs.open(std::string("../../json.org/") + filename + ".cbor", std::ios_base::binary);
        string str2((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
        auto j_binary = Json::parse(str2, err, JsonFormat::BINARY_STANDARD);
        TEST_ASSERT(err.empty());
        TEST_ASSERT(j_string == j_binary);
        std::cout << j_binary.stringify() << std::endl;
    }
}

void codec_test()
{
    CborStream cbs;
    PREPARE_TEST
    {
        cbs << true << false;
        EXPECT_CBOREQ(cbs.GetData().c_str(), "f5 f4 ");
    }
    PREPARE_TEST
    {
        for (short i : {2, 14, 25, 56, 241, -21, -124, -5, -5116, -24901})
        {
            cbs << i;
        }
        EXPECT_CBOREQ(cbs.GetData(), "02 0e 18 19 18 38 18 f1 34 38 7b 24 39 13 fb 39 61 44 ");
        // 0x02,0x0e,0x1819,0x1838,0x18f1,0x34,0x387B,0x24,0x3913FB,0x396144
    }

    PREPARE_TEST
    {

        for (auto i : {100, 1000, 10000, 100000, -100, -100000, -87923000})
        {
            cbs << i;
        }
        EXPECT_CBOREQ(cbs.GetData(),
                      "18 64 19 03 e8 19 27 10 1a 00 01 86 a0 38 63 3a 00 01 86 9f 3a 05 3d 99 37 ");
        // 0x1864,0x193e8,0x192710,0x1a0186a0,0x3863,0x3A0001869F,0x3A053D9937
    }

    PREPARE_TEST
    {
        std::vector<int64_t> vec = {3000000000, 452384728947, 17515481548154, 435678399658346583, -274632784628453285};
        for (auto i : vec)
        {
            cbs << i;
        }
        EXPECT_CBOREQ(cbs.GetData(),
                      "1a b2 d0 5e 00 1b 00 00 00 69 54 3b 27 73 1b 00 00 0f ee 24 0e 45 7a 1b "
                      "06 0b d7 32 37 f2 48 57 3b 03 cf b1 10 03 74 8f a4 ");
        // 0x1AB2D05E00,0x1B00000069543B2773,0x1B00000FEE240E457A,0x1B060BD73237F24857,0x3B03CFB11003748FA4
    }

    PREPARE_TEST
    {

        for (auto i : {0.0754f, 34.12f, 7.986f, -46583.46f, -2742.85f})
        {
            cbs << i;
        }
        EXPECT_CBOREQ(cbs.GetData(),
                      "fa 3d 9a 6b 51 fa 42 08 7a e1 fa 40 ff 8d 50 fa c7 35 f7 76 fa c5 2b 6d 9a ");
        // 0xfa3d9a6b51,0xFA42087AE1,0xFA40FF8D50,0xFAC735F776,0xFAC52B6D9A
    }

    PREPARE_TEST
    {
        for (auto i : {0.000754, 34.12, 7.98646471, 4356783996583.46583, -27463278462.8453285})
        {
            cbs << i;
        }
        EXPECT_CBOREQ(cbs.GetData(),
                      "fb 3f 48 b5 02 ab ab ea d5 fb 40 41 0f 5c 28 f5 c2 8f fb 40 1f f2 23 ce 10 6e b8 fb "
                      "42 8f b3 24 7f f5 3b ba fb c2 19 93 c1 7d fb 61 9e ");
        // 0xFB3F48B502ABABEAD5,0xFB40410F5C28F5C28F,0xFB401FF223CE106EB8,0xFB428FB3247FF53BBA,0xFBC21993C17DFB619E
    }

    PREPARE_TEST
    {
        for (std::string i : {"0.000754", "3ad4f12", "bhdsf", "0xashdgox", ""})
        {
            cbs << i;
        }
        cbs << std::string("lvaue");
        EXPECT_CBOREQ(cbs.GetData(),
                      "68 30 2e 30 30 30 37 35 34 67 33 61 64 34 66 31 32 65 62 68 64 73 66 69 30 78 61 73 68 64 "
                      "67 6f 78 60 65 6c 76 61 75 65 ");
        // 0x68302E303030373534
        // 0x6733616434663132
        // 0x656268647366
        // 0x69307861736864676F78
        // 0x60
        // 0x656c76617565
    }

    PREPARE_TEST
    {
        std::vector<unsigned char> tp{0x80, 0x81, 0x82, 0x83, 0xFF};
        cbs << tp;
        EXPECT_CBOREQ(cbs.GetData(), "45 80 81 82 83 ff ");
    }

    PREPARE_TEST
    {
        for (auto i : {"0.000754", "3ad4f12", "bhdsf", "0xashdgox", ""})
        {
            cbs << i;
        }
        char p[10] = "werttt";
        cbs << p;
        std::string str("ceshisdf");
        cbs << str << "lvaue";
        EXPECT_CBOREQ(cbs.GetData(),
                      "68 30 2e 30 30 30 37 35 34 67 33 61 64 34 66 31 32 65 62 68 64 73 66 69 30 78 61 73 68 64 "
                      "67 6f 78 60 66 77 65 72 74 74 74 68 63 65 73 68 69 73 64 66 65 6c 76 61 75 65 ");
    }

    PREPARE_TEST
    {
        std::array<int, 5> ls0{1, 2, 3, 4, 5};
        std::vector<int> ls1 = {1, 2, 3, 4, 5};
        cbs << ls0 << ls1;
        // 0x850102030405
        cbs << std::list<double>(3, 5.056);
        // 0x83FB4014395810624DD3FB4014395810624DD3FB4014395810624DD3
        std::deque<std::string> qu = {"cehi", "32846de", "queudbvf", "%^45243**&/n"};
        cbs << qu;
        // 0x84646365686967333238343664656871756575646276666C255E34353234332A2A262F6E
        cbs << std::vector<unsigned char>{'a', 'b', 'c', 'd'};
        // 0x844161416241634164
        EXPECT_CBOREQ(cbs.GetData(),
                      "85 01 02 03 04 05 85 01 02 03 04 05 83 fb 40 14 39 58 10 62 4d d3 fb 40 14 39 58 10 62 4d d3 fb 40 14 39 58 "
                      "10 62 4d d3 84 64 63 65 68 69 67 33 32 38 34 36 64 65 68 71 75 65 75 64 62 76 66 6c 25 5e 34 35 32 34 33 2a "
                      "2a 26 2f 6e 44 61 62 63 64 ");
    }

    PREPARE_TEST
    {
        std::map<int, int> mp1 = {{1, 2}, {2, 2}, {3, 56}};
        // 0xA301020202031838
        std::unordered_map<std::string, std::string> mp2 = {{"key", "value"}, {"jkfdh", "vfd"}, {"c876rw%^", ""}};
        const std::unordered_map<std::string, std::string> &pp = mp2;
        // rd order ,need https://cbor.me/
        std::vector<int> a1 = {782736, 123, -343242};
        std::vector<int> a2 = {97969, -23424, -12361};
        std::vector<int> a3 = {1212, -989, 0};
        std::map<std::string, std::vector<int>> mp3;
        mp3.insert(std::make_pair("test1", a1));
        mp3.insert(std::make_pair("test2", a2));
        mp3.insert(std::make_pair("test3", a3));
        cbs << mp1 << mp3;
        EXPECT_CBOREQ(cbs.GetData(),
                      "a3 01 02 02 02 03 18 38 a3 65 74 65 73 74 31 83 1a 00 0b f1 90 18 7b 3a 00 05 3c c9 65 74 65 73 74 32 83 1a "
                      "00 01 7e b1 39 5b 7f 39 30 48 65 74 65 73 74 33 83 19 04 bc 39 03 dc 00 ");
        cbs << pp;
    }

    PREPARE_TEST
    {
        cbs << "[1,[1,2]]"
            << "ceshi" << std::vector<double>{1.2, 2.3, 3.4};
        EXPECT_CBOREQ(cbs.GetData(), "69 5b 31 2c 5b 31 2c 32 5d 5d 65 63 65 73 68 69 83 fb 3f f3 33 33 33 33 33 33 fb 40 02 66 66 66 66 "
                                     "66 66 fb 40 0b 33 33 33 33 33 33 ");
    }

    PREPARE_TEST
    {
        test_encoder_stream_io sio(1, 2, 3.5, "cesiashdka");
        cbs << sio;
        EXPECT_CBOREQ(cbs.GetData(), "6f 31 32 33 2e 35 63 65 73 69 61 73 68 64 6b 61 ");
    }

    PREPARE_TEST
    {
        bool t1 = false;
        bool t2 = false;
        cbs << true << false;
        cbs >> t1 >> t2;
        EXPECT_EQ(t1, true);
        EXPECT_EQ(t2, false);
    }

    PREPARE_TEST
    {
        short s = 0;
        for (short i : {2, 14, 25, 56, 241, -21, -124, -5, -5116, -24901})
        {
            cbs << i;
            cbs >> s;
            EXPECT_EQ(i, s);
        }
    }

    PREPARE_TEST
    {
        int s = 0;
        for (auto i : {100, 1000, 10000, 100000, -100, -100000, -87923000})
        {
            cbs << i;
            cbs >> s;
            EXPECT_EQ(i, s);
        }
    }

    PREPARE_TEST
    {
        int64_t s = 0;
        std::vector<int64_t> vec = {3000000000, 452384728947, 17515481548154, 435678399658346583, -274632784628453285};
        for (auto i : vec)
        {
            cbs << i;
            cbs >> s;
            EXPECT_EQ(i, s);
        }
    }

    PREPARE_TEST
    {
        float s = 0.0f;
        for (auto i : {0.0754f, 34.12f, 7.986f, -46583.46f, -2742.85f})
        {
            cbs << i;
            cbs >> s;
            EXPECT_EQ(i, s);
        }
    }

    PREPARE_TEST
    {
        double s = 0.0;
        for (auto i : {0.000754, 34.12, 7.98646471, 4356783996583.46583, -27463278462.8453285})
        {
            cbs << i;
            cbs >> s;
            EXPECT_EQ(i, s);
        }
    }

    PREPARE_TEST
    {
        for (std::string i : {"0.000754", "3ad4f12", "bhdsf", "0xashdgox", ""})
        {
            cbs << i;
            std::string s;
            cbs >> s;
            EXPECT_STRING_EQUAL(s.c_str(), i.c_str());
        }
    }

    PREPARE_TEST
    {
        std::vector<unsigned char> tp{0xFF, 0XFE, 0XFD, 0XFC, 0XFB, 0XEA};
        cbs << tp;
        std::vector<unsigned char> tp2;
        cbs >> tp2;
        for (auto j = 0u; j < tp.size(); ++j)
        {
            EXPECT_EQ(tp.at(j), tp2.at(j));
        }
    }

    PREPARE_TEST
    {
        for (auto i : {"0.000754", "3ad4f12", "bhdsf", "0xashdgox", ""})
        {
            cbs << i;
            std::string s;
            cbs >> s;
            EXPECT_STRING_EQUAL(i, s.c_str());
        }
    }

    PREPARE_TEST
    {
        std::vector<int> ls1 = {1, 2, 3, 4, 5};
        std::deque<std::string> qu = {"cehi", "32846de", "queudbvf", "%^45243**&/n"};
        std::list<double> ld1(3, 5.056);
        cbs << ls1 << ld1 << qu;
        std::vector<int> tls1;
        std::list<double> lld1;
        std::deque<std::string> lqu;
        cbs >> tls1 >> lld1 >> lqu;
        EXPECT_EQ(ls1 == tls1, true);
        EXPECT_EQ(ld1 == lld1, true);
        EXPECT_EQ(qu == lqu, true);
    }

    PREPARE_TEST
    {
        std::map<int, int> mp1 = {{1, 2}, {2, 2}, {3, 56}};
        std::unordered_map<std::string, std::string> mp2 = {{"key", "value"}, {"jkfdh", "vfd"}, {"c876rw%^", ""}};
        // const std::unordered_map<std::string, std::string> &pp = mp2;
        std::vector<int> a1 = {782736, 123, -343242};
        std::vector<int> a2 = {97969, -23424, -12361};
        std::vector<int> a3 = {1212, -989, 0};
        std::map<std::string, std::vector<int>> mp3, lmp3;
        mp3.insert(std::make_pair("test1", a1));
        mp3.insert(std::make_pair("test2", a2));
        mp3.insert(std::make_pair("test3", a3));
        std::map<int, std::map<int, int>> mp4, lmp4;
        mp4.emplace(1, mp1);
        mp4.emplace(-99, mp1);
        std::map<double, double> mp5, lmp5;
        for (int i = 0; i < 100; ++i)
        {
            mp5.emplace(0.25 * i, 0.75 * i * i);
        }

        cbs << mp1 << mp3 << mp4 << mp5;
        std::map<int, int> lmp1;
        cbs >> lmp1 >> lmp3 >> lmp4 >> lmp5;
        EXPECT_EQ(mp1 == lmp1, true);
        EXPECT_EQ(mp3 == lmp3, true);
        EXPECT_EQ(mp4 == lmp4, true);
        EXPECT_EQ(mp5 == lmp5, true);
        // todo: very large nint?
    }

    PREPARE_TEST
    {
        cbs << "[1,[1,2]]"
            << "ceshi" << std::vector<double>{1.2, 2.3, 3.4};
        std::string s1, s2;
        std::vector<double> vec;
        cbs >> s1 >> s2 >> vec;
        EXPECT_STRING_EQUAL(s1.c_str(), "[1,[1,2]]");
        EXPECT_STRING_EQUAL(s2.c_str(), "ceshi");
        int cnt = 0;
        for (auto i : {1.2, 2.3, 3.4})
        {
            EXPECT_EQ(i, vec[cnt]);
            ++cnt;
        }
    }

    PREPARE_TEST
    {
        test_encoder_stream_io sio(1, 2, 3.5, "cesiashdka");
        cbs << sio;
        std::string str;
        cbs >> str;
        EXPECT_STRING_EQUAL(str.c_str(), "123.5cesiashdka");
    }

    {
        std::stringstream ss;
        EXPECT_STRING_EQUAL(enum_to_string(TestEnum::WeakEnum::A).c_str(), "wa");
        EXPECT_STRING_EQUAL(enum_to_string(TestEnum::WeakEnum::B).c_str(), "wb");
        ss << TestEnum::WeakEnum::A;
        TestEnum::WeakEnum val;
        ss >> val;
        EXPECT_EQ(TestEnum::WeakEnum::A, val);
    }

    {
        std::stringstream ss;
        EXPECT_STRING_EQUAL(enum_to_string(TestEnum::StrongEnum::A).c_str(), "sa");
        EXPECT_STRING_EQUAL(enum_to_string(TestEnum::StrongEnum::B).c_str(), "sb");
        ss << TestEnum::WeakEnum::A;
        TestEnum::WeakEnum val;
        ss >> val;
        EXPECT_EQ(TestEnum::WeakEnum::A, val);
    }

    {
        std::stringstream ss;
        EXPECT_STRING_EQUAL(enum_to_string(TestEnum::Foo::NestedEnum::A).c_str(), "fa");
        EXPECT_STRING_EQUAL(enum_to_string(TestEnum::Foo::NestedEnum::B).c_str(), "fb");
        ss << TestEnum::Foo::NestedEnum::A;
        TestEnum::Foo::NestedEnum val;
        ss >> val;
        EXPECT_EQ(TestEnum::Foo::NestedEnum::A, val);
    }

    {
        auto j = Json(Json::array{1, 2, 3, 4, 5});
        CborStream cbs;
        cbs << j;
        string err;
        auto j2 = Json::parse(cbs.GetData(), err, JsonFormat::BINARY_STANDARD);
        EXPECT_EQ(j, j2);
        std::cout << j2.stringify() << std::endl;
    }

    {
        TestReflect::Point p1{1, 2, {1.1, 2.1345555, 3.08E+10, 4, 5}, "point1"};
        TestReflect::Point p2{3, 5, {1, 1.9, 1, 1.09, 1}, "point2"};
        TestReflect::Point p3{-1, -2, {10, 9.1, 6, 4.77, 2}, "point3"};
        TestReflect::Rect rect1{p1, std::vector<TestReflect::Point>{p1, p2}, 0xffff};
        TestReflect::Rect rect2{p3, std::vector<TestReflect::Point>{p2, p3}, 0xffee};
        TestReflect::Box box;
        box.faces.emplace("left face", rect1);
        box.faces.emplace("right face", rect2);
        box.Boxtype = TestReflect::box_type::none;
        CborStream cbs;
        cbs << box;
        string err;
        auto j = Json::parse(cbs.GetData(), err, JsonFormat::BINARY_STANDARD);
        EXPECT_STRING_EQUAL(j["Boxtype"].string_value().c_str(), "none");
        EXPECT_EQ(j["faces"]["left face"]["plist"][1]["x"], 3);
        std::cout << j.stringify() << std::endl;
    }
}

int main(int, char **)
{
    jsonp_test();
    cborp_test();
    combo_test();
    codec_test();
}
