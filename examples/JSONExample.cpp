#include "../ConstFuse.h"

#include <iostream>
#include <numeric>
#include "../StringParsers.h"

using namespace constfuse;
using namespace constfuse::string;

using ctxStringIter = ContextAwareIterator<std::string::iterator>;



int main() {

	//Transformations	
	constexpr auto to_string = [](auto v) { return std::string(1, v); };
	constexpr auto vec_to_string = [](auto v) {
		return std::accumulate(v.begin(), v.end(), std::string{});
	};

	//Grammar
	constexpr auto ws = monadic::Many(symbs<'\n', '\t', '\r', ' '>() % to_string);
	constexpr auto str_wrap = compound::Wrapper('"'_symb, '"'_symb);
	constexpr auto obj_wrap = compound::Wrapper('{'_symb, '}'_symb);
	constexpr auto array_wrap = compound::Wrapper('['_symb, ']'_symb);

	//Handle Scientific notation
	constexpr auto json_int = AcceptString('0'_symb % to_string || "1..9"_range % to_string && monadic::Many(AcceptString("0..9"_range)));
	constexpr auto json_exp = symbs<'E', 'e'>() % to_string && Optional(symbs<'+', '-'>() % to_string) && json_int;
	constexpr auto float_part = AcceptString('.'_symb % to_string && monadic::Many(AcceptString("0..9"_range)));

	constexpr auto json_number = Optional('-'_symb % to_string) && json_int && Optional(float_part) && Optional(json_exp);

	constexpr auto json_string = str_wrap(monadic::Repeat(AcceptString("a..z"_range || "A..Z"_range)));

	constexpr auto json_obj = [=](const auto json_value) {
		return obj_wrap(SepBy(json_value, ','_symb) % vec_to_string || ws);
	};
	constexpr auto json_arr = [=](const auto json_value) {
		return array_wrap(SepBy(json_value, ','_symb) % vec_to_string || ws);
	};

	Rule<ctxStringIter, std::string> json_value =
		json_number ||
		json_string ||
		json_arr(Reference(&json_value)) ||
		json_obj(json_string && AcceptString(':'_symb) && AcceptString(Reference(&json_value))) ||
		ParseLit("true") ||
		ParseLit("false") ||
		ParseLit("null");


	std::string test_string = "[1,-3.45,[],{},5,6,\"asd\",{\"Test\":\"Arr\"},{\"Tester\":[1,2,34,3,5,6]},{\n}]";

	ctxStringIter start = test_string.begin();
	ctxStringIter end = test_string.end();

	decltype(json_value)::return_type var_test;
	bool hasParsed = json_value(start, end, &var_test);

	if (hasParsed) {
		std::cout << "Parsed Successfully" << std::endl;
		std::cout << "Result:= " << var_test << std::endl;
		std::cout << "(line: " << start.line() << " col: " << start.column() << std::endl;
	}
	else {
		std::cout << "Error @ (line: " << start.line() << " col: " << start.column() << ")" << std::endl;
	}


	return 0;
}