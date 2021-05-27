#include "../ConstFuse.h"
#include "../StringParsers.h"

#include <iostream>

using namespace constfuse;
using namespace constfuse::string;

using ctxStringIter = ContextAwareIterator<std::string::iterator>;
using binary_int_op = std::function<int(int, int)>;
using unary_int_op = std::function<int(int)>;

int main() {

	constexpr auto single_sum = [](auto)->binary_int_op { return [](int a, int b) { return a + b; }; };
	constexpr auto single_sub = [](auto)->binary_int_op { return [](int a, int b) { return a - b; }; };
	constexpr auto single_mul = [](auto)->binary_int_op { return [](int a, int b) { return a * b; }; };
	constexpr auto single_div = [](auto)->binary_int_op { return [](int a, int b) { return a / b; }; };
	constexpr auto prefix_double = [](auto)->unary_int_op { return [](int a) { return a * a; }; };
	constexpr auto prefix_neg = [](auto)->unary_int_op { return [](int a) { return -a; }; };



	auto mathParser =
		compound::LeftBinOper('+'_symb % single_sum || '-'_symb % single_sub) >>=
		compound::LeftBinOper('*'_symb % single_mul || '/'_symb % single_div) >>=
		compound::Prefix('-'_symb % prefix_neg || '!'_symb % prefix_double) >>=
		ParseNumber();



	std::string test_expression = "3*5*2+1*2+1+1+1+2+3+3";

	ctxStringIter start = test_expression.begin();
	ctxStringIter end = test_expression.end();

	decltype(mathParser)::return_type final_result;
	bool hasParsed = mathParser(start, end, &final_result);

	if (hasParsed) {
		std::cout << "Parsed Successfully" << std::endl;
		std::cout << "Result:= " << final_result << std::endl;
		std::cout << "Ended @ (line: " << start.line() << " col: " << start.column() << ")"<< std::endl;
	}else {
		std::cout << "Error @ (line: " << start.line() << " col: " << start.column() << ")" << std::endl;
	}

}
