#pragma once

namespace constfuse::string {

	
	struct ParseChar {
		using is_parser_type = std::true_type;
		using return_type = char;

		const char ltr;
		constexpr ParseChar(const char& _ltr) :ltr(_ltr) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* r) const {
			if (it == end) return false;
			if (*it == ltr) {
				if (it != end) ++it;
				if (r != nullptr) {
					*r = ltr;
				}
				return true;
			}

			return false;
		};

	};



	struct ParseNumber {
		using is_parser_type = std::true_type;
		using return_type = unsigned int;

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* r) const {
			if (it == end) return false;
			
			if (*it >= '0' && *it <= '9') {
				char num = *it;
				if (it != end) it++;
				*r = num - '0';
				return true;
			}

			return false;
		};
	};

	struct CharRange {
		using is_parser_type = std::true_type;
		using return_type = char;

		const char range_start;
		const char range_end;

		constexpr CharRange(const char& start, const char& end) 
									:range_start(start), range_end(end) {}

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* r) const {
			if (it == end) return false;
			if (*it >= range_start && *it <= range_end) {
				*r = *it;
				if (it != end) ++it;
				return true;
			}
			return false;
		}
	};

	template<typename Parser>
	struct AcceptString {
		using is_parser_type = std::true_type;
		using return_type = std::string;
		using parser_return_type = typename Parser::return_type;

		Parser const p;

		constexpr AcceptString(Parser const& par) :p(par) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			parser_return_type p_result;
			auto res = p(it, end, &p_result);
			if (res) {
				if constexpr (std::is_same_v<parser_return_type, char>) {
					result->append(std::string(1, p_result));
				}
				else {
					result->append(p_result);
				}
				return true;
			}
			return false;
		}
	};

	struct ParseASCII {
		using is_parser_type = std::true_type;
		using return_type = char;

		constexpr ParseASCII() {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			if (it == end) return false;
			if (*it >= 0 && *it <= 255) {
				*result = *it;
				if (it != end) it++;
				return true;
			}

			return false;
		}
	};

	template<std::size_t Size>
	struct ParseLit {
		using is_parser_type = std::true_type;
		using return_type = std::string;

		const char* const p_;
		const std::size_t sz_;

		constexpr ParseLit(const char(&str)[Size]) :p_(str), sz_(Size - 1) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			if (it == end) return false;
			std::size_t cnt = 0;
			while (it != end && *it == *(p_ + cnt))
			{
				result->append(std::string(1, *it));
				++it; cnt++;
			}
			if (cnt == sz_) {
				return true;
			}
			//Backtrack ?
			return  false;

		}
	};


	constexpr auto operator ""_symb(const char letter) {
		return ParseChar(letter);
	};

	constexpr auto operator ""_asymb(const char letter) {
		return AcceptString(ParseChar(letter));
	};


	/*
	If this made it into the spec for C++17 i could have been a templated user defined literal
	there was an idea of using a array buffer and applying it to a lambda 
	but that just looks ugly and eats heap*/
	template<char ...c>
	constexpr auto symbs() {
		return (ParseChar(c) || ...);
	}


	constexpr auto operator ""_range(const char* string, std::size_t size) {
		//static_assert(size == 4,"Invalid Character range specified");
		//good idea but not a constant expression :(
		return CharRange(string[0], string[3]);
	};



}