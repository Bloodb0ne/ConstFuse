#include <vector>
#include <algorithm>
#include <array>
#include <functional>
#include <variant>
#include <optional>
#include <any>

namespace constfuse {
	
	namespace traits {
		//Unique Variant 
		template<typename T, typename ...List>
		constexpr bool is_in_list = (std::is_same_v<T, List> || ...);

		template<typename ...Types>
		struct flat_type_container {
			using type = flat_type_container<Types...>;
			using variant = std::variant < Types...>;
		};

		template<typename First, typename ...Types>
		struct flat_type_container<First, flat_type_container<Types...>> {
			using type = flat_type_container<First, Types...>;
		};

		template<typename First, typename ...Rest>
		struct unique_types {
			using type = typename std::conditional_t <
				!is_in_list<First, Rest...>,
				typename flat_type_container<First, typename unique_types<Rest...>::type>::type,
				typename unique_types<Rest...>::type
			>;
		};

		template<typename First>
		struct unique_types<First> {
			using type = flat_type_container<First>;
		};

		template<typename ...Types>
		struct get_unique_variant {
			using container = typename unique_types<Types...>::type;
			using variant = typename container::variant;
		};

		template<typename ...List>
		using unique_variant = typename get_unique_variant<List...>::variant;


		template <typename A, typename B> struct is_compat {
			using PA = typename std::add_pointer<A>::type;
			using PB = typename std::add_pointer<B>::type;
			static constexpr bool value = std::is_convertible<PA, PB>::value;
		};

		template<typename A, typename B>
		struct are_compatible {
			static bool constexpr value = is_compat<A, B>::value || is_compat<B, A>::value;
		};

		template< class, class = std::void_t<>>
		struct has_parser_trait : std::false_type { };

		template< class T >
		struct has_parser_trait< T, std::void_t<typename T::is_parser_type> > {
			static constexpr bool value = std::is_same_v<typename T::is_parser_type, std::true_type>;
		};


		template<typename A = void>
		static constexpr bool is_parser_v = has_parser_trait<A>::value;
		
		template<typename ...A>
		static constexpr bool are_parser_v = (has_parser_trait<A>::value && ... );


		template<typename A>
		struct is_parser {
			static_assert(is_parser_v<A>, "Type is not a parser.Parsers need a ::is_parser_type = std::true_type.");
			static bool constexpr value = is_parser_v<A>;
		};


		template< class, class = std::void_t<>>
		struct has_parser_handle_trait : std::false_type
		{ };

		template< class T >
		struct has_parser_handle_trait< T, std::void_t<typename T::is_parser_rule_type> > {
			static constexpr bool value = std::is_same_v<typename T::is_parser_rule_type, std::true_type>;
		};

		template<typename A = void>
		static constexpr bool is_parser_handle_v = has_parser_handle_trait<A>::value;

		template<typename P>
		using is_parser_handle_t = typename std::enable_if_t<is_parser_handle_v<P>>;

		template<typename P>
		using is_parser_t = typename std::enable_if_t<is_parser_v<P>>;

		template<typename ...P>
		using are_parsers = std::enable_if_t<are_parser_v<P>, int>;


		template<typename A, typename B>
		using are_parsers_concept = std::enable_if_t<is_parser_v<A> && is_parser_v<B>, int>;

		template<typename A, typename B>
		using are_parsers_handles_concept = std::enable_if_t<(is_parser_v<A> || is_parser_handle_v<A>) && (is_parser_v<B> || is_parser_handle_v<B>), int>;

		template<typename A, typename B>
		struct most_common {
			using type =
				std::conditional_t<
				is_compat<typename A, typename B>::value,
				typename A,
				typename B>;
		};

	}

	namespace helpers{
		/*
			Folding helpers for associative combinators
		*/

		constexpr auto postfix_fold = [](auto t) {
			auto result = std::get<0>(t);
			std::for_each(std::get<1>(t).begin(), std::get<1>(t).end(), [&result](auto item)->void {
				result = std::invoke(item, result);
				});

			return result;
		};
		constexpr auto prefix_fold = [](auto t) {
			auto result = std::get<1>(t);
			std::for_each(std::get<0>(t).rbegin(), std::get<0>(t).rend(), [&result](auto item)->void {
				result = std::invoke(item, result);
				});

			return result;
		};


		constexpr auto left_associative_fold = [](auto t) {
			auto result = std::get<0>(t);
			std::for_each(std::get<1>(t).begin(), std::get<1>(t).end(), [&result](auto item)->void {
				auto action = std::get<0>(item);
				result = std::invoke(action, result, std::get<1>(item));
				});

			return result;
		};
		constexpr auto right_associative_fold = [](auto t) {
			auto result = std::get<0>(t);
			std::for_each(std::get<1>(t).rbegin(), std::get<1>(t).rend(), [&result](auto item)->void {
				auto action = std::get<0>(item);
				result = std::invoke(action, result, std::get<1>(item));
				});

			return result;
		};

		/*
		Helper function for applying each parser to the value in the result tuple
		Note: could be a member of Seq, but i failed to make it work with apply
		*/
		template<typename Iterator, typename PResults, typename ...Parsers>
		inline auto all_applicator_res(Parsers const& ...p) {

			return [&](Iterator& it, Iterator end, PResults& result) -> bool {
				return std::apply([&](auto& ...result_item) -> bool {
					return (p(it, end, &result_item) && ...);
					}, result);
			};

		};

		/*
			Helper for the Any parser combinator
		*/
		template<typename Iterator, typename PResults, typename FResult, typename ...Parsers>
		inline auto any_parser_applicator(Parsers const& ...p) {

			return [&](Iterator& it, Iterator end, Iterator backtrack, PResults& result, FResult& fres) -> bool {
				auto aplicator = [&](auto& ...result_item) -> bool {
					//Pack and pluck after ?
					Iterator max = backtrack;
					bool parse_successful = false;
					bool any_parse = (
						(backtrack = it,
							parse_successful = p(backtrack, end, &result_item),
							max = backtrack < max ? max : backtrack,
							fres = result_item,
							parse_successful)
						|| ...);
					if (any_parse) {
						it = backtrack; //maybe
					}
					else {
						it = max; //Max failiure point
					}

					return any_parse;

				};
				return std::apply(aplicator, result);
			};
		};

	}

	template<typename Parser>
	struct Try {
		using is_parser_type = std::true_type;
		using return_type = typename Parser::return_type;

		Parser p;
		constexpr Try(Parser const& p_) :p(p_) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			Iterator backtrack = it;

			if (p(it, end, result)) {
				return true;
			}

			it = backtrack;
			return false;
		}
	};

	namespace monadic {

		template<typename Parser>
		struct Many {
			using is_parser_type = std::true_type;
			using return_type = typename Parser::return_type;


			Parser const p;
			constexpr Many(Parser const& p_) :p(p_) {};

			template<typename Iterator>
			bool operator()(Iterator& it, Iterator end, return_type* result) const {

				Iterator backtrack = it;
				size_t cnt = 0;

				while (p(it, end, result)) {
					backtrack = it;
					++cnt;
				}

				return true;
			}
		};



		template<typename Parser>
		struct Repeat {
			using is_parser_type = std::true_type;
			using return_type = typename Parser::return_type;

			const Parser p;

			constexpr Repeat(Parser const& p_) :p(p_) {};

			template<typename Iterator>
			bool operator()(Iterator& it, Iterator end, return_type* result) const {

				Iterator backtrack = it;
				size_t cnt = 0;

				while (p(it, end, result)) {
					backtrack = it;
					++cnt;
				}
				if (cnt == 0) return false;

				return true;
			}
		};

		template<typename Parser>
		struct RepeatN {
			using is_parser_type = std::true_type;
			using return_type = typename Parser::return_type;

			const Parser p;
			const std::size_t count;

			constexpr RepeatN(Parser const& p_,std::size_t n) :p(p_),count(n) {};

			template<typename Iterator>
			bool operator()(Iterator& it, Iterator end, return_type* result) const {

				Iterator backtrack = it;
				size_t cnt = 0;

				while (p(it, end, result)) {
					backtrack = it;
					++cnt;
					if (cnt == count) {
						return true;
					}
				}
				

				it = backtrack; //Should this be here
				return false;
			}
		};


		template<typename LeftParser, typename RightParser>
		struct Combine {
			using is_parser_type = std::true_type;
			using return_type = typename traits::most_common<typename LeftParser::return_type, typename RightParser::return_type>::type;

			LeftParser const lparser;
			RightParser const rparser;

			constexpr Combine(LeftParser const& lp, RightParser const& rp) :lparser(lp), rparser(rp) {};

			template<typename Iterator>
			bool operator()(Iterator& it, Iterator end, return_type* result) const {
				return lparser(it, end, result) && rparser(it, end, result);
			};

		};

		template<typename LeftParser, typename RightParser>
		struct OrCombine {
			using is_parser_type = std::true_type;
			using return_type = typename traits::most_common<typename LeftParser::return_type, typename RightParser::return_type>::type;

			LeftParser const lparser;
			RightParser const rparser;

			constexpr OrCombine(LeftParser const& lp, RightParser const& rp) :lparser(lp), rparser(rp) {};

			template<typename Iterator>
			bool operator()(Iterator& it, Iterator end, return_type* result) const {
				return Try(lparser)(it, end, result) || Try(rparser)(it, end, result);
			};
		};

		
	}

	template<typename Iterator, typename ReturnType>
	struct Rule {
		using is_parser_type = std::true_type;
		using return_type = ReturnType;

	private:

		struct PolyInterface {
			virtual ~PolyInterface() {}

			virtual bool parse(
				Iterator& it,
				Iterator end,
				ReturnType* result = nullptr
			)const = 0;
		};

		template<typename Parser>
		struct PolyParser :PolyInterface {
			Parser const p;
			explicit PolyParser(Parser const& q) : p(q) {}
			explicit PolyParser(Parser&& q) : p(std::forward<Parser>(q)) {}

			virtual bool parse(Iterator& it, Iterator end, ReturnType* result) const override {
				return p(it, end, result);
			};
		};

		std::shared_ptr<const PolyInterface> p;

	public:

		template<typename Parser, typename = typename traits::is_parser_t<Parser>>
		constexpr Rule(Parser const& q) : p(new PolyParser<Parser>(q)) {  };

		template<typename Parser, typename = typename traits::is_parser_t<Parser>>
		constexpr Rule(Parser&& q) : p(new PolyParser<Parser>(std::forward<Parser>(q))) {};

		constexpr Rule() {};

		bool operator()(Iterator& it, Iterator end, ReturnType* r) const {

			if (p) return p->parse(it, end, r);
			return false;
		};


	};

	
	struct Success {
		using is_parser_type = std::true_type;
		using return_type = bool;

		constexpr Success(){};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			
			return true;
		}
	};
	struct Fail {
		using is_parser_type = std::true_type;
		using return_type = bool;

		constexpr Fail() {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			return false;
		}
	};
	

	template<typename LeftParser, typename RightParser>
	struct LIgnore {
		using is_parser_type = std::true_type;
		using return_type = typename RightParser::return_type;

		using ignored_result = typename LeftParser::return_type;

		LeftParser const lparser;
		RightParser const rparser;

		constexpr LIgnore(LeftParser const& lp, RightParser const& rp) :lparser(lp), rparser(rp) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			ignored_result ignored;
			return lparser(it, end, &ignored) && rparser(it, end, result);
		}
	};

	template<typename LParser, typename RParser>
	struct RIgnore {
		using is_parser_type = std::true_type;
		using return_type = typename LParser::return_type;

		using ignored_result = typename RParser::return_type;

		LParser const lparser;
		RParser const rparser;

		constexpr RIgnore(LParser const& lp, RParser const& rp) :lparser(lp), rparser(rp) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			ignored_result ignored;
			return lparser(it, end, result) && rparser(it, end, &ignored);
		}
	};


	template<typename F, typename Parser>
	struct Map {
		using is_parser_type = std::true_type;
		using return_type = typename std::invoke_result_t<F, typename Parser::return_type>;
		using parser_return_type = typename Parser::return_type;

		F const functor;
		Parser const p;

		constexpr Map(F const& f, Parser const& par) :p(par), functor(f) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			parser_return_type p_result;
			auto res = p(it, end, &p_result);
			if (res) {
				*result = functor(std::forward<parser_return_type>(p_result));
				return true;
			}
			return false;
		}
	};


	template<typename F, typename Parser>
	struct Precond {

		using is_parser_type = std::true_type;
		using return_type = typename Parser::return_type;

		F const functor;
		Parser const p;

		constexpr Precond(F const& f, Parser const& par) :p(par), functor(f) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			if (functor(result)) {
				return p(it, end, result);
			}
			return false;
		}
	};

	template<typename F,typename Parser>
	struct Postcond {
		
		using is_parser_type = std::true_type;
		using parser_return_type = typename Parser::return_type;
		using return_type = typename std::invoke_result_t < F, std::add_lvalue_reference_t<bool>, std::add_lvalue_reference_t<parser_return_type>>;

		

		F const functor;
		Parser const p;

		constexpr Postcond(F const& f, Parser const& par) :p(par), functor(f) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			Iterator backtrack = it;
			parser_return_type temp;
			bool res = p(it, end, &temp);
			*result = functor(res, temp); //can modify the result
			if (!res) { it = backtrack; } // Is this usefull
			return res;
		}
	};


	template<typename ...Parsers>
	struct Seq {

		using is_parser_type = std::true_type;
		using return_type = typename std::tuple<typename Parsers::return_type...>;
		using container_type = typename std::tuple<Parsers...>;


		container_type const parsers;

		explicit constexpr Seq(Parsers const& ...p) :parsers(p...) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			return_type tmp;
			Iterator backtrack = it;
			bool res = std::apply(helpers::all_applicator_res<Iterator, return_type, Parsers...>, parsers)(it, end, tmp);

			if (res) {
				*result = tmp;
			}
			return res;
		}
	};


	template<typename Parser>
	struct Not {
		using is_parser_type = std::true_type;
		using return_type = typename Parser::return_type;


		Parser const p;
		constexpr Not(Parser const& p_) :p(p_) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			Iterator backtrack = it;
			size_t cnt = 0;

			if (p(it, end, result)) {
				it = backtrack; //Q
				return false;
			}
			it = backtrack; // Q
			return true;
		}
	};


	template<typename Parser>
	struct Repeat {
		using is_parser_type = std::true_type;
		using return_type = typename std::vector<typename Parser::return_type>;
		using temp_result_type = typename Parser::return_type;


		Parser const p;
		constexpr Repeat(Parser const& p_) :p(p_) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			Iterator backtrack = it;
			size_t cnt = 0;

			temp_result_type tmpres;

			while (p(it, end, &tmpres)) {
				result->push_back(tmpres);
				backtrack = it;
				++cnt;
			}
			if (cnt == 0) return false;

			return true;
		}
	};

	template<typename Parser>
	struct RepeatN {
		using is_parser_type = std::true_type;
		using return_type = typename std::vector<typename Parser::return_type>;
		using temp_result_type = typename Parser::return_type;

		const Parser p;
		const std::size_t count;

		constexpr RepeatN(Parser const& p_, std::size_t n) :p(p_), count(n) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			Iterator backtrack = it;
			size_t cnt = 0;

			temp_result_type tmpres;

			while (p(it, end, &tmpres)) {
				result->push_back(tmpres);
				backtrack = it;
				++cnt;
				if (cnt == count) return true;
			}
			

			it = backtrack;
			return false;
		}
	};

	template<typename Parser>
	struct Many {
		using is_parser_type = std::true_type;
		using return_type = typename std::vector<typename Parser::return_type>;
		using temp_result_type = typename Parser::return_type;


		Parser const p;
		constexpr Many(Parser const& p_) :p(p_) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			Iterator backtrack = it;
			size_t cnt = 0;

			temp_result_type tmpres;

			while (p(it, end, &tmpres)) {
				result->push_back(tmpres);
				tmpres = temp_result_type();
				backtrack = it;
				++cnt;
			}

			return true;
		}
	};

	

	template<typename Parser>
	struct Reference {
		using is_parser_type = std::true_type;
		using return_type = typename Parser::return_type;

		Parser const* p;

		constexpr Reference(Parser const* par) :p(par) {};


		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			return (*p)(it, end, result);
		}
	};


	template<typename Parser>
	struct Optional {
		using is_parser_type = std::true_type;
		using return_type = typename Parser::return_type;

		Parser p;
		constexpr Optional(Parser const& p_) :p(p_) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			Iterator backtrack = it;

			if (p(it, end, result)) {
				return true;
			}

			it = backtrack;
			return true;
		}
	};


	

	template<typename ...Parsers>
	struct Any {
		using is_parser_type = std::true_type;
		using return_type = typename traits::unique_variant<typename Parsers::return_type...>;
		using parsers_container = typename std::tuple<Parsers...>;
		using tmp_return_type = typename std::tuple<typename Parsers::return_type...>;

		/*
		Alternative code for handling the execution
		template<size_t N, typename ReturnType, typename Iterator, typename Parsers, typename Results>
		struct AnyParsersMap {
			ReturnType operator()(Parsers& parsers, Results& results, Iterator& it, Iterator end, Iterator back) {
				if (std::get<N>(parsers)(it, end, &std::get<N>(results))) {
					return std::get<N>(results);
				}
				it = back;
				return AnyParsersMap<N - 1, ReturnType, Iterator, Parsers, Results>()(parsers, results, it, end, back);
			}
		};

		template<typename ReturnType, typename Iterator, typename Parsers, typename Results>
		struct AnyParsersMap<0, ReturnType, Iterator, Parsers, Results> {
			ReturnType operator()(Parsers& parsers, Results& results, Iterator& it, Iterator end, Iterator back) {
				if (std::get<0>(parsers)(it, end, &std::get<0>(results))) {
					return std::get<0>(results);
				}
				it = back;
				return std::nullopt;
			}
		};

		// expand index sequence, evaluate parser is_valid_parse(std::get<I>(parsers)(it, end, &std::get<I>(results))
		template<typename Iterator,typename ReturnType,typename Parsers>
		bool execute(Iterator& it, Iterator& end, Iterator & back, ReturnType& result,Parsers const& p) {
			return
				std::apply([&](auto& ...p)->bool {
							return ((it = back, p(it, end, result)) || ...);
					}, p);
			return false;
		}
		*/

		parsers_container parsers;
		
		constexpr Any(Parsers const& ...p) :parsers(p...) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			Iterator backtrack = it;
			return_type final_result;
			
			auto tempres = new tmp_return_type;
			
			bool has_any_parsed = std::apply(
				helpers::any_parser_applicator<Iterator, tmp_return_type, return_type, Parsers...>, parsers)
				(it, end, it, *tempres, final_result);
			
			delete tempres;
			
			if (has_any_parsed) {
				*result = final_result;
				return true;
			}
			else {
				//it = backtrack;
				return false;
			}
		};
	};

	template<typename Parser, typename Separator,
		typename = typename traits::are_parsers_handles_concept<Parser, Separator>>
		struct SepBy {
		using is_parser_type = std::true_type;
		using return_type = std::vector<typename Parser::return_type>;
		using inner_parser_type = typename Parser::return_type;

		Parser const p;
		Separator const sep;

		constexpr SepBy(Parser const& p_, Separator const& sep_) :p(p_), sep(sep_) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {

			Iterator backtrack = it;
			inner_parser_type temp;

			auto res = p(it, end, &temp);
			if (res) {
				res = Many(sep << p)(it, end, result);
				if (res) result->insert(result->begin(), std::forward<inner_parser_type>(temp));

			}
			return res;
		}
	};

	template<typename Parser, typename Separator,
		typename = typename traits::are_parsers_handles_concept<Parser, Separator>>
		struct FullSepBy {
		using is_parser_type = std::true_type;
		using return_type = typename Seq<Parser, Many<Seq<Separator, Parser>>>::return_type;
		using inner_parser_type = typename Parser::return_type;

		Parser const p;
		Separator const sep;

		constexpr FullSepBy(Parser const& p_, Separator const& sep_) :p(p_), sep(sep_) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			Iterator backtrack = it;

			return Seq(p, Many(Seq(sep, p)))(it, end, result);
		}
	};

	template<typename Sep,typename ...Parsers,
		typename = typename traits::are_parsers<Sep,Parsers...>
	>
	constexpr auto zip(Sep s ,Parsers& ...p) {
		//Cant really make the last not match
		return Seq(p >> s ...);
	};



	/*
		Compound parsers, dont feature a proper return type because that seems
		to confuse some of the type deductions happening.
		We cant know what the return type is gonna be at the time of construction
	*/
	namespace compound {

		template<typename Operator, typename = typename traits::is_parser_t<Operator>>
		struct LeftBinOper {
			using is_parser_type = std::true_type;

			Operator const op;
			constexpr LeftBinOper(Operator const& _op) :op(_op) {}

			template<typename Parser, typename = typename traits::is_parser_t<Parser>>
			constexpr auto operator ()(Parser const& inner) const {

				return FullSepBy(inner, op) % helpers::left_associative_fold;
			}
		};

		template<typename Operator, typename = typename traits::is_parser_t<Operator>>
		struct RightBinOper {
			using is_parser_type = std::true_type;

			Operator const op;
			constexpr RightBinOper(Operator const& _op) :op(_op) {}

			template<typename Parser, typename = typename traits::is_parser_t<Parser>>
			constexpr auto operator ()(Parser const& inner) const {

				return FullSepBy(inner, op) % helpers::right_associative_fold;
			}
		};

		template<typename Operator, typename = typename traits::is_parser_t<Operator>>
		struct Postfix {
			using is_parser_type = std::true_type;

			Operator const op;
			constexpr Postfix(Operator const& _op) :op(_op) {}

			template<typename Parser, typename = typename traits::is_parser_t<Parser>>
			constexpr auto operator ()(Parser const& inner) const {
				return Seq(inner, Many(op)) % helpers::postfix_fold;
			}
		};

		template<typename Operator, typename = typename traits::is_parser_t<Operator>>
		struct Prefix {
			using is_parser_type = std::true_type;

			Operator const op;
			constexpr Prefix(Operator const& _op) :op(_op) {}

			template<typename Parser, typename = typename traits::is_parser_t<Parser>>
			constexpr auto operator ()(Parser const& inner) const {
				return Seq(Many(op), inner) % helpers::prefix_fold;
			}
		};


		/*
			Other Compound parsers
		*/

		template<typename LeftWrap, typename RightWrap, typename = typename  traits::are_parsers_handles_concept<LeftWrap, RightWrap>>
		struct Wrapper {
			using is_parser_type = std::true_type;

			LeftWrap const lhs;
			RightWrap const rhs;
			constexpr Wrapper(LeftWrap const& _lhs, RightWrap const& _rhs) :lhs(_lhs), rhs(_rhs) {}

			template<typename Parser, typename = typename traits::is_parser_t<Parser>>
			constexpr auto operator ()(Parser const& inner) const {
				return (lhs << inner >> rhs);
			}
		};

	}
	/*
		Operator Overloads
	*/

	template<typename P1, typename P2,
		typename = typename traits::are_parsers_handles_concept<P1, P2>>
		constexpr auto operator &&(P1 const& p1, P2 const& p2) {
		return monadic::Combine(p1, p2);
	};

	template<typename P1, typename P2,
		typename = typename traits::are_parsers_handles_concept<P1, P2>>
		constexpr auto operator ||(P1 const& p1, P2 const& p2) {
		return monadic::OrCombine(p1, p2);
	};

	template<typename P1, typename P2,
		typename = typename traits::are_parsers_handles_concept<P1, P2>>
		constexpr auto operator >>(P1 const& p1, P2 const& p2) {
		return RIgnore(p1, p2);
	};

	template<typename P1, typename P2,
		typename = typename traits::are_parsers_handles_concept<P1, P2>>
		constexpr auto operator <<(P1 const& p1, P2 const& p2) {
		return LIgnore(p1, p2);
	};

	template<typename F, typename P,
		typename = typename std::enable_if_t<traits::is_parser_v<P> && std::is_invocable_v<F, typename P::return_type>>
	>
		constexpr auto operator %(P const& p, F const& f) {
		return Map(f, p);
	};

	template<typename F, typename P,
		typename = typename std::enable_if_t<traits::is_parser_v<P>>,
		typename = typename std::is_same<bool, std::invoke_result_t < F, typename P::return_type >>
	>
		constexpr auto operator ^(P const& p, F const& f) {
		return Precond(f, p);
	};

	template<typename F, typename P,
		typename = typename std::enable_if_t<traits::is_parser_v<P>>,
		typename = typename std::invoke_result_t < F, std::add_lvalue_reference_t<bool>, std::add_lvalue_reference_t<typename P::return_type>>
	>
		constexpr auto operator |(P const& p, F const& f) {
		return Postcond(f, p);
	};
	
	//Should a lambda returning a parser be accepted here as a parameter?
	template<typename P1, typename P2,
		typename = typename traits::are_parsers_handles_concept<P1, P2>>
		constexpr auto operator >>=(P1 const& p1, P2 const& p2) {

		return p1(p2);
	};


	

	struct EmptyContext {
		using label_type = void;
	};
	
	template<typename Iterator, std::size_t threshold = 16,typename Context = EmptyContext>
	struct ContextAwareIterator {

		//using iterator_category = typename std::iterator_traits<Iterator>::iterator_category;
		using iterator_category = std::forward_iterator_tag;
		using value_type = typename std::iterator_traits<Iterator>::value_type;
		using difference_type = typename std::iterator_traits<Iterator>::difference_type;
		using pointer = typename std::iterator_traits<Iterator>::pointer;
		using reference = typename std::iterator_traits<Iterator>::reference;

		struct multi_pass_shared_state {
			using buffer_type = std::vector<value_type>;

			buffer_type value_queue;
			Context ctx;
			multi_pass_shared_state() {
				value_queue.reserve(threshold);
			}
		};

		std::shared_ptr<multi_pass_shared_state> multi_pass_state;

		std::size_t queue_position = 0;

		ContextAwareIterator& operator=(ContextAwareIterator const& rhs) = default;
		ContextAwareIterator(Iterator const& it) :
			wrapped_iterator(it),
			multi_pass_state(std::make_shared<multi_pass_shared_state>())
		{
		};
		
		
		ContextAwareIterator operator ++(int) {
			ContextAwareIterator<Iterator, threshold,Context> temp(*this);
			++*this;
			return temp;
		}

		ContextAwareIterator& operator ++() {
			updateState(this->operator*());
			
			std::size_t size = multi_pass_state.get()->value_queue.size();
			if (queue_position == size)
			{
				if (size >= threshold && multi_pass_state.use_count() == 1){
					multi_pass_state.get()->value_queue.clear();
					queue_position = 0;
				}else{
					multi_pass_state.get()->value_queue.push_back(*(wrapped_iterator));
					++queue_position;
				}
				++wrapped_iterator;
			}
			else
			{
				++queue_position;
			}


			return *this;
		}

		bool operator ==(ContextAwareIterator const& rhs) const {
			return wrapped_iterator == rhs.wrapped_iterator;
		}

		bool operator !=(ContextAwareIterator const& rhs) const {
			return wrapped_iterator != rhs.wrapped_iterator;
		}

		reference operator*() {
			std::size_t size = multi_pass_state.get()->value_queue.size();
			if (queue_position == size)
			{
				if (size >= threshold && multi_pass_state.use_count() == 1)
				{
					multi_pass_state.get()->value_queue.clear();
					queue_position = 0;
				}
				return *wrapped_iterator;
				
			}
			return multi_pass_state.get()->value_queue[queue_position];
		}

		bool operator <(ContextAwareIterator const& rhs) const{
			return (line() * column()) < (rhs.line() * rhs.column());
		}

		Context& getContext() { return multi_pass_state.get()->ctx; }
		auto line() const {
			return line_;
		}

		auto column() const {
			return col_;
		}

		void updateState(reference const val) {
			if (val == '\n' || val == '\r') {
				++line_;
				col_ = 0;
			}
			else {
				++col_;
			}

		}
		
		

	private:
		Iterator wrapped_iterator;
		uint16_t line_{1};
		uint16_t col_{0};
	};


	template<typename T>
	struct GenericContext {

		using label_type = T;

		struct Element {
			T tag;
			std::size_t depth{1};
			bool exited{false};
		};

		void enter(T contextTag) {
			++depth;
			context_ring.push_back({contextTag,depth});
		};
		void exit(T contextTag) {
			--depth;
			//Should be able to mark the past node to exited
		};

		bool hasSibling(T contextTag) {
			for (Element& elem : context_ring) {
				if (elem.depth < depth) break;
				if (elem.depth == depth && elem.tag == contextTag) return true;
			}
			return false;
		};

		bool hasParent(T contextTag) {
			auto result = std::find_if(std::begin(context_ring), std::end(context_ring), [&depth](auto t) {
				return t.depth == (depth - 1);
			});

			return result != std::begin(context_ring);
		};
		//Parrents

		std::size_t currentDepth() { return depth; }

	private:
		std::size_t depth{1};
		std::vector<Element> context_ring;
	};
	
	
	template<typename P,std::size_t N>
	struct Labeled {
		using is_parser_type = std::true_type;
		using return_type = typename P::return_type;

		const char* const name;
		P  const p;
		const std::size_t size;

		constexpr Labeled(const char(&name_)[N], P const& p_) :name(name_), p(p_), size(N - 1) {};

		template<typename Iterator>
		bool operator()(Iterator& it, Iterator end, return_type* result) const {
			static_assert(std::is_convertible_v<const char*, std::remove_reference_t<decltype(it.getContext())>::label_type>,"Invalid Label type in context container");
			it.getContext().enter(name);
			bool res = p(it, end, result);
			it.getContext().exit(name);
			return res;
		}
	};

	template<typename To, std::size_t ...I>
	struct Converter {
		constexpr Converter() {};

		template<typename T, class Tuple, std::size_t ...I>
		static constexpr T tuple_forwarder(Tuple&& t, std::index_sequence<I...>) {
			return T{ std::get<I>(std::forward<Tuple>(t)) ... };
		}

		template<typename T, typename Tuple>
		static constexpr T tuple_to_object(Tuple&& t) {
			return tuple_forwarder<T>(std::forward<Tuple>(t), std::make_index_sequence<std::tuple_size_v<std::remove_reference_t<Tuple>>>{});
		}

		template<typename T, typename Tuple, std::size_t ...I,
			typename = std::enable_if_t<std::tuple_size_v<std::remove_reference_t<Tuple>> == sizeof...(I)> >
			static constexpr T tuple_to_object(Tuple&& t) {
			return tuple_forwarder<T>(std::forward<Tuple>(t), std::integer_sequence<std::size_t, I...>{});
		}

		template<typename From>
		To operator()(From&& obj) const {
			return To{ std::forward<From>(obj) };
		}

		template<typename ...From>
		To operator()(std::tuple<From...>&& obj) const {
			return tuple_to_object<To, std::add_lvalue_reference_t<std::tuple<From...>>, I...>(obj);
		}
	};

	template<>
	struct Converter<int> {
		int operator()(std::string s) const{
			return std::stoi(s);
		}
		
	};

	template<typename Value>
	struct to_value {

		Value const v;

		constexpr to_value(Value const& v_) :v(v_) {};

		template<typename T>
		const auto operator()(bool&, T&) const {
			return v;
		}
	};

}
