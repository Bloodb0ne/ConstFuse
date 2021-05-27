# ConstFuse
Single header library for parser combinators written in C++ 17. Its not really meant for production, but a exploration of templates and combinator parsing and constexpr.


Medium Article: https://link.medium.com/W0oAN8D3z5

Alternative Blog Article: https://bloodb0ne.github.io/constfuse_parser_combinator.html

**ConstFuse.h** 	Contains all the combinators and helper components
**StringParsers.h** Contains simple string and character related parsers used in conjunction with **ConstFuse.h**

## Combinators
_namespace: **constfuse**_

**Seq**
    _matches a sequence of parsers in the defined order_

**Any**
    _matches any of the passed parses following the same order they are passed in_

**Repeat**
    _matches a parser multiple times but has to match it at leas once_

**RepeatN**
    _matches a parser a fixed amount of times_

**Many**
    _matches a parser multiple times, can be a matched 0 times_

**Optional**
    _matches a parser, but succeeds every time even if it failed_

**LIgnore ( operator << )**
    _matches a parses but ignores the result from the left side_

**RIgnore ( operator >> )**
    _matches a parser but ignores the result from the right side (ex. comma)_

**Not**
    _when you want a certain parser to not match_

**Map**
    _applies a function to the parsed result_

**Precond**
    _executes the parsing if a condition is satisfied in the current accumulated result_

**Postcond**
    _modifies the result after succesful parsing_

**Reference**
    _create a parser reference for handling recursive cases_

**SepBy**
    _matches a parser separated by another parser_

**FullSepBy**
    _matches a parser separated by another parser, the result includes the parsed separator result_

## Monadic
_namespace: **constfuse::monadic**_

**Many**
    _similar to the normal Many combinator, but works over a single result_

**Repeat**
    _similar to the normal Repeat combinator, but works over a single result_

**Combine ( operator && )**
    _chains parsers together and combines the result if they all succeded_

**OrCombine ( operator || )**
    _chains parsers together and combines the result if at least one succeded_

## Compound
_namespace: **constfuse::compound**_

**LeftBinOper**
    _handles left associative binary operations_    

**RightBinOper**
    _handles right associative binary operations_ 

**Postfix**
    _handles postfix operators_

**Prefix**
    _handles prefix operators_

**Wrapper**
    _handles a parses wrapper between 2 different parses (ex. Handling brackets)_

The compound operators can be made and used to handle complex relations of parsers like associativity and order that cant really be done because of left recursive grammars.

## String Combiners

_namespace: **constfuse::string**_

**ParseChar**
    _parses a single character_

**ParseNumber**
    _parses a single digit_

**CharRange**
    _parses a range of characters specified with a start and end one_

**AcceptString**
    _parses a string and returns it as a result_

**ParseASCII**
    _parses a ascii symbol between 0-255_

**ParseLit**
    _parses a string literal_

# Literals

**""_symb**
    _a shorter way to use ParseChar_

**""_asymb**
    _same as symb but accepts the result returning a string (using AcceptString)_

**""_range**
    _parses any of the symbols in the range_

# Things that didnt make the first version
- Simplified Iterator usage, currently it uses a start and an end iterator but it would be better to have a single parameter similar to span.
- Basic Parsers and Iterator for handling binary files ( parsing binary structure sounds like something usefull for validating file structure)
- Polishing String/Char handling parsers
- Add References to some related papers on the subject
- Add more interesting and usefull examples 

# Error handling
At the moment errors might get quite cryptic because of the templating, static_asserts might be usefull but its hard to mention class names ( including template params ) in the error message. Another thing that must be considered is pushing a error state inside the ContextIterator to track error/s history, another benefit from that might be attaching error recovery procedures. Part of the Context handling is layed out but only basic features (at the end of the file EmptyContext/GenericContext and the template parameter of the Iterator).