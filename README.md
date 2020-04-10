# ConstFuse
Single header library for parser combinators written in C++ 17.

Medium Article: https://link.medium.com/W0oAN8D3z5

**ConstFuse.h** 	Contains all the combinators and helper components
**StringParsers.h** Contains simple string and character related parsers

## Combinators
***general***
Seq
Any
Repeat
Many
Optional
LIgnore ( operator << )
RIgnore ( operator >> )
Map
Precond
Reference

***monadic parsers ( they work on the same result value )***
Many
Repeat
Combine ( operator && )
OrCombine ( operator || )

***compound parsers***
LeftBinOper
RightBinOper
Postfix
Prefix
Wrapper

# Things that didnt make the first version
- Simplified Iterator usage, currently it uses a start and an end iterator but it would be better to have a single parameter similar to span.
- Basic Parsers and Iterator for handling binary files ( parsing binary structure sounds like something usefull for validating file structure)
- Polishing String/Char handling parsers
- Refine this document
- Add References to some related papers on the subject
- Add more interesting and usefull examples 
