# Camel-FunctionalProgramming
## Description
This interpreter is meant to first parse the input into a program of types as defined in "asgparser.mli". The idea is to keep typedefs and valdefs in lists of variable pairs so needed values can be found through using List.assoc.

However, we have only arrived at a partial version. Current issues are as follows:

- Lexer cannot get caret (string concatenation) token. In order to get Concat token, input need to has a space or \n, \t character after the ^ character.
- Parser still cannot parse too complex strings, including function (type -> type), and id expression*
- We haven't came up with a proper interpreter yet apart from binary calculations. Other than that, expression parsing is correct so far as long as the input is according to the syntax.
- Modules can be compiled but output of main file is not running properly.

We will still try to work on these problems.

## Authors:
* Hoa Nguyen
* Thang Nguyen
* Linh Nguyen
* Khang Nguyen 
* Tuan Nguyen
* Khoa Nguyen
