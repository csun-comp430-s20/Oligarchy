# pls look at our documentation.md for requested info
# Language Design Proposal

**Student Name(s):** Eduardo Preciado, Imon Daneshmand, Daniel Cardenas, Jiamin Zhu, Stephanie Contreras

**Language Name:** Oligarchy

**Compiler Implementation Language and Reasoning:** Scala. We are unfamiliar with scala but because it stems from java we believe the benefits of scala such as the pattern matching, functional.

**Target Language:**  Jvm byte code

**Language Description:** We are making this language with the goal of having a lower level language target language so that we can learn more of how things work. We are choosing lazy evaluation because it seems so intuitive yet would like to know how it works. Class based inheritance and subtyping are being chosen because inheritance is one of the major pillars taught when first learning OOP.

**Planned Restrictions:** No optimizations. No this, Outside Variables must be named differently from local. Casting a variable to something that it is not will lead to a runtime error. If you try to have logical operation on numbers due to exp being defined the way it is it will throw a runtime error. We will not be having accessors because of our low level target.

**Syntax:**

**var** is a variable

**classname** is the name of a class

**func** is the name of a method

**int** is an integer

**Boolean** is true or false

**type** ::= Int | Boolean |

classname // class type; includes Object and String

**math** ::= + | - | \* | / | \&lt; | \&gt; |  \&lt;= | \&gt;= Arithmetic operations // something that requires numbers to evaluate

**logic** ::=  &amp;&amp; | || |   ==  // something that needs logic on each side to evaluate

**exp** ::= var | str | i | Variables, strings, and integers are expressions

 Boolean| //

 exp\_1 logic exp\_2|

  exp\_1 math exp\_2 | Arithmetic operations // adds planned restriction

  exp\_1.methodname(exp\_2\*) | Calls a method

  new classname(exp\*) | Creates a new instance of a class

  (type)exp Casts an expression as a type // bad casts are in planned restrictions

 (type var ) =\&gt; exp // variable is in scope and will probably be used in the exp

 exp\_1(exp\_2)// how to call a high order function

**vardec** ::= var  Variable declaration  // add back changes

**stmt** ::= exp;|

  var = exp; | vardec = exp; | Assignment

  for(vardec^; exp;  stmt^) stmt | //^ optional omit them to be treated as a while loop

  { stmt\* } | block

  if (exp) stmt else stmt | if/else

  return exp; | return an expression

  return; | return Void
  print(exp) | Prints something to the terminal


**methoddef** ::= type methodname(vardec\*) stmt //vardecs are comma-separated

**instancedec** ::= vardec; instance variable declaration

**classdef** ::= class classname extends^ classname^ {

   instancedec\*

   constructor(vardec\*) stmt  // vardecs are comma-sep

   methoddef\*

  }   // ^ indicates optional

**program** ::= classdef\* exp //exp is entry point

**Computation Abstraction Non-Trivial Feature:** High order functions  // because we are going to jvm byte code we are choosing high order functions as a computation since jvm already has class and inheritance bytecode

**Non-Trivial Feature #2:** expressions // because we are doing a low level target

**Non-Trivial Feature #3:  ** subtyping

**Work Planned for Custom Component:** Using a low-level target language.



**Tokens**

1. var
2. classname
3. func
4. str
5. int
6. boolean
7. plus
8. subtract
9. multiplication
10. division
11. caret (square operation)
12. greaterThan
13. lessThan
14. greaterThanEquals  // should we handle this at the lexer level or at the parser level.
// ie we can just say in the parser that a greater than token and an equal token will be a greater than equal thing
15. lessThanEquals
16. doubleAnd
17. doubleOr
18. doubleEquals
19. rightCurly
20. leftCurly
21. break
22. semicolon
23. leftParen
24. rightParen
25. for
26. if
27. else
28. class
29. print
30. constructor
31. return
32. period
