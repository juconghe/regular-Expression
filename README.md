# Regular Expression Derivative Evaluator

## Overview

The goal of this assignment is to implement a regular expression
evaluator library. The evaluator library will provide an interface for
creating regular expression objects and pattern matching against input
strings. The result of the matching operation is simply to return
*true* if the match succeeds and *false* otherwise. You are welcome to
provide any implementation/representation for your regular expressions
as long as you conform to the library interface that we provide.

You should re-read the [notes] on regular expression derivatives
before you begin this assignment.

[notes]: https://github.com/umass-cs-220/week-08-regular-expressions/blob/master/notes/derivative.md

## Setup

First, you need to download the [project starter kit]. This will
download a zip file named
`asn-regex-derivative-student-master.zip`. You should then unzip the
archive and move it into your git repository (do not clone this
assignment from github). After you move it into your git repository
run the necessary `git` commands to add it, commit it, and push it.

[project starter kit]: https://github.com/umass-cs-220/asn-regex-derivative-student/archive/master.zip

## Documentation

You can generate the *scaladoc* documentation from the provided
project source code running the following command from `sbt`:

```bash
$ sbt
> sbt doc
```

This will generate HTML documentation into the directory
`target/scala-X.YZ/api/index.html`. You can then open that
`index.html` file in any browser and read the documentation
easily. You can also read the documentation from the source code.

## Compiling

You can compile the project using `sbt`:

```bash
$ sbt
> compile
```

This project will compile out of the box.

## Testing

You can test the project using `sbt`:

```bash
$ sbt
$ test
```

This will run the provided unit tests using the *ScalaCheck*
framework. The tests that we include can be found in
`src/test/scala/cs220/regex/RegExTestSuite.scala`. The tests conform
to the interface of the regular expression library that you will
implement. We do not depend on your implementation directly, rather
indirection through the libraries public interface.

*Note:* The tests will fail immediately because you haven't
implemented anything yet.

## Provided

Your project is distributed with an `sbt` formatted project
structure. The files that are most important include:

[src/main/scala/cs220/regex/RegEx.scala]: This file defines the
interface for the classes you need to implement representing regular
expressions as well as the interface your library will export to the
client that uses it. In particular, you will notice the `RegEx` and
`RegExLanguage` trait defined in this file.

[src/main/scala/cs220/regex/Factory.scala]: This file is used to
export your implementation of the `RegExLanguage` trait described
below. It offers us nothing more than an entry point into your library
without dictating the names of your files.

[src/main/scala/cs220/regex/RegEx.scala]: src/main/scala/cs220/regex/RegEx.scala
[src/main/scala/cs220/regex/Factory.scala]: src/main/scala/cs220/regex/Factory.scala

### RegEx Trait

The `RegEx` trait defines 6 methods, 4 are abstract and 2 we provide
an implementation for. Here is a summary of these methods:

**delta: RegEx**: The `delta` method implements the &delta; function
defined in the [notes]. That is, it returns the "empty" regular
expression &epsilon; if the regular expression matches "empty" or
&empty; if the regular expression matches "null". Recall that the
empty regular expression &epsilon; matches everything and &empty;
matches nothing.

**isEmpty: Boolean**: This method returns *true* if it matches empty
or *false* otherwise. That is, if we have a regular expression `ab`
then invoking the `isEmpty` method on `ab` would be *false*.

**derivative(c: Char): RegEx**: This method implements the rule for
regular expression derivatives in the [notes]. You will need to
implement each rule for each of the corresponding regular expressions
that you need to create (different classes). The `derivative` method
yields a new regular expression (`RegEx` object) with respect to the
character argument `c`. Note, the derivative rules are exactly that -
they describe rules that *generate a new regular expression* based off
of the current regular expression and some character `c`.

**normalize: RegEx**: This method implements the "evaluation" of a
regular expression after taking its derivative. This method implements
the implicit nature of the theoretical aspect of regular expression
derivatives. You can see how we use it in the `matches` method
described below. That is, after you apply the derivative we must
*normalize* its form with respect to the application of `delta` and
the `derivative`. Here are the rules you need to consider during
normalization (we use N to indicate a call to `normalize`):

1. N(&epsilon;) = &epsilon;
1. N(&empty;) = &empty;
1. N(c) = c
1. N(&empty; re<sub>2</sub>) = &empty;
1. N(re<sub>1</sub> &empty;) = &empty;
1. N(re<sub>1</sub> &epsilon;) = N(re<sub>1</sub>)
1. N(&epsilon; re<sub>2</sub>) = N(re<sub>2</sub>)
1. N(re<sub>1</sub> re<sub>2</sub>) = N(re<sub>1</sub>) N(re<sub>2</sub>)
1. N(re<sub>1</sub>|&empty;) = N(re<sub>1</sub>)
1. N(&empty;|re<sub>2</sub>) = N(re<sub>2</sub>)
1. N(re<sub>1</sub>|re<sub>2</sub>) = N(re<sub>1</sub>)|N(re<sub>2</sub>)

You can see another form of these rules in the documentation for this
method.

**matches(s: String): Boolean**: This method is simple - it calls the
method `toList` on the String `s` and calls `matches`. It is the only
publicly available method from the `RegEx` trait. You do not need to
modify this method.

**matches(cc: List[Char]): Boolean**: This method implements the
*matching rules* which you can find in the [notes]. In particular, it
pattern matches on the list of characters. If we are at the end of the
list (`Nil`) then we check to see if *this* regular expression is
empty by invoking its `isEmpty` method. Otherwise, we have a character
`c` followed by the remaining characters `cc` in the list. As rule
**R2** suggests we then apply the derivative with respect to character
`c`, normalize the resulting regular expression with the `normalize`
method, then recursively call `matches` on the remaining characters in
the list. Thus, we keep matching character by character, taking the
derivative of the regular expression, until we reach the end of the
list of characters - where we then check if *this* regular expression
matches empty (**R1**).

### RegExLanguage Trait

The `RegExLanguage` trait defines abstract methods for each of the
  corresponding regular expression types that can be created. Here is
  a summary of these methods:

**empty: RegEx**: This method returns a regular expression object
representing the "empty" regular expression.

**char(c: Char): RegEx**: This method returns a regular expression
object representing the character regular expression (the regular
expression that matches a single character `c`).

**seq(re1: RegEx, re2: Reg): RegEx**: This method returns a regular
expression that represents a sequence of regular expressions `re1`
and `re2`. Note, to represent a regular expression of a sequence of
characters such as `/foo/` we could do the following:
`seq(char('f'), seq(char('o), seq(char('o), empty)))`.

**alt(re1: RegEx, re2: RegEx): RegEx**: This method returns a regular
expression that represents an alternative regular expression. That is,
a regular expression such as `/a|b/` (matches either "a" or "b"),
which we would create with `alt(char('a), char('b))`.

**str(s: String): RegEx**: This method creates a regular expression
for matching the string `s`. As we mentioned above this can (and
should) be implemented by a sequence of characters.

**closure(re: RegExp): RegEx**: This method implements the closure
regular expression. For example, `/a*/` matches 0 or more `a`'s, which
we would represent as `closure(char('a'))`.

### Factory Object

The `Factory` object contains a single method `re` which returns an
implementation of the `RegExLanguage` trait. You will need to
implement this method as described below.

## Task 1: Create Regular Expression Classes

Your first task is to implement the regular expression classes. That
is, for each of the regular expressions that can be created by the
methods in the `RegExLanguage` trait (except for `str`) you must
create a class that extends the `RegEx` trait. In addition, you will
need to create a class to represent the *null* regular expression
&empty; because you will need to use this as part of the
implementation of `delta` and `normalize`.

The best way to approach any project is to work incrementally. So,
first create the regular expression classes and implement each of the
abstract methods using `???`. You can have `sbt` compiling by your
side at each step of the way to make sure you are writing correct
Scala.

After you have implemented each of the regular expression classes you
must *override* the `toString` method to return a string
representation of the regular expression. Thus,
`alt(closure(char('a')),char('b'))` would be "(a*|b)".

Because we are building a library you must make sure that each of your
regular expression classes are private to the `regex` package. That
is, we should not have access to those classes outside of that
package.

**Do not implement the `isEmpty`, `derivative`, `delta`, and
`normalize` methods at this point.**

## Task 2: Extend RegExLanguage Trait

The next task is to extend the `RegExLanguage` trait with an
implementation of each of the abstract methods. These methods should
use your implementation of the `RegEx` trait from Part 1 to create
proper regular expression objects representing each of the regular
expressions described in Part 1.

You should then modify the `???` implementation of the `Factory.re`
method (in the `Factory.scala` file) to return an instance of you
implementation. In doing so, you have provided public access to those
functions are can start creating your regular expressions in the
*repl*:

```scala
> console
scala> import cs220.regex._
scala> val re = Factory.re
scala> re.char('f')
scala> re.str('foo')
```

**Note:** it might be best to implement each method one-by-one and
test them in the *repl* to make sure they are working properly. You
may want to add some debugging methods to your Factory that will print
out the structure of your regular expressions to make sure you have
created them properly. Do not change anything to public in the
provided code as we will be checking to make sure that you adhere to
the access rights of your library.

## Task 3: Implement Regular Expression Classes

At this point you should be able to construct regular expressions
through the regular expression library API. Now, we need to implement
matching. To do this you must implement each of the corresponding
abstract methods in the `RegEx` trait in your regular expression
classes. You should implement each of the methods in the order
described below.

**isEmpty: Boolean**: This method is simple. You should simply return
a boolean for each regular expression that indicates if it is "empty" or
"null".

**delta: RegEx**: Use the rules defined in the [notes] for the &delta;
function to implement this method for each of your regular expression
classes (including *null*). Most of the implementations are extremely
simple. The implementation for sequences and alternative require some
additional work to determine if they are "empty" or "null".

**derivative(c: Char): RegEx**: This is the hardest function to
implement. However, if you make sure you understand the rules covered
in the [notes] you will be able to construct the proper regular
expressions given the rules. Remember, all you are doing in this
method is creating a new regular expression that represents the
derivative of the regular expression you are invoking the `derivative`
method on. Again, the most complicated of these are the sequence and
alternative regular expressions. Thus, you should start with the
simply ones first. Perhaps add some debugging methods in your
`Factory` object to help with debugging purposes (as this is the only
way to gain access to the internals). Again, do not modify the access
modifiers on any of the provided code.

**normalize: RegEx**: If you follow the rules outlined above for
normalization you should not have too much trouble with this
method. This method is simplifying the regular expression based on the
result of taking the regular expression's derivative. Again, start
with the simple rules first, add some debugging support, and do not
modify the public API that is provided.

**Notes:** You should test your code step-by-step to ensure that it is
working properly. Do not write all of this at once and then test - it
will be too confusing and harder to debug. You should implement the
simple regular expressions first and then run the unit tests to see if
it works on the simple cases. You can also test your implementation
from the console. For example, implement the "empty" regular
expression first and test to see if matching works:

```scala
> console
scala> import cs220.regex._
scala> val re = Factory.re
scala> re.empty matches ""
res0: Boolean = true
scala> re.empty matches "x"
res1: Boolean = false
```

Then try to see if the character regular expression works:

```scala
scala> re.char('c') matches "c"
res2: Boolean = true
scala> re.char('c') matches "x"
res3: Boolean = false
scala> re.char('c') matches "cc"
res4: Boolean = false
```

Continue in this fashion implementing and testing your code along the
way. In the end, you should run the unit tests and verify that all of
the provided tests pass.

## Task 4: Submit Your Work

You should make frequent git adds, commits, and pushes as you do your
work. You should make sure that your final implementation has been
pushed to the remote git server by the assigned due date.

