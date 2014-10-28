Learn Symta by Example


Table of Contents
------------------------------
- What is Symta?
- Installing Symta
- Printing Text on Screen
- Variables and Arithmetics
- Functions
- Lists
- Conditional Evalutation
- Looping
- Object Oriented Programming
- Pattern Matching
- Syntatic Sugar
- Module System
- Macros
- Memory Management
- Non-local Return
- Command-Line Arguments
- Core Library
- Comparison to Other Languages



What is Symta?
------------------------------
Symta is a novel dialect of Lisp programming language. Symta features succinct syntax, geared towards list-processing, and innovative approach to memory management, without requiring pause-inducing garbage collection or manual deallocation.

An example of Symta's expressiveness could be the implementation of quick sort algorithm, which takes just single line of Symta code
qsort@r$[] [H@T] = [@T.keep{?<H}^r H @T.skip{?<H}^r]

Despite a lot of consing, calling the qsort function doesn't generate garbage.


Installing Symta
------------------------------
Symta can be installed by downloading and unpacking the latest release zip file:
https://github.com/saniv/symta-releases

On Unix extract the zip archive, move to the extracted directory and type "bash build.sh". This should bootstrat the compiler, producing ./symta executable.

NOTE: currently Symta relies on GCC to produce native executables. Make sure your PATH variable includes path to gcc's bin folder. On Windows it is adviced to install GCC through mingw64. Currently there are no plans supporting 32-bit architectures, unless someone provides help.


Printing Text on Screen
------------------------------
To start with Symta, create a file main.s with the following content:
```
say "Hello, World!" //put text on screen
```

Now invoke Symta compiler by typing at command line:
```
symta main.s
```

Note: this initial file should be called exactly `main.s`.

That creates `./lib` folder and, more importanly, `./run` executable. Running it produces the following output:
```
Hello, World!
```

That is because `say` is a function, that prints on screen its argument, in our case `"Hello, World!"`. The `//` denotes a comment.

If text is just a single lowercase word, there is no need escaping it. I.e. you can write just
```
say hello
```

Variables and Arithmetics
------------------------------
Symta supports variables and all the basic arithmetics. The example would be main.s file:
```
A = 123 // declare first variable
B = 456 // declare second variable
say "addition: [A+B]"
say "multiplication: [A*B]"
say "division: [B/A]"
say "remainder: [B%A]"
say "exponentiation: [A**3]"
say "average: [(A+B)/2]" // change operator precedence by using `(` and `)`
A <= 789 // assign a new value to A
say "Now A contains [A]"
!B * 3 // multiply B by 3
say "Now B contains [B]"
```

Above example declares two variables, `A` and `B` (bound to 123 and 456 respectively), and then shows some arithmetics with them. Declaring A second time would hide its previous version for the code, bellow the new declaration, but doesn't affect the code above or outside of current scope (more about Symta's scoping rules later). The operator `<=` is used when previously declared variable is really have to be reassigned for all code, that sees it. The operator `!` stores expression's result back to variable it prefixes, so `!B*3` is the same as `B <= B*3`.

Note that all variable names must start from an uppercase letter, while function names start with anything non-uppercase. Such notation makes code more readable and simplifies syntax.

Of course Symta has comparison operators. These return 1 on true and 0 on false. Here they are
```
A><B // 1 if A equals B, else 0
A<>B // 1 if A doesn't equal B, else 0
A<B // 1 if A is below B, else 0
A>B // 1 if A is above B
A<<B // 1 if A is below or equal B
A>>B // 1 if A is above or equal B
```

The operator `not` gives 1 for 0 and 0 for anything else

The empty expression, `()`, would produce value No, which denotes absence of value, similar to null entries in database or NIL value in other Lisps.

Additionally, there are floating point numbers:
```
PI = 3.14159265
say 2.0*PI
```

Note that `2*PI` would produce error, because Symta is strongly typed language and avoids implicit conversion. You must convert you integers to floats, before using them in expressions involving floats. That can be done by invoking method `float` on integer, like that:
```
say PI*IntegerNumber.float
```

See "Object Oriented Programming" to learn more about Symta's implementation of methods and objects.


Functions
------------------------------
Good code is flexible and adaptable. The previous 'hello'-program can be improved to this:
```
greet Name = "Hello, [Name]!"
greet "World"
```

Compiling and running it would produce the same output:
```
Hello, World!
```

Yet the code is different: `greet Name =` declares function `greet`, which takes one argument `Name`; the body of this function, "Hello, [Name]!", is a string construction expression, where `[Name]` denotes that we want to put there the value of Name. The second line, `greet "World"`, invokes the declared function `greet`.

The function argument `Name` behaves just like variables - it is a memory cell, not constants, like mathematical or Haskell's variables. So we can reassign it, using `<=` and hide using the following variable declaration.

There is an alternative {}-based syntax for function calls:
```
say{"Hello, World!"}
```

Factorial would be an example of recursive function (a function calling itself):
```
factorial N = if N >< 0 then 1 else N*factorial{N-1}
```

Symta's functions support keywords:
```
greet name/"World" = "Hello, [Name]!"
greet
greet name/"Symta"
```

Here `Name` became an optional argument, by default bound to "World".

Symta uses {} instead of (), because otherwise `say(hello)` would be indistinguishable from `say  (hello)` - function say taking the result of calling function hello. The {}-syntax also provides additional sugar for lambdas.

In many cases it makes sense to specify function in Forth-style - after the arguments. Symta provides operator `^` to do that:
```
"Hello, World!"^say
```

The `^` is useful, when a lot of calls get chained together left-to-right, so you wont have to think in advance or read right-to-left.

Lists
------------------------------
After all, Symta is a list processing language, so it provides succinct syntax to express lists and operations on them.
```
Xs = [hello world 123] // create a simple list with three values
Ys = [an other list]
Zs = [@Xs @Ys] // Zs would be concatenation of two lists
[456 @!Ys 789]

say "Xs: [Xs]"
say "Ys: [Ys]"
say "Zs: [Zs]"
```

As always Symta doesn't requre quotes around lowercase symbols, therefore [a list of bare words] is perfectly valid. The `[456 @!Ys 789]` adds two numbers to both ends of `Ys` and stores the result back into `Ys` (remember the `!`)

More basic way to access lists elements would be through index operator - `.`:
```
Xs = [a b c d e f]
say Xs.3 // prints `d`, because indices start from 0
Xs.3 <= 123 // replaces `d` with 123
say "changes Xs is [Xs]"
```

Lists support vector arithmetics:
```
say ([1 2 3]+[4 5 6])*2
```

Using lists as vectors is relatively efficient, because internally lists are implemented as arrays.

Symta also has the `mtx` macro used to express matrices, which are lists of lists:
```
Identity = mtx | 1 0 0
               | 0 1 0
               | 0 0 1 

say "Identity matrix is [Identity]"
```

Conditional Evalutation
------------------------------
Other basic construct is the conditional if/then/else:
```
say if 1 then 123 else 456        // prints 123
say if 0 then 123 else 456        // prints 456
say if 666 then 123 else 456      // prints 123
say if 'hello' then 123 else 456  // prints 123
```

Any non-zero value is true and triggers `then` clause, while zero would trigger the evaluation of value after `else`.


When else clause is empty, Symta's standard library provides two shorthand macros:
```
when C: say 'C <> 0'  // same as if C then say 'C <> 0' else
less C: say 'C >< 0'  // same as if C then  else say 'C >< 0'
```

The operator `:` is yet another shorthand to avoid excessive nesting of `(` and `)` in function calls. So the `when C: say hello` is actually an equivalent of `when C (say hello)`

Several logical expressions can be joined by using `and` and `or` operators:
```
when A and B: say "Both A and B are true"
when A or B: say "Either A or B is true"
```

Here the operator `:` is mandatory and specifies the evaluation order, because (when A and B say hello) would be evaluated as `when A and (B say hello)`



Looping
------------------------------
Repeating expression or running it through a list of elements is a common task. Symta provides several constructs to handle that:
```
times I 20: say I // iterate ove numbers below 20 and print them
Xs = [1 2 3 4 5 6 7 8 9]
for X Xs: say X*X // print squares of Xs elements
Ys = map X Xs: X*X // collect the same squares for future use
say "Ys = [Ys]"
```

Using macro `dup` is the way to quickly crate new list out of nothing. Its first argument is a loop variable, the second is the size of list, the third is an expression used to generate an element of list based on its position. It supports two shorthands:
```
say: dup I 10 I // sequence of all numbers below 10
say: dup N // equivalent to: dup I N 0
say: dup N: X // equivalent to: dup I N X
```

Symta also provides simpler and more verbose loop constructs, like gotoes, while and till:
```
I = 0
while I < 10
| say I
| !I + 1
```

Here the `|` acts like off-side rule or semicolon with curly braces in other languages. The `|`s at the same indentation level are take as single sequence of statements.

The `map` functionality also exists as a method `{}`:
```
Xs = dup I 10 I
say Xs{X => X*X}
```

The `=>` is the lambda operator, which expresses anonymous function. In our case it take argument `X` and returns `X*X`. In case of `{}`, there is a shorthand for that: `Xs{?*?}`, which is equivalent to `Xs{X => X*X}`



Object Oriented Programming
------------------------------
Symta has unique approach to type system. The most striking difference is that Symta mixes class-based and prototype based OOP approaches. Compared to other dynamic languages, Symta's method calls are a lot more efficient, due to use of vtables, instead of hash-tables; yet Symta allows putting methods and values into a hash-table and using it like Lua, Python or JavaScript do.

Typical OOP example would be geometric point type:
```
type point x y
point.as_text = "[$x], [$y]"
P = point
P.x <= 123
P.y <= 456
say "created a point: [P]"
```

The expression "type point x y" does several things: register's new type `point` with Symta's runtime and provides constructor function `point`, which creates instance of `point` type, with fields `x` and `y` - both initialized to 0. The `point` type already has method `is_point` defined on it, and all other objects too gain this method. The `point.as_text` declares a method, invoked by functions like `say` to get textual representation of objects. The $x and $y are shorthands for Me.x and Me.y, where Me is a way to reference object inside of a method, similar to `this` pointer in C++ and `self` in Smalltalk.

If method takes arguments, they can be specified using `{}`. For example:
```
point.set_x_and_y A B = | $x <= A
                        | $y <= B
P.set_x_and_y{666 777}
```


Note: when type declaration is available, Symta compiles the Me.field_name call to an array look-up, which is somewhat faster than function call.

Initializing fields with `<=` is too verbose, so symta provides a shorthand for that, which allows to rewrite previous example more succinctly:
```
type point{X Y} x/X y/Y
point.as_text = "[$x], [$y]"
P = point 123 456
say "created a point: [P]"
```

Now constructor `point` takes two arguments X and Y and assigns them to the respective fields. Expressing ideas with a few keystrokes is useful, but there are cases when you need to add more stuff into constructor, like for examply notifying the user when each point is created for debug purposes. To do that Symta allow adding this `|` body to constructor:
```
type point{X Y} x/X y/Y | say "created a point: [Me]"
point.as_text = "[$x], [$y]"
P = point 123 456
```

Now, after initializing the fields, constructor evaluates its body - the `| say "created a point: [Me]"`.

Sometimes we can reuse pre-existing functionality. To do that Symta provides inheritance. Here is how we can declare a `circle` type, which extends point with a radius around it:
```
type circle{X Y R} base/point{X Y} radius/R
circle.as_text = "point [$base] with radius [$radius]"
heir circle $base
```

The `heir` keyword makes newly declared type `circle` to inherit all the methods $base provides (which holds point value), so now both methods is_point and is_rect return true on it. Note that we inherit from prototype object as opposed to class. Instead of $base, we can pass any value to `heir` and it will still work.


The other way to inherit methods is to use interfaces. Say you want to provide point with methods previously defined on type `list`:
```
type point.list{X Y} x/X y/Y 
point.head = $x
point.tail = [$y]
point.end = 0
```

Now point has all methods `list` has, like `map` and `sum`. The methods `head`, `tail` and `end` are required for all types implementing list functionality.


Functionality presented above should be enough for most uses, but sometimes we need even more flexibility, like when you want to delegate all undefined method calls to an object running on the remote server through an RPC interface. Symta allows that:
```
point._ Method Args = send_to ServerIP ServerPort Method Args
```

Now all undeclared methods get redirected to send_to. This tehnique is called 'sinking'. Although doing that would be a little overkill for a humble `point` type.

When you want to make a method available to all type, declare it with `_` in place of type name:
```
_.get_my_typename = Me^typename
```

The type `_` denotes the default parent of all objects. But there is a way to declare object without any parent. Use `~` instead of inheritance name.
```
type point.~{X Y} x/X y/Y
```

Now type `point` doesn't inherit any methods from `_`. This is especially useful, when you wan't to catch all methods as undefined.


Pattern Matching
------------------------------
Lists pose a problem of transforming them and accesing their elements in a succinct and robust way. Symta was designed specially to process lists efficiently. An example shows how Symta handles accessing list elements:
```
Xs = [1 2 3]
[X Y Z] = Xs // X=1, Y=2, Z=3
[A @As] = Xs // A=1, As=[2 3]
[@Bs B] = Xs // B=3, Bs=[1 2]

say "X, Y, Z = [X], [Y], [Z]"
say "A=[A], As=[As]"
say "B=[B], Bs=[Bs]"
```

Expressions like `[A@As]=Xs`, with just `[`, `]`, variables and no other literal values, are the simple case of pattern matching, called 'destructuring' - an advanced version of variable declaration. Yet pattern matching can match literal values and include more than one `@`:
```
Xs = [we have a needle in the middle]
[@Ys needle @Zs] = Xs
say "Ys = [Ys]"
say "Zs = [Zs]"
```

Here `[@Ys needle @Zs]` splits Xs on the needle, giving two lists: Ys and Zs

Function arguments can be destructured the same way:
```
vector_length [X Y] = @sqrt X*X + Y*Y
```

But what if a value doesn't match the pattern or we want to match against several patterns? To solve that, Symta has macros `case`:
```
case Xs
  [Y<a+b+c @Ys] | "Xs's head is [Y], which is one of a, b or c"
  Else | "some other value: [Else]"
```

The operator `<` in the pattern expression is used to to match several patterns to a single value. The `+` in case of pattern means `or`, so `A+B` means that value matches either `A` or `B`.

If no `Else` handler is present, then 0 will be returned. This allows for combining `case` with `when`:
```
when case Xs [A B C] 1: say "Xs is a list of three elements".
```

Another example declares a function, that checks if input is a palindrome - sequence invariant under reversing (like `noon` or `racecar`):
```
palindrome Xs = case Xs.list
  [S @Xs &S] | palindrome Xs
  []+[X] | 1
```

`Xs.list` converts input into a list, if it is text. The example introduces yet another pattern-matching operator - `&`, which is used to compare with previously bound variables or just an arbitrary expression. The `[]+[X]` matches against empty list or a list of single element.

Symta's pattern matching can also be applied to strings, solving the same problems regular expressions solve
```
Filename = "song.mp3"
Name = case Filename "[N].mp3": N // matches a file with .mp3 extension and strips it.
say Name // print "song"
```

A more advanced example of pattern matching would be parsing a binary file with typical FourCC tagged chunks:
```
Chunks = File.get^| @r$[] [4/T.utf8 4/L.u4 L/D @Xs] => [[T D] @Xs^r]
```

That expression may look cryptic at first, because it packs a lot of stuff into single line. Here is what it does:
```
`File.get` - retrives the content of File as a list of bytes
A^B - syntatic sugar for applying a function B to value A (same as `B A` or B{A})
`| @r$[] Args => Body` is a lambda expression
`@r` - allows to self reference the lambda inside of the Body by name `r`
`$[]` - makes [] a default value, returned if input doesn't match the pattern [4/T.utf8 4/L.u4 L/D @Xs]
```

The most complex part is `[4/T.utf8 4/L.u4 L/D @Xs]`, which binds `T` to the value of the first 4 bytes decoded as utf8 text, then binds `L` to the next four bytes conveted to 32-bit unsigned integer, afterwards D gets binded to a list of `L` bytes, that follows the 8 already parsed bytes. The `Xs` is bound to the rest of bytes.

Finally, the body of lambda produces `[[T D] @Xs^r]`, where `[T D]` is the tag of parsed chunk together with its contents, and `@Xs^r` calls the lambda recursively on the unparsed bytes and prefixes `[T D]` to result.

It should be noted, that `utf8` and `u4` are simply methods defined on list type, so you can declare you own methods to work with pattern matching and even pattern-match non-list objects.

