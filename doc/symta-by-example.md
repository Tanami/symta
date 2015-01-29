# Learn Symta by Example


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
- Module System
- Macros
- Symta's Approach to Memory Management
- Non-local Return
- Foreign Function Interface
- Comparison to Other Languages
- Core Library
- Thanks


What is Symta?
------------------------------
Symta is a novel dialect of Lisp programming language. Symta features succinct syntax, geared towards list-processing, and innovative approach to memory management, without requiring pause-inducing garbage collection or manual deallocation.

An example of Symta's expressiveness could be the implementation of quick sort algorithm, which takes just a single line of Symta code:
```
qsort@r$[] H,@T = @T.keep{?<H}^r,H,@T.skip{?<H}^r
```

Despite a lot of consing, calling the qsort function doesn't generate garbage.

Symta allows very tight code. The above ``qsort`` example beats in brevity even the kings of conciseness - APL and J:
```
qsort=: (($:@(<#[), (=#[), $:@(>#[)) ({~ ?@#)) ^: (1<#)
```

Yet, compared to J, Symta's ``qsort`` is arguably more readable than Haskell's version:
```
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]
```


Installing Symta
------------------------------
Symta can be installed by downloading and unpacking the latest release zip file:
https://github.com/saniv/symta-releases

On Unix extract the zip archive, move to the extracted directory and type "bash build.sh". This should bootstrap the compiler, producing ./symta executable.

NOTE: currently Symta relies on GCC to produce native executables. Make sure your PATH variable includes path to gcc's bin folder. On Windows it is advised to install GCC through mingw64. Currently there are no plans supporting 32-bit architectures, unless someone provides help.


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

Note that `2*PI` would produce error, because Symta is strongly typed language and avoids implicit conversion. You must convert your integers to floats, before using them in expressions involving floats. That can be done by invoking method `float` on integer, like that:
```
say PI*IntegerNumber.float
```

See "Object Oriented Programming" to learn more about Symta's implementation of methods and objects.


Functions
------------------------------
Good code is flexible and adaptable. The previous 'hello'-program can be improved to this:
```
greet Name = say "Hello, [Name]!"
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
greet name/"World" = say "Hello, [Name]!"
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
After all, Symta is a list processing language, so it provides a concise way to express lists and operations on them.
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


When either then or else clause is empty, Symta's standard library provides two shorthand macros:
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

Another secret, the `:` operator hides, is the subexpression binding:
```
Words = No
get_words = Words

when got !it get_words: say it
Words <= [a list of words]
when got !it get_words: say it
```
The `got` checks if value doesn't equal `No`, while `!it` binds the result of get_words to `it` (before passing it to got), and exposes this variable for the right hand side of `:`. Note that variable name before `!` should start from a lowercase letter, liket `it`.


Looping
------------------------------
Repeating expression or running it through a list of elements is a common task. Symta provides several constructs to handle that:
```
times I 20: say I // iterate over numbers below 20 and print them
Xs = 1,2,3,4,5,6,7,8,9 // another way to specify list
for X Xs: say X*X // print squares of Xs elements
Ys = map X Xs: X*X // collect the same squares for future use
say "Ys = [Ys]"
```

Using macro `dup` is the way to quickly create a new list out of nothing. Its first argument is a loop variable, the second is the size of list, the third is an expression used to generate an element of list based on its position. It supports two shorthands:
```
say: dup I 10 I // sequence of all numbers below 10
say: dup N // equivalent to: dup I N 0
say: dup N: X // equivalent to: dup I N X
```

Symta also provides simpler and more verbose loop constructs - gotoes (see Macros section), `while` and `till`:
```
I = 0
while I < 10
| say I
| !I + 1
```

Here the `|` acts like off-side rule or semicolon with curly braces in other languages. The `|`s at the same indentation level are taken as a single sequence of statements.

The `map` functionality also exists as a method `{}`:
```
Xs = dup I 10 I
say Xs{X => X*X}
```

The `=>` is the lambda operator, which expresses anonymous function. In our case it take argument `X` and returns `X*X`. In case of `{}`, there is a shorthand for that: `Xs{?*?}`, which is equivalent to `Xs{X => X*X}`



Object Oriented Programming
------------------------------
Symta has unique approach to type system. The most striking difference is that Symta mixes class-based and prototype-based OOP approaches. Compared to other dynamic languages, Symta's method calls are a lot more efficient, due to use of vtables, instead of hash-tables; yet Symta allows putting methods and values into a hash-table and using it like Lua, Python or JavaScript do.

Typical OOP example would be geometric point type:
```
type point x y
point.as_text = "[$x], [$y]"
P = point
P.x <= 123
P.y <= 456
say "created a point: [P]"
```

The expression "type point x y" does several things: registers new type `point` with Symta's runtime and provides constructor function `point`, which creates instance of `point` type, with fields `x` and `y` - both initialized to 0. The `point` type already has method `is_point` defined on it, and all other objects too gain this method. The `point.as_text` declares a method, invoked by functions like `say` to get textual representation of objects. The `$x` and `$y` are shorthands for `Me.x` and `Me.y`, where `Me` is a way to reference object inside of a method, similar to `this` pointer in C++ and `self` in Smalltalk.

Note: when type declaration is available, the `Me.field_name` call compiles to an array look-up, which is somewhat faster than a function call.


If a method takes arguments, they can be specified using `{}`. For example:
```
point.set_x_and_y A B = | $x <= A
                        | $y <= B
P.set_x_and_y{666 777}
```

If you dislike `{` and `}`, there is a function-style syntax to invoke a method:
```
@set_x_and_y 666 777 P
```

Initializing fields with `<=` is too verbose, therefore symta provides a shorthand for that, which allows writing a tighter version of previous example:
```
type point{X Y} x/X y/Y
point.as_text = "[$x], [$y]"
P = point 123 456
say "created a point: [P]"
```

Now constructor `point` takes two arguments `X` and `Y` and assigns them to the respective fields. Expressing ideas with a few keystrokes is useful, but there are cases when you need to add more stuff into constructor, like for examply notifying the user when each point is created for debug purposes. To do that Symta allow adding this `|` body to constructor:
```
type point{X Y} x/X y/Y | say "created a point: [Me]"
point.as_text = "[$x], [$y]"
P = point 123 456
```

Now, after initializing the fields, constructor evaluates its body - the `| say "created a point: [Me]"`.


For simple structure Symta provides initialization shorthand
```
type point{x y}
```

which is a complete equivalent to more verbose `type point{X Y} x/X y/Y`


Sometimes we can reuse pre-existing functionality. To do that, Symta provides inheritance. Here is how we can declare a `circle` type, which extends point with a radius around it:
```
type circle.$base{X Y R} base/point{X Y} radius/R
circle.as_text = "point [$base] with radius [$radius]"
```

The `$base` makes newly declared type `circle` to inherit all the methods the $base field provides (which holds point value), so now both methods is_point and is_rect return true on it. Note that we inherit from prototype object as opposed to class. Instead of $base, we can specify any value and it will still work.


The other way to inherit methods is to use interfaces. Say you want to provide point with methods previously defined on type `list`:
```
type point.list{X Y} x/X y/Y 
point.head = $x
point.tail = [$y]
point.end = 0
```

Now point has all methods `list` has, like `map` and `sum`. The methods `head`, `tail` and `end` are required for all types implementing list functionality.


Functionality presented above should be enough for most uses, but sometimes we need even more flexibility, like when you want to delegate all undefined method calls to an object running on some remote server through an RPC interface. Symta allows that:
```
point._ Method Args = send_to ServerIP ServerPort Method Args
```

Now all undeclared methods get redirected to send_to. This technique is called 'sinking'. Although doing that would be a little overkill for a humble `point` type.

When you want to make a method available to all type, declare it with `_` in place of type name:
```
_.get_my_typename = Me^typename
```

The type `_` denotes the default parent of all type. But there is a way to declare a type without any parent. Use `~` instead of inheritance name.
```
type point.~{X Y} x/X y/Y
```

Now type `point` doesn't inherit any methods from `_`. This is especially useful, when you wan't to catch all methods as undefined or supply your own master type.


To get all methods exported by an object, the `methods` function could be used:
```
say point{123 456}.methods //shows methods provided by point's instance
```

Copy constructor methods `copy` and `deep_copy` both make field-by-field copies of an object, with the difference being that `deep_copy` method call itself recursively on each field.


Pattern Matching
------------------------------
Lists pose a problem of transforming them and accessing their elements in quick and robust way. Symta was designed specially to process lists efficiently. An example shows how Symta handles accessing list elements:
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

Here `[@Ys needle @Zs]` splits `Xs` on the `needle`, giving two lists: `Ys` and `Zs`

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
Chunks = File.get^| @r$[] [4/T.utf8 4/L.u4 L/D @Xs] => [T,D @Xs^r]
```

That expression may look cryptic at first, because it packs a lot of stuff into single line. Here is what it does:
- `File.get` - retrives the content of File as a list of bytes
- `A^B` - syntatic sugar for applying a function `B` to the value of `A` (same as `B A` or `B{A}`)
- `| @r$[] Args => Body` is a lambda expression
- `@r` - allows to self reference the lambda inside of the Body by name `r`
- `$[]` - makes `[]` a default value, returned if input doesn't match the pattern `[4/T.utf8 4/L.u4 L/D @Xs]`

The most complex part is `[4/T.utf8 4/L.u4 L/D @Xs]`, which binds `T` to the value of the first 4 bytes decoded as utf8 text, then binds `L` to the next four bytes conveted to 32-bit unsigned integer, afterwards D gets binded to a list of `L` bytes, that follows the 8 already parsed bytes. The `Xs` is bound to the rest of bytes.

Finally, the body of lambda produces `[T,D @Xs^r]`, where `@Xs^r` calls the lambda recursively on the unparsed bytes, after which `T,D` gets prefixed to the result of `@Xs^r`. The `T,D` (a shorthand for [T D]) is the tag of parsed chunk together with chunk's content (a list of bytes).

It should be noted, that `utf8` and `u4` are simply methods defined on list type, so you can declare you own methods to work with pattern matching and even pattern-match non-list objects.


Now we can understand the earlier quick-sort example:
```
qsort@r$[] H,@T = @T.keep{?<H}^r,H,@T.skip{?<H}^r
```

Again `@r` - makes a shorthand synonym `r` for `qsort`, `$[]` provides a default value (`[]`), when input doesn't match `H,@T` - i.e. a non-list or an empty list. The `H,@T` binds the first element of the input list to `H`, while the list's tail gets bound to `T`. In the body we have A=`T.keep{?<H}^r` (keep all elements less than `H` and apply `qsort` recursively), B=`T.skip{?<H}^r` - same but with skipped elements, finally we concatenate A and B, with `H` in the middle `@A,H,@B`, resulting into elements smaler than `H` being below it and larger or equal to `H` being above it. That example isn't the fastest quick-sort implementation, but it is fast enough for most uses, yet requires little effort to write in Symta.



Module System
------------------------------
With Symta each `.s` file is a module in itself. By default `.s` files import system modules `rt_`, `core_` and `macro_`, which provide all the basic functions, methods and macros. But there are additional modules to put graphics on screen and evaluate Symta's code during runtime.

Available modules can be imported using the `use` keyword. For example, here is how the module `reader` could be exposed to the current code:
```
use reader

say: @parse '(A+B)/C+1' //parse expression into syntax tree
```

The `text.parse` method comes from the `reader` module and won't be available, unless you `use reader`. That is because initializing reader structures, when you don't need it, wastes time and memory.

Note, that `use` keyword should come as the first line in the file.

Of course you can write your own custom modules. All that required is to write an `.s` file and add `export` keyword at the end, to specify what you want to export. For example, the `greet` code from the first example can be packed into a separate `module.s` file
```
greet Name = say "Hello, [Name]!"
export greet
```

now `main.s` in the same directory uses `module.s` like that:
```
use module
greet "World"
```

By default, Symta's comiler searches for modules in the compiler's `./src` directory and the directory of the file, that tries to use the module. During compilation, Symta traces dependencies and recompiles all changed modules. Don't call your modules `main.s`, because `main` is reserved for the entry module, which gets called on startup by runtime.



Macros
------------------------------
Macros provide a way to do computation at compile time, generate code algorithmically and declare your own statements, like `mtx`, `when` or `while`. Before use, macros have to be declared in a separate module. Here is how you can define a module with the `pi` macro, which provides a well know math constant multiplied by some value:
```
pi Multiplier = Multiplier*3.14159265
export 'pi'
```

Note how we export `pi` like any other function, but inside thr `''` quotes. Now calling the `pi 2.0` would produce `6.28318530` at compile time, without wasting any runtime resources.

Macros get their arguments unevaluated, as is, so `pi [1.0 2.0]` would produce a compile-time error. Yet such behaviour is benefical, because it allows us to transform unevaluated arguments on the syntax level, using arbitrary rules. Here is how a custom version of `when` macro can be defined:
```
my_when @Cond Body = ['if' Cond Body No]
```

Above macro returns a list, which is the actual representation of Symta's code for `(if Cond then Body else No)`, after it gets parsed inside of memory.

To simplify writing macros, there is the `form` macro, used to generate code without messy escaping with `''` quotes, explicit lists and manually creating unique variables names:
```
my_when @Cond Body = form: if Cond then Body else No
```

Neither Symta's core, nor your computer's central processing unit know about `map` or `while` loops. The only looping capability CPU has is the goto instruction (sometimes called "jump" or "branch"). The next common macro example is the definition of the while loop through the goto instruction:
```
my_while @Head Body = form: _progn (_label ~L)
                                   (if Head
                                    then _progn Body (_goto ~L)
                                    else No)
```

The example uses the three lowest level Symta's constuct, rarely used normally; they are: `_progn`, `_label` and `_goto`. The `_progn` is a low-level version of `|`, which doesn't allow variable declarations, lambdas, local functions and other fance stuff. The only special thing `_progn` allows inside is the `(_label LabelName)` construct, which declares a label, that can be referenced by a local `_goto` around it. In case of `my_while`, the `LabelName` is `~L`, where the `~` prefix specifies that label name must be unique to avoid conflicts with other variable names; such functionality is called "auto-gensym" in other Lisps.


Symta's Approach to Memory Management
------------------------------
The major problem of functional programming languages is the construction of many temporary data structures, which makes manual memory management impractical. Thus arises requirement for automatic memory management, usually implemented through heap and garbage collection process (GC), which determines obsolete data and frees its memory. Heap-based GC processes are complex, requiring suspending execution and copying large chunks of memory to avoid fragmentation, which impedes allocation of large continuous data structures. To avoid pauses, modern GCs uses generations, but generations are still unreliable, uncontrollable by user and poorly map to memory usage.

Symta's motto for memory management is "no heap - no garbage". By using just stack we can compact incrementally and use stack frames naturally as garbage collector generations: when a function leaves a stack frame, all frame's data is freed or compacted to parent frame. A good example would be webserver's `render_html_page` function, allocating a lot of per frame data, which won't be needed for the next http request and therefore can be safely thrown away. Stack provides explicit, intuitive and predictable way to manage memory.


Non-local Return
------------------------------

Foreign Function Interface
------------------------------
Symta provides two approaches to FFI. The first uses `ffi_load PathToLibrary SymbolName` to get raw pointer to a library symbol, which then can be called through the  builtin operation `_ffi_call (ResultType @ArgsTypes) Pointer @Args`. That builtin can be used to invoke any C/C++ function using integer pointer, not just loaded through `ffi_load`.

The second approach uses `ffi_begin` and `ffi` macros, which provide simplified interface to `ffi_load` and `_ffi_call`. The `ffi_begin LibraryName` call requires shared library to be present under as `./ffi/LibraryName/lib/main`, which would be automatically copied to the program's `lib/` folder during compilation.

The following example imports symbol `new_gfx` from the `gfx` dynamic library as `creat_new_gfx` macro (which would be expaned to `_ffi_call` during compilation):
```
ffi_begin gfx
ffi creat_new_gfx: new_gfx.ptr Width.u4 Height.u4
```

The `new_gfx.ptr` points compiler, that call to `new_gfx` returns `ptr` FFI type, while `Width.u4` and `Height.u4` declare arguments to `new_gfx` as 32-bit (4-byte) uinsigned integers

Here is the list of currently supported native types
- `ptr` - pointer (all pointers must be aligned to 3 bytes)
- `void` - no return (calling it would produce `No`)
- `int` - int type
- `s4` - 32-bit signed integer
- `u4` - 32-bit unsigned integer
- `float` - 32-bit floating point number
- `double` - 64-bit floating point number
- `text` - char* string (will be converted to Symta's text type)


Comparison to Other Languages
------------------------------
Compared to other languages, Symta provides macro-system to extend the language and do meta-programming. Some thoughts were put into avoiding verbosity and bloated constructs; for example, Symta has some syntatic sugar and shorthands missing from other Lisps, while predefined identifier names were picked based on length in characters. That is how Symta got "flip" instead of "reverse" and "say" instead of "print".

Here is an OOP example from Wikipedia's Ruby article rewritten into Symta:
```
type person{name age}
person.`<` X = $age < X.age
person.as_text = "[$name] ([$age])"

Group = ['Bob',33 'Chris',16 'Ash',23].map{(person @?)}

say Group.sort.flip
```

Compare it to the original Ruby code:
```
class Person
  attr_reader :name, :age
  def initialize(name, age)
    @name, @age = name, age
  end
  def <=>(person) # the comparison operator for sorting
    age <=> person.age
  end
  def to_s
    "#{name} (#{age})"
  end
end
 
group = [
  Person.new("Bob", 33),
  Person.new("Chris", 16),
  Person.new("Ash", 23)
]
 
puts group.sort.reverse
```

As you probably noticed, Ruby's code has a lot of annoying boilerplate, like ``attr_reader :name, :age``, ``def initialize`` and "end".


Core Library
------------------------------
This section provides a quick reference of the content of cors_.s and rt_.s files.

``t Key0(Value1) Key1(Value1) ... KeyN(ValueN)`` - new hashtable

`Table->field` - when `Table.field` is `No`, initializes Table.field to `t{}`, else returns the existing value of `Table.field`. Similar to Perl's autovivification.

`leave Value` - leaves current function, returning Value

`leave block_name Value` - leaves block_name, returning Value

`meta Object MetaInfo` - attach MetaInfo to Object

`_.meta_` - get object's metainfo

`main_args` - arguments passed from command line

`main_root` - directory where the program executable resides

`as Name Value Expr` - binds `Value` to `Name`, during evaluation of `Expr`; returns `Name`

`list.flip` - reverse elements of a list

`list.take N` - take N elements from the beginning of a list

`list.drop N` - drop N elements from the beginning of a list

`list.head` - first element of a list

`list.tail` - all but first elements of a list

`list.last` - last element of list

`list.lead` - all but last elements of a list

`list.size` - number of elements in a list

`list.xs @Xs` - pick list elements indexed by Xs

`table.xs @Xs` - pick list elements indexed by Xs

`pop Xs` - shorthand for `Xs <= Xs.tail`

`push Item Xs` - shorthand for `Xs <= Item,@Xs`

`list.replace A B` - searches for `A` and replaces it with `B`.

`list.group N` - break list's element into lists of N elements

`list.all F` - true if all of list's elements are F

`list.any F` - true if any of list's elements are F

`list.max` - element with largest value

`list.min` - element with smalles value

`list.i` - produces a list of elements of source list paired with their position indices

`list.del Index` - a new list without the element at `Index`

`list.insert Index Item` - a new list with `Item` inserted at `Index`

`list.change Index Item` - a new list with element at `Index` changed to `Item`

`list.sort` - elements of a list in sorted order

`list.shuffle` - elements of a list in random order

`list.locate F` - position index of `F` in list or `No`

`list.find F` - returns the first element matching `F` or `No`

`list.infix Item` - put `Item` between elements of a list

`list.sum` - sum of elements in a list

`list.transpose` - matrix transpose

`list.bytes` - coerce list of integers to an efficient internal representation, using only 8 bites for each value

`_.as_text` - convert object to textual represenation

`int.x` - heXadecimal textual representation of an integer

`text.title` - capitalizes text

`text.trim` - removes spaces at left or right of a text

`text.int @Radix` - parse text as integer; Radix is optional, defaulting to 10 

`unix ShellCommand` - call external command

`text.exists` = checks if file/directory exist

`text.time` = file/directory modification time

`text.mkpath` - makes a direcotry

`list.utf8` - decode list of utf8 bytes as text

`text.utf8` - encode text as list of utf8 bytes

`text.url` - break pathname into components

`list.unurl` - does the opposite

`int.rand` - random number between 0 and integer inclusive

`float.rand` - random number between 0 and float inclusive

`list.rand` - random element from a list

`A ^^ B` - bitwise and

`A -- B` - bitwise eXclusive or

`A ++ B` - bitwise inclusive or

`Integer </ Count` - bitwise shift left

`Integer /> Count` - bitwise shift right

`int.sign` - sign of a value

`int.abs` - absolute value of an integer

`int.clip A B` - integer clipped between `A` and `B` inclusive

`float.clip A B` - float clipped between `A` and `B` inclusive

`int.digits Base` - list of digits composing int

`cons F Xs` - Xs into a C-style list, where next field is embedded in object

`uncons Next Item` - uncons C-style list into Symta list


Thanks
------------------------------
Thanks to the /prog/riders of http://bbs.progrider.org/prog/ for helping me with this tutorial and criticizing the early language design.
