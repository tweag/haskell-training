---
type: slide
tags: tweag, training
slideOptions:
  progress: true
  controls: false
  slideNumber: false
---

<style>
  .reveal pre {width: 100%; max-height: 600px;}
  .reveal pre code {max-height: 600px;}
  code {color: #c7254e;}
</style>

# Haskell at Work - Basics

---

We're going to use [Gitpod](https://www.gitpod.io/) to have development environments for free.

I recommend to use Visual Studio Code - online

---

If you want to set up the project locally, you could use `nix` to manage all the dependencies of the project

```bash
nix-shell
```

---

Otherwise, you could install [GHCup](https://www.haskell.org/ghcup/) and then use it to install `GHC 9.0.2`, `cabal`, `stack` and `hls`.

Then install and configure the Haskell extension for your favourite IDE to integrate with the above tools (e.g. the [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) extension for Visual Studio Code)

---

[Stack](https://docs.haskellstack.org/en/stable/) is the build tool we will use throughout the course.

We will always use it to interact with our codebase.

---

Let's start by looking at some basic Haskell concepts and their syntax.

---

We're going to use a REPL to play a bit with the language

```
stack ghci
```

---

As a functional programming language, Haskell is based on [expressions](https://en.wikipedia.org/wiki/Expression_(mathematics)) rather than statements.

While statements instruct the program what to do, expressions are combinations of other expressions which evaluate to a value to describe what we want.

---

Let's start to try to write some expressions

```
> 42
42

> 42 * 42
1764

> pi
3.141592653589793

> "hello"
"hello"

> "hello" ++ "world"
"helloworld"

> True && False
False

> 42 * 42 == 42 * 40 + 42 * 2
True
```

---

Evaluation of expressions is just simplification, as in mathematical expressions

```
42 * 42 == 42 * 40 + 42 * 2
1764 == 1680 + 84
1764 == 1764
True
```

---

A good example of the difference between expressions (in Haskell) and statements (in imperative languages) is `if`:

```
> if True then "hello" else "grrr"
"hello"
```

---

Every valid expression in Haskell has a type

```
> 42 :: Int
42

> "hello" :: String
"hello"

> True && False :: Bool
False

> if True then "hello" else "grrr" :: String
"hello"
```

Notice that here we are not selecting a type, just making it explicit.

---

You can obtain the type of an expression in GHCi using `:t`

```
> :t "hello"
"hello" :: String

> :t True
True :: Bool

> :t if True then "hello" else "grrr"
if True then "hello" else "grrr" :: String
```

---

You can assign names to expressions and refer to those names in other expressions

```
> x = "hello"

> x
"hello"
```

The expression `x` is completely equivalent to the expression `"hello"`.

They could be used interchangeably.

---

We can also define our own functions

```
> f x = x && x

> :t f
Bool -> Bool
```

---

This is the same as

```
> f = \x -> x && x
```

---

And we can apply functions to values

```
> f True
True
```

---

Let's now move to work in a file `src/Basics.hs`

```haskell
module Basics where
```

---

Now we can run `stack build --file-watch` to recompile the file as soon as we save it.

---

We can define some values and functions, similarly to what we did in the REPL

```haskell
x :: String
x = "hello"

f :: Bool -> Bool
f x = x && x
```

---

Next we want to define our own data types

```haskell
data Person = MkPerson
  { name :: String
  , age  :: Int
  }
```

---

Pay attention that Haskell is an [indentation](https://en.m.wikibooks.org/wiki/Haskell/Indentation) sensitive language!

> Code which is part of some expression should be indented further in than the beginning of that expression

---

We can now check what these things are in GHCi

```
> :l Basics

> :i Person
type Person :: *
data Person = MkPerson {name :: String, age :: Int}

> :t MkPerson
MkPerson :: String -> Int -> Person

> :t name
name :: Person -> String
```

---

We can define a new `Person` like this

```haskell
me :: Person
me = MkPerson
  { name = "Marco Perone"
  , age = 38
  }

stillMe :: Person
stillMe = MkPerson "Marco Perone" 38
```

---

How do we use a `Person`?

```haskell
personDetails :: Person -> String
personDetails (MkPerson name age)
  = name ++ " is " ++ show age ++ " years old"
```

---

We could have defined the same type without specifying the name of the fields

```haskell
data Person1 = MkPerson1 String Int
```

```
> :i Person1
type Person1 :: *
data Person1 = MkPerson1 String Int

> :t MkPerson1
MkPerson1 :: String -> Int -> Person1
```

---

In the examples above, we refer to `MkPerson` and `MkPerson1` as constructors. Data types can have more than one constructor:

```haskell
data TrafficLight
  = Green
  | Yellow
  | Red
```

---

Notice that the options for `TrafficLight` are closed. It is not possible to add another option without modifying the actual definition.

---

We can then define a function looking at every single case

```haskell
canIPass :: TrafficLight -> Bool
canIPass Green  = True
canIPass Yellow = True
canIPass Red    = False
```

---

The same function could be written also using a `case` expression

```haskell
canIPass :: TrafficLight -> Bool
canIPass colour = case colour of
  Green  -> True
  Yellow -> True
  Red    -> False
```

This is useful when you want to split cases in the middle of a function without defining a new function.

---

Each constructor of a data type can store different data:

```haskell
data Shape
  = Rectangle {side1 :: Float, side2 :: Float}
  | Square {side :: Float}
  | Circle {radius :: Float}
```

---

Now we can pattern match on the argument

```haskell
perimeter :: Shape -> Float
perimeter (Rectangle side1 side2) = (side1 + side2) * 2
perimeter (Square side)           = side * 4
perimeter (Circle radius)         = 2 * pi * radius
```

---

In addition to working with concrete types, such as `Shape`, we can also work with polymorphic functions (think generics in other languages) where we have type variables instead of concrete types

---

```
> :t (:)
(:) :: a -> [a] -> [a]
```

The signature is telling us that `(:)` allows us to attach a new element of type `a` to a list of elements of type `a`, for any possible type `a`.

---

Concrete types always start with a capital letter, type variables always start with a lowercase character.

---

We can also define our own polymorphic data structures

```haskell
data Foo a b c = MkFoo
  { bar :: a
  , baz :: b -> c
  }
```

---

Whenever there is a polymorphic data structure, the caller has the freedom to decide the value of the type variables

```haskell
foo :: Foo Int Char String
foo = MkFoo
  { bar = 42
  , baz = \char -> [char]
  }
```
