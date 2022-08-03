# Basics

---

Let's start by reviewing some basic Haskell concepts and its syntax.

We're going to use a REPL to play a bit with the language

```
stack ghci
```

---

Differently from many other common languages, Haskell is based on [expressions](https://en.wikipedia.org/wiki/Expression_(mathematics)) rather than statements.

> An expression is a syntactic combination of symbols that is well-formed, according to rules that depend on the context

---

Let's start to try to write some expressions

```
> 42
42

> 42 * 42
1764

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

---

Every expression in Haskell has a type

```
> 42 :: Int
42

> "hello" :: String
"hello"

> True && False :: Bool
False
```

---

You can obtain in GHCi the type of an expression using `:t`

```
> :t "hello"
"hello" :: String

> :t True
True :: Bool
```

---

You can assign names to expressions and declare variables

```
> x = "hello"

> x
"hello"
```

The expression `x` is completely equivalent to the expression "hello".

They could be substituted interchangeably.

---

Even functions are expressions

```
> :t (&&)
(&&) :: Bool -> Bool -> Bool
```

---

We can also define our own functions

```
> f x = x && x

> :t f
Bool -> Bool
```

---

Functions are expressions as other values, and they can be defined directly, without the need to be defined explicitly for every input.

```
> notnot = not . not
```

---

Let's now leave the REPL (with `:q`) and open a file `src/Basics.hs`

```haskell
module Basics where
```

Now we can run `stack build --file-watch` to recompile the file as soon as we edit it.

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

We can now check what these things are in GHCi

```
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
---

How do we use a `Person`?

```haskell
personDetails :: Person -> String
personDetails (MkPerson name age) = name ++ " is " ++ show age ++ " years old"
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

Another kind of data type (called sum types) we can define is one which could contain one of several options

```haskell
data TrafficLight
  = Green
  | Yellow
  | Red

canIPass :: TrafficLight -> Bool
canIPass Green  = True
canIPass Yellow = True
canIPass Red    = False
```

---

The same function could be written also using a `case` statement

```haskell
canIPass :: TrafficLight -> Bool
canIPass colour = case colour of
  Green  -> True
  Yellow -> True
  Red    -> True
```

This is useful when you want to split cases in the middle of a function without defining a new function.

---

We can actually combine the two kind of types to create complicated types with multiple constructors, each one containing various data

```haskell
data Shape
  = Rectangle {side1 :: Float, side2 :: Float}
  | Square {side :: Float}
  | Circle {radius :: Float}

perimeter :: Shape -> Float
perimeter (Rectangle side1 side2) = (side1 + side2) * 2
perimeter (Square side)           = side * 4
perimeter (Circle radius)         = 2 * pi * radius
```

---

Instead of working only with concrete types we could work with type variables (think generics in other languages)

```
> :t (:)
(:) :: a -> [a] -> [a]
```

The signature is telling us that `(:)` allows us to attach a new element of type `a` to a list of elements of type `a`, for any possible type `a`.

Concrete types always start with a capital letter, type variables always start with a lowercase character.
