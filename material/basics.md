# Basics

---

Let's start by looking at some basic Haskell concepts and their syntax.

We're going to use a REPL to play a bit with the language

```
stack ghci
```

---

As a functional programming language, Haskell is based on [expressions](https://en.wikipedia.org/wiki/Expression_(mathematics)) rather than statements. While statements instruct the compiler what to do, expressions describe what we want.

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

Every valid expression in Haskell has a type

```
> 42 :: Int
42

> "hello" :: String
"hello"

> True && False :: Bool
False
```

Notice that here we are not selecting a type, just making it explicit.

---

You can obtain the type of an expression in GHCi using `:t`

```
> :t "hello"
"hello" :: String

> :t True
True :: Bool
```

---

You can assign names to expressions and refer to those names in other expressions

```
> x = "hello"

> x
"hello"
```

The expression `x` is completely equivalent to the expression "hello".

They could be substituted interchangeably.

---

We can also define our own functions

```
> f x = x && x

> :t f
Bool -> Bool
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

In the examples above, we refer to `MkPerson` and `MkPerson1` as constructors. Data types can have more than one constructor:

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

The same function could be written also using a `case` expression

```haskell
canIPass :: TrafficLight -> Bool
canIPass colour = case colour of
  Green  -> True
  Yellow -> True
  Red    -> True
```

This is useful when you want to split cases in the middle of a function without defining a new function.

---

Each constructor of a data type can store different data:

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

In addition to working with concrete types, such as `Shape`, we can also work with polymorphic functions  (think generics in other languages) where we have type variables instead of concrete types:

```
> :t (:)
(:) :: a -> [a] -> [a]
```

The signature is telling us that `(:)` allows us to attach a new element of type `a` to a list of elements of type `a`, for any possible type `a`.

Concrete types always start with a capital letter, type variables always start with a lowercase character.
