# Cheatsheet

## declarations and types

When we declare a value `f`, we usually first write the type of `f` and then, on a separate line, its actual value. For example

```haskell
fortytwo :: Int
fortytwo = 42

hi :: String
hi = "hi"
```

## functions

Functions types are defined using the `->` symbol

```haskell
foo :: Int -> Int -> Int -> Int
foo i j k = i + j * k
```

This means that `foo` is a function with 3 `Int` arguments.

Functions are curried by default (e.g. `f 1` is a function `Int -> Int -> Int`)

The same function could be written in an anonymous form as

```haskell
\i j k -> i + j * k
```

### function application

Function application is denoted with whitespace.
For example, `foo 1 2 3` equals `1 + 2 * 3 = 7`

## data declaration

A data declaration looks as follows:

```haskell
data Person = MkPerson
  { name :: String
  , age  :: Int
  }
```

`Person` is the name of the type.
`MkPerson` is the constructor, which is a function `String -> Int -> Person`
`name` and `age` are fields which have respectively type `Person -> String` and `Person -> Int`

### instantiating a value

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

### multiple constructors

A data declarations could have several constructors, as in

```haskell
data Bar
  = Bar1 Int
  | Bar2 String
```

`Bar1` and `Bar2` are two separate constructors for `Bar`, of type `Int -> Bar` and `String -> Bar` respectively

### pattern matching

To consume a value of type `Bar`, we can pattern match on the constructors and define the result case by case

```haskell
toString :: Bar -> String
toString (Bar1 i) = show i
toString (Bar2 s) = s
```

## type variables

Concrete types always start with a capital letter, type variables (i.e. generics) always start with a lowercase character

```
> :t (:)
(:) :: a -> [a] -> [a]
```

The signature is telling us that `(:)` allows us to attach a new element of type `a` to a list of elements of type `a`, for any possible type `a`.
