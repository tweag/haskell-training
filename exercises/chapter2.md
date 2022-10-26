# Exercises for chapter 2

Suppose you have a data structure

```haskell
data Person = Person
  { name :: String
  , age  :: Int
  }
```

Solve these exercises using functors and applicatives

- Suppose you have two optional values `mName :: Maybe String` and `mAge :: Maybe Age`. Write code to construct an optional person of type `Maybe Person` ([here](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#t:Maybe) you have the definition of `Maybe`).

- Suppose you have two functions `validateName :: String -> Either e String` and `validateAge :: Int -> Either e Int` which could return a value or an error message. Write code to define a function `validatePerson : String -> Int -> Either e Person` which could return either a person or an error message.

- Suppose you have two functions `getName :: a -> String` and `getAge :: a -> Int` to extract data from some other data type `a`. Write code to construct a function `a -> Person`.

- (harder) Suppose you have two functions `nameOrError :: a -> Either e String` and `ageOrError :: a -> Either e Int` which extract data from a data type `a`, but could actually fail with an error of type `e`. Write code to construct a function `a -> Either e Person`.

- (harder and longer) Use the [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) library to build a little executable which reads a `name` and an `age` option from a command line command and builds a `Parser Person`, where `Parser` is the context provided by the library
