# Forms - Domain

---

In this first chapter, we want to start creating a simplified clone of Google Forms. That is, we want to create a web app which allows us to create questionnaires and submit answers to them.

---

Let's start modelling our domain. The main entities are `Question`s and `Answer`s.

We use a file `src/Forms.hs`.

```haskell
module Forms where
```

---

We define a `Question` by its title, some text containing the actual question, and the type required for the answer:

```haskell
data Question = MkQuestion
  { title      :: String
  , answerType :: AnswerType
  }
```

We are introducing a new type `Question`, with a single constructor `MkQuestion` and two fields:
- `title` of type `String`
- `answerType` of type `AnswerType`

What is the type of `MkQuestion`, `title`, and `answerType`?

---

Google Forms allows several options for the type of questions (e.g. paragraph, number, multiple choice, data, etc.)

To avoid unnecessary complexity, we begin with forms which could ask questions which require either a text or an integer answer:

```haskell
data AnswerType
  = Paragraph
  | Number
```

We are defining another type `AnswerType` with two constructors `Paragraph` and `Number`. What is their type?

---

Notice that the options for `AnswerType` are closed. It is not possible to add another option without modifying the actual definition.

---

Now we can start defining some actual questions. Try to define the questions `What is your name` and `How old are you?`.

```haskell
whatIsYourName :: Question
whatIsYourName = MkQuestion
  { title = "What is your name?"
  , answerType = Paragraph
  }

howOldAreYou :: Question
howOldAreYou = MkQuestion
  { title = "How old are you?"
  , answerType = Number
  }
```

---

Next we should try to define what `Answer`s look like. It could either contain a paragraph of text or an integer.

Try to define an `Answer` type

```haskell
data Answer
  = ParagraphAnswer String
  | NumberAnswer Int
```

We are defining the `Answer` type, with two constructors `ParagraphAnswer` and `NumberAnswer`. What is their type?

---

Now we have an extremely basic model for our domain.

Next we want to try interacting with the user, asking them a question and letting them reply with an answer.

We want to do this using the terminal as our interaction medium.

---

We want to obtain an `Answer` from a `Question`. We could use a function to model this.

```haskell
ask :: Question -> Answer
```

---

Beware, all functions in Haskell are pure! Does this mean that we need to define the answer to our questions at compile time? Not a friendly user interaction...

---

What exactly does `purity` mean?

An expression is said to be [`referentially transparent`](https://www.wikiwand.com/en/Referential_transparency) if it can be replaced with its corresponding value without changing the program's behavior.

For example, `42 * 42 == 42 * 40 + 42 * 2` could be replaced anywhere with `True` without changing the sematics of the program.

---

A function is called `pure` if `f x` is referentially transparent for any possible input `x`.

---

Practically, what does _purity_ means?

It means that we can replace any function `f` with a lookup table which assigns to any input `x` its output `f x`

---

What are the benefits of purity?

- Easy to reason about the code.
- Easy testing.
- Easy to compose.

---

What is ruled out by purity? The following can not be represented by pure functions:

- Every side effect, basically.
- Mutable variables
- Exceptions
- (Implicitly) stateful code
- system interactions
- random values generation

---

To allow interaction with external world at runtime, we wrap the return type in the `IO` context

```haskell
ask :: Question -> IO Answer
```

This way we are saying that the result of the `ask` function is an interaction with the external world which produces a value of type `Answer`

See also
- https://wiki.haskell.org/Introduction_to_IO
- https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions
- https://www.haskellforall.com/2013/01/introduction-to-haskell-io.html
- http://learnyouahaskell.com/input-and-output

---

Before implementing the `ask` function, we need to understand this `IO` type a little better .

---

Typical interactions working in the `IO` context are [`putStrLn`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:putStrLn), to print to standard output, and [`getLine`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:getLine), to read a line from standard input

```haskell
putStrLn :: String -> IO ()

getLine :: IO String
```

---

Code in the `IO` context is by nature impure. As a result, the order in which operations are executed matters, just like in imperative languages. Haskell has a special notation to write sequential code, introduced by the [`do` keyword](https://en.m.wikibooks.org/wiki/Haskell/Simple_input_and_output)

```haskell
foo :: a -> IO b
foo x = do
  y <- f x -- f :: a -> IO c
           -- y :: c
  g y      -- g :: c -> IO b
```

---

The `do` notation is just syntactic sugar. There's no real value in it, it only makes the code cuter.

The code we write above we could have actually written as

```haskell
foo :: a -> IO b
foo = f >=> g
```

---

Try to write a function which reads a string from standard input and prints it on standard output

```haskell
parrot :: IO ()
parrot = do
  input <- getLine
  putStrLn input
```

---

Now we have all the ingredients, we need to actually implement the `ask` function.

```haskell
ask :: Question -> IO Answer
ask question = _
```

---

Now `_` should contain a value of type `IO Answer`, which is a sequence of actions which, in the end, return a value of type `Answer`.

To introduce the sequence of actions to be performed, we use `do` notation

```haskell
ask :: Question -> IO Answer
ask question = do
  _
```

---

The first thing we need to do is actually ask the question, which means printing it to the screen

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  _
```

---

Then we want to collect the answer provided by the user

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  _
```

---

Now `answer` is of type `String`. It's exactly what we want for a `Paragraph` question, not really for a `Number` one. Therefore, we need to distinguish the two cases. We need a `case` statement

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  case answerType question of
    Paragraph -> _
    Number    -> _
```

---

In the `Paragraph` case we have a `String`, and we need to return an `IO Answer`.

First we can create an `Answer` from a `String` using the `ParagraphAnswer` constructor

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  case answerType question of
    Paragraph -> _ (ParagraphAnswer answer)
    Number    -> _
```

---

Now we are missing a function `Answer -> IO Answer`. We need to lift our value into the `IO` context.

We can use `pure :: a -> IO a`

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  case answerType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    -> _
```

---

In the other case, where `answerType` is `Number`, we need to check whether the provided text actually contains a number.

---

Haskell does not provide casting between different types automatically. Everything needs to be explicit.

We need to parse our `String` to check if it is an `Int`

---

What we want is a function that takes some `String` as input and returns an `Int`

```haskell
parseInt :: String -> Int
```

---

Actually, not every text contains an integer, so we need a way to fail somehow.

Remember that functions in Haskell are pure, which means, among other things, that we need to return a result for every input.

Notice: Haskell has exceptions (only in `IO`, though).

---

The standard mechanism to have the possibility of failing while maintaining purity is to enlarge the return type

```haskell
parseInt :: String -> Either String Int
```

---

`Either` is defined as

```haskell
data Either a b
  = Left a
  | Right b
```

And is the default type used when a type needs to contain two separate options of a possibly different type.

It is often used for operations which might fail with an error message. Conventionally, the `Left` constructor is used for the failure case and the `Right` constructor for the success case.

---

For the sake of completeness, we can implement `parseInt` as follows.


```haskell
-- base
import Text.Read (readEither)

parseInt :: String -> Either String Int
parseInt = readEither
```

---

Applying `parseInt` to `answer` we can now distinguish the cases where `answer` contains an integer of does not

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  case answerType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    ->
      case parseInt answer of
        Left errorMessage -> _
        Right intAnswer   -> _
```

---

Similarly to what we did for the other case, if we are in the `Right` case, we can return the wrapped value

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  case answerType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    ->
      case parseInt answer of
        Left errorMessage -> _
        Right intAnswer   -> pure (NumberAnswer intAnswer)
```

---

In the `Left` case, we want to inform the user that they did not provide a valid integer and then ask again

```haskell
ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  case answerType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    ->
      case parseInt answer of
        Left errorMessage -> do
          putStrLn ("invalid integer: " <> errorMessage <> ". Try again")
          ask question
        Right intAnswer   -> pure (NumberAnswer intAnswer)
```

---

Great! We have some code which should do what we want!

Let's try it out!

---

We need to create a `Main` module with a `main :: IO ()` function

```haskell
module Main where

main :: IO ()
main = _
```

---

Let's try to ask our `whatIsYourName` and `howOldAreYou` questions and then print the answers

```haskell
import Domain.Forms

main :: IO ()
main = do
  name <- ask whatIsYourName
  age  <- ask howOldAreYou
  putStrLn ("name: " <> _ <> ". age: " <> _)
```

---

We need to fill the holes with strings while we have `Answer`s. Luckily in Haskell there is a standard mechanism to transform a data type to `String`.

It is the `Show` type class, which provides a `show :: Show a => a -> String` function.

---

But, what is a type class?

It's the Haskell mechanism for ad-hoc polymorphism (think interfaces in OOP).

It means that we can overload a function to behave differently for different data types.

---

For example, the `Show` type class is defined as

```haskell
class Show a where
  show :: a -> String
```

---

A data type gets access to the type class functionalities by declaring an instance

```haskell
instance Show Foo where
  show a = _
```

---

We actually need an instance of `Show` for `Answer`

```haskell
instance Show Answer where
  show (ParagraphAnswer t) = t
  show (NumberAnswer    i) = show i
```

---

Actually, Haskell could address this boilerplate automatically

```haskell
data Answer
  = ParagraphAnswer Text
  | NumberAnswer Int
  deriving Show
```

---

Deriving is a huge topic (we're not going into it, though) and can save a lot of boilerplate.

See [here](https://kowainik.github.io/posts/deriving) for more details

---

I need to mention that `Show` should only be used for debugging. If you need to show data to users, a pretty-printing library would be more appropriate.

---

Now we can complete our definition of `main`

```haskell
main :: IO ()
main = do
  name <- ask whatIsYourName
  age  <- ask howOldAreYou
  putStrLn ("name: " <> show name <> "; age: " <> show age)
```

---

And we can try it out in our terminal

```bash
stack exec forms
```

---

Next, we want to be able to ask multiple questions one after the other

```haskell
askMultiple :: [Question] -> IO [Answer]
askMultiple questions = _
```

Let's try to go through the implementation together

---

Try to pattern match on the questions

```haskell
askMultiple :: [Question] -> IO [Answer]
askMultiple []                         = _
askMultiple (question : nextQuestions) = _
```

---

When we have no questions, we have no answers

```haskell
askMultiple :: [Question] -> IO [Answer]
askMultiple []                         = pure []
askMultiple (question : nextQuestions) = _
```

---

If we have at least one question, we ask it, and then we ask the others

```haskell
askMultiple :: [Question] -> IO [Answer]
askMultiple []                         = pure []
askMultiple (question : nextQuestions) = do
  answer <- ask question
  nextAnswers <- askMultiple nextQuestions
  pure (answer : nextAnswers)
```

---

Actually, Haskell already has a function which does exactly this!

```haskell
askMultiple :: [Question] -> IO [Answer]
askMultiple = traverse ask

---

Let's now refactor `main` to use `askMultiple`

```haskell
main :: IO ()
main = do
  answers <- askMultiple [whatIsYourName, howOldAreYou]
  print answers
```

---

A `String` is defined as a [list of characters](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:String). This makes it easy to manipulate but not particularly performant. As a consequence, it is not recommended for usage in production. Use [`Text`](https://hackage.haskell.org/package/text-2.0/docs/Data-Text.html#t:Text) instead, which provides a similar API but is optimized for better performance.

See also:
- https://www.fpcomplete.com/haskell/tutorial/string-types/
- https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings
- https://www.alexeyshmalko.com/2015/haskell-string-types/
- https://free.cofree.io/2020/05/06/string-types/

---

Let's then refactor our code to use `Text` instead of `String`.

---

We first need to import the `Text` type:

```haskell
-- text
import Data.Text (Text)
```

---

We also need to declare the `text` package as a dependency in `package.yaml`:

```yaml
dependencies:
  - text
```

---

We also need to change all the `IO` functions which generally work with `String`s to their counterparts for `Text`, which can be found in the `Data.Text.IO` module.

```haskell
import qualified Data.Text.IO as Text
```

---

`parseInt` needs to handle the conversions between `String` and `Text`

```haskell
import Data.Bifunctor

parseInt :: Text -> Either Text Int
parseInt = first pack . readEither . unpack
```

---

Where we have string literals, the compiler is now telling us that it is expecting `Text`s, but is still seeing `String`s.

We could solve this by enabling the `OverloadedStrings` extension. One way to do this is using a [pragma](https://wiki.haskell.org/Language_Pragmas) on top of our module:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

---

[Extensions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/intro.html) are a mechanism to opt into (or out of) different language behaviours. They are basically feature flags for the compiler.

---
## Learned concepts

- define a data type
- sum types
- `IO`, `putStrLn` and `getLine`
- `do` notation
- `case` expressions
- `pure` as a lifting mechanism
- `Either` as a failure mechanism
- `main` to run the code
- typeclasses and instances
- `deriving`
- lists
- pattern matching
- `traverse`
- `Text` over `String`
- importing packages and modules
- extensions and pragmas
- qualified imports
