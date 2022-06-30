# Forms - Domain

---

In this first chapter, we want to start creating a simplified clone of Google Forms. That is, we want to create a web app which allows us to create questionnaires and submit answers to them.

---

Let's start modelling our domain. The main entities are `Question`s and `Answer`s.

---

We define a `Question` by its title, some text containing the actual question, and the type required for the answer:

```haskell
data Question = MkQuestion
  { questionTitle :: String
  , questionType  :: QuestionType
  }
```

We are introducing a new type `Question`, with a single constructor `MkQuestion` and two fields:
- `questionTitle` of type `String`
- `questionType` of type `QuestionType`

What is the type of `MkQuestion`, `questionTitle`, and `questionType`?

---

Google Forms allows several options for the type of questions (e.g. paragraph, number, multiple choice, data, etc.)

To avoid unnecessary complexity, we begin with forms which could ask questions which require either a text or an integer answer:

```haskell
data QuestionType
  = Paragraph
  | Number
```

We are defining another type `QuestionType` with two constructors `Paragraph` and `Number`. What is their type?

---

Notice that the options for `QuestionType` are closed. It is not possible to add another option without modifying the actual definition.

---

Now we can start defining some actual questions. Try to define the questions `What is your name` and `How old are you?`.

```haskell
whatIsYourName :: Question
whatIsYourName = MkQuestion
  { questionTitle = "What is your name?"
  , questionType = Paragraph
  }

howOldAreYou :: Question
howOldAreYou = MkQuestion
  { questionTitle = "How old are you?"
  , questionType = Number
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

Before we continue, let's refine our type for dealing with strings. A `String` is defined as a [list of characters](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:String). This makes it easy to manipulate but not particularly performant. As a consequence, it is not recommended for a production project. Use [`Text`](https://hackage.haskell.org/package/text-2.0/docs/Data-Text.html#t:Text) instead, which provides a similar API but is optimized for better performance.

See also:
- https://www.fpcomplete.com/haskell/tutorial/string-types/
- https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings
- https://www.alexeyshmalko.com/2015/haskell-string-types/
- https://free.cofree.io/2020/05/06/string-types/

---

Let's then refactor our code to use `Text` instead of `String`.

---

We need to first import the `Text` type:

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

The compiler is telling us that it is expecting `Text`s, but is still seeing `String`s.

We could solve this by enabling the `OverloadedStrings` extension. One way to do this is using a [pragma](https://wiki.haskell.org/Language_Pragmas) on top of our module:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

---

[Extensions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/intro.html) are a mechanism to actually modify the language behavior. They are basically feature flags for the compiler.

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

Typical interactions working in the `IO` context are [`putStrLn`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:putStrLn), to print to standard output, and [`getLine`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:getLine), to read a line from standard input

---

Code in the `IO` context is by nature imperative. Haskell has a special notation to write sequential code, introduced by the `do` keyword

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

We need to import the functions we need to work with `Text`

```haskell
import Data.Text.IO

ask :: Question -> IO Answer
ask question = do
  Data.Text.IO.putStrLn (questionTitle question)
  _
```

---

To clean it up a bit, we can qualify our import

```haskell
import qualified Data.Text.IO as Text

ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (questionTitle question)
  _
```

---

Then we want to collect the answer provided by the user

```haskell
ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  _
```

---

Now `answer` is of type `Text`. It's exactly what we want for a `Paragraph` question, not really for a `Number` one. Therefore, we need to distinguish the two cases. We need a `case` statement

```haskell
ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  case questionType question of
    Paragraph -> _
    Number    -> _
```

---

In the `Paragraph` case we have a `Text`, and we need to return an `IO Answer`.

First we can create an `Answer` from a `Text` using the `ParagraphAnswer` constructor

```haskell
ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  case questionType question of
    Paragraph -> _ (ParagraphAnswer answer)
    Number    -> _
```

---

Now we are missing a function `Answer -> IO Answer`. We need to lift our value into the `IO` context.

We can use `pure :: a -> IO a`

```haskell
ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  case questionType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    -> _
```

---

In the other case, where `questionType` is `Number`, we need to check whether the provided text actually contains a number.

---

Haskell does not provide casting between different types automatically. Everything needs to be explicit.

We need to parse our text to check if it is an `Int`

---

What we want is a function that takes some `Text` as input and returns an `Int`

```haskell
parseInt :: Text -> Int
```

---

Actually, not every text contains an integer, so we need a way to fail somehow.

Remember that functions in Haskell are pure, which means, among other things, that we need to return a result for every input.

Notice: Haskell has exceptions (only in `IO`, though).

---

The standard mechanism to have the possibility of failing while maintaining purity is to enlarge the return type

```haskell
parseInt :: Text -> Either Text Int
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

Applying `parseInt` to `answer` we can now distinguish the cases where `answer` contains an integer of does not

```haskell
ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  case questionType question of
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
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  case questionType question of
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
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  case questionType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    ->
      case parseInt answer of
        Left errorMessage -> do
          Text.putStrLn ("invalid integer: " <> errorMessage <> ". Try again")
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
main :: IO ()
main = do
  name <- ask whatIsYourName
  age  <- ask howOldAreYou
  putStrLn "name: " <> _ <> ". age: " <> _
```

---

The compiler complains that he wants strings while we have `Answer`s. Luckily in Haskell there is a standard mechanism to transform a data type to `String`.

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
  show (ParagraphAnswer t) = unpack t
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
