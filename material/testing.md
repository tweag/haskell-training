---
type: slide
tags: tweag, training
---

# Haskell at Work - Testing

---

There is a big elephant in the room for the code we wrote up to now.

We wrote the code, it compiles, but we are not checking that it behaves how we want.

---

Types are awesome and can encode a lot of information, but they are often not enough to guarantee the correct behavior of our application

---

The first level of testing is just manual testing.

---

You can use the REPL to test the code you wrote

```bash
stack ghci
```

Stack already loads all the modules of your project, so that you can use the exposed functions and data types immediately.

---

Manual testing is cumbersome and error-prone.

We definitely want to automate the process.

---

We are going to use a testing library called [`hspec`](https://hackage.haskell.org/package/hspec)

```yaml
tests:
  forms-spec:
    main:           Spec.hs
    source-dirs:    spec
    dependencies:
      - haskell-training
      - hspec
```

---

We need to create a `Spec.hs` file inside the `spec` folder, containing the magic

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

This will auto-discover test files, ending in `Spec.hs`, inside the `spec` folder.

---

Let's start testing our initial `Basics` file

```haskell
module BasicsSpec where

-- hspec
import Test.Hspec

spec :: Spec
spec = _
```

---

To write tests we use the quite common `describe` and `it` functions

```haskell
spec =
  describe "Basics" $ do
    describe "perimeter" $ do
      it "returns 4 for a square of side 1" $ do
        _
```

---

We use the `shouldBe` infix function to assert that two things should be equal

```haskell
import Basics

      it "returns 4 for a square of side 1" $ do
        perimeter (Square 1) `shouldBe` 4
```

There are [many other functions](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html#v:shouldBe) to write test assertions.

---

Try to implement yourself

```haskell
      it "returns 6 for a rectangle of sides 1 and 2" $ do
        _
```

---

As they say, testing is a great tool for documentation. And the best documentation lives close to the code.

We could use [`doctest`](https://hackage.haskell.org/package/doctest-parallel) to have the tests closer to our functions.

```
tests:
  forms-doctest:
    source-dirs:      test
    main:             doctests.hs
    ghc-options:      -threaded
    dependencies:
      - haskell-training
      - doctest-parallel >= 0.1
```

---

We need to create a `test/doctests.hs` file to get the machinery running

```haskell
module Main where

-- base
import System.Environment (getArgs)

-- doctest-parallel
import Test.DocTest (mainFromCabal)

main :: IO ()
main = do
  mainFromCabal "haskell-training" =<< getArgs
```

---

Now we can write our first doctest, right above the definition of our function

```haskell
-- | Perimeter of a shape
--
-- >>> perimeter (Rectangle 1 2)
-- 6.0
--
-- >>> perimeter (Square 1)
-- 4.0
--
-- >>> perimeter (Circle 1)
-- 6.2831855
```

---

Unit tests are great, but they help us only with case we thought about, they do not help us to discover cases we were not expecting.

---

To solve this, the [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) library was developed in the Haskell ecosystem.

It enables to write property based tests, i.e. asserting that a property holds for basically and combination of the involved values.

```yaml
tests:
  forms-spec:
    dependencies:
      - QuickCheck
```

---

A property is just an invariant of our code, which should hold true for any possible combination of the relevant values.

---

There are several strategies to find good properties for our code. See for example:
- [Find the best properties for Property Based Testing](https://medium.com/@nicolasdubien/find-the-best-properties-for-property-based-testing-ee2ed9d442e1)
- [Choosing properties for property-based testing](https://fsharpforfunandprofit.com/posts/property-based-testing-2/)

---

For example, we would like the perimeter of a rectangle to remain constant if we swap the sides

```haskell
-- hspec
import Test.Hspec.QuickCheck

      prop "does not change for rectangles with swapped sides" $ do
        perimeter (Rectangle s1 s2) `shouldBe` perimeter (Rectangle s2 s1)
```

---

Does not work because `s1` and `s2` are not in scope

Let's introduce them with an anonymous function

```haskell
      prop "does not change for rectangles with swapped sides" $ do
        \s1 s2 -> perimeter (Rectangle s1 s2) `shouldBe` perimeter (Rectangle s2 s1)
```

---

We get

```
+++ OK, passed 100 tests.
```

This means the test suite tried for 100 times

---

Next we want to try a property on the whole `Shape` data type.

We are going to check whether it is true that `the perimeter is zero only if the shape is degenerate`

```haskell
      prop "is zero only if the shape is degenerate" $ do
        _
```

---

First thing, we need to define what we mean by `degenerate`

```haskell
isDegenerate :: Shape -> Bool
isDegenerate (Rectangle s1 s2) = s1 == 0 && s2 == 0
isDegenerate (Square s)        = s == 0
isDegenerate (Circle r)        = r == 0
```

---

What we want to check is that the shape is degenerate

```haskell
        \shape -> shape `shouldSatisfy` isDegenerate
```

---

The compiler is asking us for instances of `Show` and `Arbitrary` for `Shape` to be able to run the test.

The `Show` instance is easy, since we can derive it.

```haskell
data Shape
  = Rectangle {side1 :: Float, side2 :: Float}
  | Square {side :: Float}
  | Circle {radius :: Float}
  deriving Show
```

---

The `Arbitrary` instance is a little more complex, but let's start

```haskell
{-# LANGUAGE InstanceSigs #-}

-- QuickCheck
import Test.QuickCheck

instance Arbitrary Shape where
  arbitrary :: Gen Shape
  arbitrary = _
```

---

We can first try to generate a random `Square`

```haskell
  arbitrary = Square <$> arbitrary
```

---

Or a random rectangle

```haskell
  arbitrary = Rectangle <$> arbitrary <*> arbitrary
```

---

We actually have three options, and we would like to choose at random between them.

Let's try to take a look at the [documentation](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#g:8) of `Gen` to check if there is a combinator which could work.

---

[`oneof`](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:oneof) is what we were looking for

```haskell
  arbitrary = oneof
    [ Rectangle <$> arbitrary <*> arbitrary
    , Square <$> arbitrary
    , Circle <$> arbitrary
    ]
```

---

We have our generator, but actually we would like to restrict our test only to shapes which are degenerate.

For this, we could use the [`forAll`](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:forAll) and [`suchThat`](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html#v:suchThat) combinators.

```haskell
        forAll (arbitrary `suchThat` ((== 0) . perimeter)) $
          \shape -> shape `shouldSatisfy` isDegenerate
```

---

Now the test is actually complete, and it runs as we wanted.

Still, it fails.

---

It does so for a good reason. A rectangle with `x` and `-x` sides has actually perimeter `0`.

And `QuickCheck` is able to catch that case every time.

---

Now we want to raise a bit the level of our tests.

In a TDD style, we would like to write a test for our application to check that, when we register answers for a questionnaire, there is actually an answer for every question.

---

First we will write the test to check it.

```haskell
module Api.FormsSpec where

-- hspec
import Test.Hspec

spec :: Spec
spec =
  describe "Forms API" $ do
    describe "register answers endpoint" $ do
      it "fails if we do not register an answer for every question of the questionnaire" $ do
        _
```

---

Let's think for a minute what we actually need to do to test this at the higher level.

We need to perform a sequence of actions:
- create a questionnaire
- add some questions to the questionnaire
- add a wrong number of answers
- check that the operation failed

---

Our `formsServer` is the value which gives us access to all these functionalities

So let's suppose we have a value of type `FormsApi AsServer`

```haskell
  describe "Forms API" $ do
    let forms = formsServer _
```

---

Now the test should look something like

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Domain.Answer as Answer
import Domain.Question as Question
import Domain.Questionnaire

-- base
import Data.Either

spec :: Spec
spec =
  describe "Forms API" $ do
    let forms = formsServer _

    describe "register answers endpoint" $ do
      it "fails if we do not register an answer for every question of the questionnaire" $ do
        questionnaireId <- createNewQuestionnaire forms $ Questionnaire "title"
        questionId1 <- addNewQuestion forms $ Question "question1" Question.Paragraph questionnaireId
        _           <- addNewQuestion forms $ Question "question2" Question.Number    questionnaireId
        submissionId <- recordSubmission forms
          [ AnswerData (Answer.Paragraph "paragraph answer") questionId1
          ]
        submissionId `shouldSatisfy` isLeft
```

---

This does not work because `createNewQuestionnaire`, `addNewQuestion` and `recordSubmission` all return results in the `Handler` context, while `shouldSatify` lives in `IO`.

---

We should run the `Handler` context to get it back to `IO`.

We could use [`runHandler`](https://hackage.haskell.org/package/servant-server-0.19.1/docs/Servant-Server.html#v:runHandler).

```haskell
-- servant-server
import Servant

      it "fails if we do not register an answer for every question of the questionnaire" $ do
        submissionId <- runHandler $ do
          questionnaireId <- createNewQuestionnaire forms $ Questionnaire "title"
          questionId1 <- addNewQuestion forms $ Question "question1" Question.Paragraph questionnaireId
          _           <- addNewQuestion forms $ Question "question2" Question.Number    questionnaireId
          recordSubmission forms
            [ AnswerData (Answer.Paragraph "paragraph answer") questionId1
            ]
        submissionId `shouldSatisfy` isLeft
```

---

Now we need to fill the hole for the missing `AppServices` instance.

One option would be to reuse the instance that we already have, `postgresAppServices`, but that would mean setting up a database instance just for testing. It would also make our test extremely slow.

---

We would prefer to keep our data in memory.

Therefore, we need to write a `statefulAppServices`!

---

```haskell
module Api.StatefulAppServices where

import Api.AppServices

statefulAppServices :: AppServices
statefulAppServices = _
```

---

Using the only available constructor

```haskell
statefulAppServices = AppServices
  { questionnaireRepository = _
  , questionRepository      = _
  , submissionRepository    = _
  , answerRepository        = _
  }
```

---

We need a stateful version for every one of our repositories.

Let's start as usual from `QuestionnaireRepository`.

```haskell
module Api.StatefulQuestionnaireRepository where

import Domain.Id
import Domain.Questionnaire
import Domain.QuestionnaireRepository

-- base
import Prelude hiding (all)

statefulQuestionnaireRepository :: QuestionnaireRepository IO
statefulQuestionnaireRepository = QuestionnaireRepository
  { add = statefulAddQuestionnaire
  , all = statefulAllQuestionnaires
  }

statefulAddQuestionnaire :: Questionnaire -> IO (Id Questionnaire)
statefulAddQuestionnaire = _

statefulAllQuestionnaires :: IO [Identified Questionnaire]
statefulAllQuestionnaires = _
```

---

Before proceeding with the implementation, we need to decide how we want to make our repositories stateful.

Pure code can not be stateful, so we need to introduce some new trick.

---

We actually have two possibilities:
- use a [`State`](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Sequence-Internal.html#t:State) context
- use [mutable references](https://blog.jakuba.net/2014-07-20-mutable-state-in-haskell/) to model global mutable state.

---

We'll take the second option and use [`TVar`](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Concurrent-STM-TVar.html#t:TVar)s from the `stm` package.

A `TVar` offers us transactional operations over a shared memory location.

---

What is the state that we should put in our `TVar`?

We should use the state of our whole application, which should mimic the data we keep in the database

```haskell
module Api.InMemoryState where

data InMemoryState = InMemoryState
  { questionnaires :: _
  , questions      :: _
  , answers        :: _
  }
```

---

The first idea to store `questionnaires`, `questions` and `answers` could be to use `[]`s.

For testing purposes that might work, but we can still think whether there's a data structure that approximates better.

---

We could use a hash map, mapping ids to the actual data.

The data structure which offers this is [`Map`](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Map-Internal.html#t:Map).

It maps keys of one type to value of another type.

---

Hence, we could use

```haskell
import Domain.Answer
import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- containers
import Data.Map

data InMemoryState = InMemoryState
  { questionnaires :: Map (Id Questionnaire) Questionnaire
  , questions      :: Map (Id Question)      Question
  , answers        :: Map (Id Answer)        Answer
  }
```

---

Now we can add a `TVar InMemoryState` as a parameter to our repository to indicate that we are relying on some mutable state.

```haskell
import Api.InMemoryState

-- stm
import Control.Concurrent.STM

statefulQuestionnaireRepository :: TVar InMemoryState -> QuestionnaireRepository IO
statefulQuestionnaireRepository memory = QuestionnaireRepository
  { add = statefulAddQuestionnaire memory
  , all = statefulAllQuestionnaires memory
  }

statefulAddQuestionnaire :: TVar InMemoryState -> Questionnaire -> IO (Id Questionnaire)
statefulAddQuestionnaire memory questionnaire = _

statefulAllQuestionnaires :: TVar InMemoryState -> IO [Identified Questionnaire]
statefulAllQuestionnaires memory = _
```

---

Now we can start thinking about the implementation for our `statefulAddQuestionnaire` function.

What should it do?
- Generate a new `Id Questionnaire`.
- Add the new questionnaire to the map.
- Return the `id` we just generated.

---

First and third point do not change with respect to the Postgres implementation

```haskell
statefulAddQuestionnaire :: TVar InMemoryState -> Questionnaire -> IO (Id Questionnaire)
statefulAddQuestionnaire memory questionnaire = do
  id <- generate
  _
  pure id
```

---

We now need to modify our `InMemoryState`, adding one new `questionnaire`.

If we look at the `Data.Map` documentation, we find the [`insert`](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Map-Internal.html#v:insert) function

```haskell
-- containers
import Data.Map

statefulAddQuestionnaire memory questionnaire = do
  id <- generate
  _ $ insert id questionnaire
  pure id
```

---

At this point we need to derive an `Ord` instance for `Id`, since it is needed to insert the new data.

```haskell
newtype Id a = Id UUID
  deriving newtype (Eq, Ord)
```

---

Now we wrote the code to modify just the `questionnaires` field of the `InMemoryState` data structure.

We could (exercise for you!) write by hand a function

```haskell
modifyQuestionnaires :: (Map (Id Questionnaire) Questionnaire -> Map (Id Questionnaire) Questionnaire)
                     -> InMemoryState -> InMemoryState
```

But I'd like to present you a more Haskell-y approach.

---

I'd like to briefly introduce lenses, which are data structures used to access nested data structures.

---

A `Lens s a` allows us to focus a value of type `a` inside a value of type `s`.

```
-----------------------
|                     |
|  s                  |
|         -----       |
|         | a |       |
|         -----       |
|                     |
|                     |
-----------------------
```

---

Once we can focus on it, we can read its value, set a new value or modify it.

---

First we need to create the lenses for the `InMemoryState` data structure.

We use `TemplateHaskell`, which is the Haskell extension to do metaprogramming (i.e. generating Haskell code at compile-time)

```haskell
{-# LANGUAGE TemplateHaskell #-}

-- microlens-platform
import Lens.Micro.Platform

makeLensesFor
  [ ("questionnaires", "questionnairesL")
  , ("questions"     , "questionsL"     )
  , ("answers"       , "answersL"       )
  ]
  ''InMemoryState
```

---

Now we have the `questionnairesL` lens, which allows us to focus the `questionnaires` field of the `InMemoryState` data structure.

We can now use the [`over`](https://hackage.haskell.org/package/microlens-0.4.13.0/docs/Lens-Micro.html#v:over) combinator to lift our function which modifies a `Map` to a function which modifies a `InMemoryState`.

```haskell
  _ . over questionnairesL $ insert id questionnaire
```

---

Now we have a function to modify the content of our `TVar`.

If we look up the documentation of the [`TVar` module](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Concurrent-STM-TVar.html#t:TVar), we find there is a [`modifyTVar`](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Concurrent-STM-TVar.html#v:modifyTVar) function which allows us to modify the content of a `TVar`.

```haskell
  _ . modifyTVar memory . over questionnairesL $ insert id questionnaire
```

---

At last, we need a function to atomically execute our modifications in the `IO` context.

Such a function is called [`atomically`](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Monad-STM.html#v:atomically).

```haskell
  atomically . modifyTVar memory . over questionnairesL $ insert id questionnaire
```

---

To implement the rest of the repositories just

```bash
git checkout chapter4.1
```

---

Now that we have all the repositories, we can implement

```haskell
statefulAppServices = AppServices
  { questionnaireRepository = _
  , questionRepository      = _
  , submissionRepository    = _
  , answerRepository        = _
  }
```

---

The repositories we implemented now live in `IO`, while `AppServices` requires them in the `Handler` context.

As we did for the Postgres case, we can hoist them by mapping from one context to the other.

---

In this case, it's enough to use `liftIO`.

```haskell
import Api.AppServices
import Api.InMemoryState
import Api.StatefulAnswerRepository
import Api.StatefulSubmissionRepository
import Api.StatefulQuestionRepository
import Api.StatefulQuestionnaireRepository
import Domain.AnswerRepository as Answer
import Domain.SubmissionRepository as Submission
import Domain.QuestionnaireRepository as Questionnaire
import Domain.QuestionRepository as Question

-- base
import Control.Monad.IO.Class

-- stm
import Control.Concurrent.STM

statefulAppServices :: TVar InMemoryState -> AppServices
statefulAppServices memory = AppServices
  { questionnaireRepository = Questionnaire.hoist liftIO $ statefulQuestionnaireRepository memory
  , questionRepository      = Question.hoist      liftIO $ statefulQuestionRepository memory
  , submissionRepository    = Submission.hoist     liftIO $ statefulSubmissionRepository memory
  , answerRepository        = Answer.hoist        liftIO $ statefulAnswerRepository memory
  }
```

---

We can now use our `statefulAppServices` in our test

```haskell
import Api.StatefulAppServices

    let forms = formsServer $ statefulAppServices _
```

---

We need to initialize our state.

We would like to be initially empty

```haskell
emptyState :: InMemoryState
emptyState = InMemoryState
  { questionnaires = empty
  , questions = empty
  , answers = empty
  }
```

---

And, at last, use `newTVarIO` to initialize our mutable variable

```haskell
    initialMemory <- runIO $ newTVarIO emptyState
    let forms = formsServer $ statefulAppServices initialMemory
```

---

Now the test runs and we can finally see that it fails!

Now it's up to you fixing it!

---

So long, and thanks for all the fish!

---
