# Forms - Persistence

---

The next step we want to tackle is persisting our data.

---

In the Haskell ecosystem there are several libraries one could choose to interact with the database.

They differ a lot in the degree of abstraction they add on top of the database.

The simplest are [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) and [mysql-simple](https://hackage.haskell.org/package/mysql-simple)

---

We are going to use [rel8](https://hackage.haskell.org/package/rel8), a library which helps us to abstract away from the database and have a very Haskell-oriented approach

```yaml
-- package.yaml
dependencies:
  - rel8
```

---

First off, We need to think about our database schema

---

```mermaid
erDiagram
  QUESTIONNAIRE ||--o{ QUESTION : contains
  QUESTION ||--o{ ANSWER : "is answered by"
  QUESTIONNAIRE {
    id    Id Questionnaire
    title Text
  }
  QUESTION {
    id               Id Question
    questionnaire_id Id Questionnaire
    title            Text
    answer_type      AnswerType
  }
  ANSWER {
    id          Id Answer
    question_id Id Question
    set_id      Id AnswerSet
    content     Content
  }
```

---

Let's start working on a new file, say `Infrastructure/Persistence.hs`

```haskell
module Infrastructure.Persistence where
```

---

We need to define the data types necessary to describe the database schema to our application

---

Let's start with questionnaires

```haskell
import Domain.Id
import qualified Domain.Questionnaire as Domain

-- rel8
import Rel8

-- text
import Data.Text

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f (Id Domain.Questionnaire)
  , questionnaireTitle :: Column f Text
  }
```

---

To distinguish between data types with the same name, we use qualified imports, which require us to use a namespace

---

Now, what is that `f`?

It describes the context (e.g. documentation/query expressions/results) in which the data need to be considered

---

This approach is usually called [Higher-kinded data](https://reasonablypolymorphic.com/blog/higher-kinded-data/).

It is useful to distinguish the shape of data from the context in which they are considered

---

To please `Rel8`, we need to add some deriving clauses

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- base
import GHC.Generics

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f (Id Domain.Questionnaire)
  , questionnaireTitle :: Column f Text
  }
  deriving (Generic, Rel8able)
```

---

We are missing an instance for `DBType (Id Domain.Questionnaire)`, which we can derive

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- rel8
import Rel8

newtype Id a = Id UUID
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, DBType)
```

---

Notice the `newtype` keyword. That means that instead of deriving instances looking at the generic structure of the data type, we use directly the instance provided by the inner type.

---

Next we need to connect this Haskell representation to the database representation. We do this by defining the schema for the questionnaire table

```haskell
questionnaireSchema :: TableSchema (Questionnaire Name)
```

---

A `TableSchema (Questionnaire Name)` requires us to provide a `name`, a `schema`, and a name of a column for every field of a `Questionnaire`

```haskell
{-# LANGUAGE OverloadedStrings #-}

questionnaireSchema = TableSchema
  { name = "questionnaire"
  , schema = Nothing
  , columns = Questionnaire
    { questionnaireId    = "id"
    , questionnaireTitle = "title"
    }
  }
```

---

This is our first example of a context

```haskell
data Name a = Name String
```

`Name a` is a datatype that contains a `String` for every type `a`.

It is useful for documentation.

---

This allows us to connect the Haskell representation of our data with the database representation, while keeping the two completely separate.

---

Now you could try to do the same thing for `Question` and `Answer`, following the schema presented above.

---

```haskell
import qualified Domain.Answer as Domain
import Domain.Answer (Content)
import qualified Domain.Question as Domain
import Domain.Question (AnswerType)

data Question f = Question
  { questionId              :: Column f (Id Domain.Question)
  , questionQuestionnaireId :: Column f (Id Domain.Questionnaire)
  , questionTitle           :: Column f Text
  , questionAnswerType      :: Column f AnswerType
  }
  deriving (Generic, Rel8able)

questionSchema :: TableSchema (Question Name)
questionSchema = TableSchema
  { name    = "question"
  , schema  = Nothing
  , columns = Question
    { questionId              = "id"
    , questionQuestionnaireId = "questionnaire_id"
    , questionTitle           = "title"
    , questionAnswerType      = "answer_type"
    }
  }

data Answer f = Answer
  { answerId         :: Column f (Id Domain.Answer)
  , answerQuestionId :: Column f (Id Domain.Question)
  , answerSetId      :: Column f (Id Domain.AnswerSet)
  , answerContent    :: Column f Content
  }
  deriving (Generic, Rel8able)

answerSchema :: TableSchema (Answer Name)
answerSchema = TableSchema
  { name    = "answer"
  , schema  = Nothing
  , columns = Answer
    { answerId         = "id"
    , answerQuestionId = "question_id"
    , answerSetId      = "set_id"
    , answerContent    = "content"
    }
  }
```

---

To use `AnswerType` and `Content`, they need to have an instance of `DBType`. Following [the documentation](https://rel8.readthedocs.io/en/latest/concepts/dbtype.html?highlight=ReadShow#deriving-dbtype-via-readshow) of `Rel8`, we can just cleverly derive it

```haskell
{-# LANGUAGE DerivingVia #-}

-- rel8
import Rel8

data AnswerType
  = Paragraph
  | Number
  deriving stock (Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving DBType via ReadShow AnswerType
```

```haskell
{-# LANGUAGE DerivingVia #-}

-- rel8
import Rel8

data Content
  = Paragraph Text
  | Number Int
  deriving stock (Generic, Read, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving DBType via ReadShow Content
```

---

---

Now it is time to start writing some queries!

---

The first query we want to write is to extract all the available questionnaires:

```haskell
-- SQL: SELECT * FROM questionnaire
allQuestionnaires = each questionnaireSchema
```

We are defining a value which represents the query which extracts all rows from the table described by the `questionnaireSchema`.

---

The type of `allQuestionnaires` is

```haskell
allQuestionnaires :: Query (Questionnaire Expr)
```

It means that `allQuestionnaires` is a `Query` producing `Questionnaire`s in the `Expr` context.

We use the `Expr` context to create valid typed SQL expressions.

---

Next we want to retrieve all the `Question`s for a single `Questionnaire`.

```sql
SELECT * FROM question
WHERE questionnaier_id = :questionnaire_id
```

---

We want to create a `Query` which produces `Question`s, given a specific `QuestionnaireId`

```haskell
questionnaireQuestions :: Id Domain.Questionnaire -> Query (Question Expr)
```

---

We start by retrieving all the `Question`s


```haskell
questionnaireQuestions questionnaireId = do
  question <- each questionSchema
  _
```

---

This is the same `do` notation we saw for `IO`

---

Actually, `do` notation is far more general and works for every [`Monad`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:Monad).

For our purposes a monad instance on a data structure allows executing sequential computation is a given context.

---

Monads encode the ability to perform sequential computations in a given context.

This focus on contexts is what makes monads so important in Haskell and why they are not that common in other programming languages.

---

As you might expect, `IO` is a monad, and `Query` is a monad.

---

And then we filter only the `questions` which have the correct `Id Questionnaire`

`Rel8` offers us a `where_` combinator which allows us to filter based on a criterion.

```haskell
  where_ $ _
  _
```

where the first `_` has type `Expr Bool`

---

First we want to extract the `questionQuestionnaireId`.

We can use the `questionQuestionnaireId` field as a function

```haskell
  where_ $ _ (questionQuestionnaireId question)
```

and be left with a hole `_ :: Expr (Id Questionnaire) -> Expr Bool` to fill.

---

We need to compare our `questionQuestionnaireId question` with the `questionId` we got as input.

We can do this using the [`(==.)`](https://hackage.haskell.org/package/rel8-1.3.1.0/docs/Rel8.html#v:-61--61-.) operator.

```haskell
  where_ $ questionQuestionnaireId question ==. _
```

---

The compiler is signalling us that we are missing a `DBEq` instance on `Id Questionnaire`, which is needed to compare fields for equality.

```haskell
  deriving newtype (DBType, DBEq)
```

---

Now we need a value of type `Expr (Id Questionnaire)` to fill our remaining hole.

We can use the [`lit`](https://hackage.haskell.org/package/rel8-1.3.1.0/docs/Rel8.html#v:lit) function to lift our `Id Questionnaire` to the `Expr` context.

```haskell
  where_ $ questionQuestionnaireId question ==. lit questionnaireId
```

---

As a last step, we need to return a value

```haskell
questionnaireQuestions questionnaireId = do
  question <- each questionSchema
  where_ $ questionQuestionnaireId question ==. lit questionnaireId
  pure question
```

---

As an exercise, try to implement yourself the query to retrieve all the answers for a specific questionnaire

---

```haskell
-- SELECT * FROM answer
-- JOIN (SELECT * FROM question
--       WHERE questionnaire_id = :questionnaire_id) AS questions
-- WHERE answer.question_id = questions.id
questionnaireAnswers :: Id Domain.Questionnaire -> Query (Answer Expr)
questionnaireAnswers questionnaireId = do
  question <- questionnaireQuestions questionnaireId
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. questionId question
  pure answer
```

---

Now we would like to execute this last query and print results to the console

We're not going to analyze the code in detail, we're just going to check that it works

---

First, we need to be able to print some `Answer Result`

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

deriving stock instance f ~ Result => Show (Answer f)
```

---

We need also to add a `Show` instance for the `Id a` data type.

```haskell
newtype Id a = Id UUID
  deriving newtype (Show, ...)
```

---

Then we can write our `main` function to run the query and print its results

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Domain.Id
import Infrastructure.Persistence

-- base
import Data.Maybe

-- bytestring
import Data.ByteString.Char8

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- uuid
import Data.UUID

main :: IO ()
main = do
  connection <- acquire "host=localhost port=5432 dbname=db user=user password=pwd"
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> do
      response <- run (statement () . select $ questionnaireAnswers (Id nil)) connection'
      print response)
    connection
```

---

And we can try to run it with

```bash
stack exec forms
```

The results will depend on the data you have in your database

---

---

Now we want to connect our database to our domain logic.

We can do that by implementing the repositories which we introduced last time.

---

These concrete implementations are part of the infrastructure layer

```haskell
module Infrastructure.PostgresQuestionnaireRepository where
```

---

Let's start by defining a concrete implementation of our interface and take it from there

```haskell
import Domain.Id
import qualified Domain.Questionnaire as Domain
import Domain.QuestionnaireRepository

-- base
import Prelude hiding (all)

postgresQuestionnaireRepository :: QuestionnaireRepository m
postgresQuestionnaireRepository = QuestionnaireRepository
  { add = postgresAddQuestionnaire
  , all = postgresAllQuestionnaires
  }

postgresAddQuestionnaire :: Domain.Questionnaire -> m (Id Domain.Questionnaire)
postgresAddQuestionnaire questionnaire = _

postgresAllQuestionnaires :: m [Identified Domain.Questionnaire]
postgresAllQuestionnaires = _
```

---

First we're going to implement `postgresAddQuestionnaire`.

What we need to do is:
- create a new `Id Questionnaire`
- convert from the domain representation to the database representation
- persist the data
- return the newly created `Id`

---

The implementation we are looking for is clearly sequential. This means that the context `m` in which we are working needs to be a `Monad` and that we can use `do` notation.

```haskell
postgresAddQuestionnaire :: Monad m => Domain.Questionnaire -> m (Id Domain.Questionnaire)
postgresAddQuestionnaire questionnaire = do
  _
```

---

The `Monad m` constraint needs to be propagated to `postgresQuestionnaireRepository`

```haskell
postgresQuestionnaireRepository :: Monad m => QuestionnaireRepository m
```

---

The first implementation step is creating a new `Id Questionnaire`.

We can generate one by generating a `UUID` with `nextRandom :: IO UUID` and then wrapping it with the `Id` constructor

```haskell
import Data.UUID.V4

generate :: IO (Id a)
generate = Id <$> nextRandom
```

---

We notice now that `generate` is operating in the `IO` context, while we are working in a generic monadic context `m`.

---

The easiest way to make it all work is to use `IO` instead of `m`, knowing that we might need to improve on it later.

```haskell
postgresQuestionnaireRepository :: QuestionnaireRepository IO

postgresAddQuestionnaire :: Domain.Questionnaire -> IO (Id Domain.Questionnaire)
postgresAddQuestionnaire questionnaire = do
  id <- generate
  _
```

---

Now we have all the data necessary to convert the domain representation to the database representation

---

We provide all the conversion functions in a specific `Infrastructure.Serializer` module

```haskell
module Infrastructure.Serializer where

import Domain.Id
import qualified Domain.Questionnaire as Domain
import Infrastructure.Persistence

-- rel8
import Rel8

serializeQuestionnaire :: Identified Domain.Questionnaire -> Questionnaire Result
serializeQuestionnaire (Identified questionnaireId questionnaire) = _
```

---

Constructing a `Questionnaire Result` is easy. We just need to use the `Questionnaire` constructor

```haskell
serializeQuestionnaire (Identified questionnaireId questionnaire) = Questionnaire
  { questionnaireId    = _
  , questionnaireTitle = _
  }
```

---

And then we just fill the holes using the input values

```haskell
import qualified Domain.Forms.Questionnaire as Questionnaire

serializeQuestionnaire (Identified questionnaireId (Domain.Questionnaire title)) = Questionnaire
  { questionnaireId    = questionnaireId
  , questionnaireTitle = title
  }
```

---

Now we can use our serialization function in the implementation of `postgresCreateNewQuestionnaire`

```haskell
import Infrastructure.Serializer

postgresAddQuestionnaire questionnaire = do
  id <- generate
  let serializedQuestionnaire = serializeQuestionnaire (Identified id questionnaire)
  _
```

---

Now we can create our query

```haskell
import qualified Infrastructure.Persistence as DB

-- rel8
import Rel8

postgresAddQuestionnaire questionnaire = do
  id <- generate
  let serializedQuestionnaire = serializeQuestionnaire id questionnaire
      addQuestionnaire        = DB.add DB.questionnaireSchema [lit serializedQuestionnaire]
  _
```

where `add` is a query we need to define in the `Persistence` module

```haskell
import qualified Rel8 as Insert (Insert(returning))

add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Insert ()
add schema rows' = Insert
  { into             = schema
  , rows             = values rows'
  , onConflict       = Abort
  , Insert.returning = pure ()
  }
```

---

And run it

```haskell
-- hasql
import Hasql.Session

postgresAddQuestionnaire questionnaire = do
  id <- generate
  let serializedQuestionnaire = serializeQuestionnaire (Identified id questionnaire)
      addQuestionnaire        = DB.add DB.questionnaireSchema [lit serializedQuestionnaire]
  eitherError <- run (statement () . insert $ addQuestionnaire) connection
  _
```

---

`run` requires a `connection` to work, so we need to pass that as an argument

```haskell
import Hasql.Connection

postgresAddQuestionnaire :: Connection -> Domain.Questionnaire -> IO (Id Domain.Questionnaire)
postgresAddQuestionnaire connection questionnaire = do
  id <- generate
  let serializedQuestionnaire = serializeQuestionnaire (Identified id questionnaire)
      addQuestionnaire        = DB.add DB.questionnaireSchema [lit serializedQuestionnaire]
  eitherError <- run (statement () . insert $ addQuestionnaire) connection
  _
```

---

We need to propagate that argument to `postgresQuestionnaireRepository`

```haskell
postgresQuestionnaireRepository :: Connection -> QuestionnaireRepository IO
postgresQuestionnaireRepository connection = QuestionnaireRepository
  { add = postgresAddQuestionnaire connection
  , all = postgresAllQuestionnaires
  }
```

---

Last thing we need to do is return the newly crafted `id`. We need to be careful though to manage carefully the case in which the query actually failed. This is signalled by the `Either` returned by `run`.

The most sensible thing we can do is pass on the information to the caller

```haskell
postgresAddQuestionnaire :: Connection -> Domain.Questionnaire -> IO (Either QueryError (Id Domain.Questionnaire))
```

This does not work since the return type of `add` for a `QuestionnaireRepository` needs to be of the form `m (Id Questionnaire)`.

We need to somehow move the `Either QueryError` inside the `m`.

---

The solution is to use a monad transformer, which allows integrating `IO` and `Either QueryError` in the same context

```haskell
-- from Control.Monad.Trans.Except in the `transformers` package

newtype ExceptT e m a = ExceptT (m (Either e a))
```

If we specialize this to `m = IO` and `e = QueryError`, we get exactly the type we're using

---

What do we gain?

```haskell
instance (Monad m) => Monad (ExceptT e m)
```

Moreover, the monad instance handles failures automatically, i.e.

```haskell
foo :: Monad m => EitherT e m a
foo = do
  a
  b
```

If `a` fails (i.e. it returns a `Left`), then `b` is skipped and the whole computation fails with the error result of `a`.

---

We can adapt our code to use `ExceptT QueryError IO`

```haskell
postgresAddQuestionnaire :: Connection -> Domain.Questionnaire -> ExceptT QueryError IO (Id Domain.Questionnaire)
```

we need to update also

```haskell
postgresQuestionnaireRepository :: Connection -> QuestionnaireRepository (ExceptT QueryError IO)
```

---

Now `generate` is no more OK, because it returns something in `IO`, while here we are working in `ExceptT QueryError IO`.

Luckily we have a function `liftIO :: IO a -> ExceptT e m a ` which allows us to lift values in `IO` to `EitherT e m`.

```haskell
-- base
import Control.Monad.IO.Class

postgresAddQuestionnaire connection questionnaire = do
  id <- liftIO generate
  ...
```

---

Similarly, `run` does not work because it returns a `IO (Either QueryError ())`.

We just need to wrap it in the `ExceptT` newtype.

The code now becomes

```haskell
postgresAddQuestionnaire connection questionnaire = do
  id <- liftIO generate
  let serializedQuestionnaire = serializeQuestionnaire (Identified id questionnaire)
      addQuestionnaire        = DB.add DB.questionnaireSchema [lit serializedQuestionnaire]
  ExceptT $ run (statement () . insert $ addQuestionnaire) connection
  _
```

---

Eventually, we can return our desired value

```haskell
postgresAddQuestionnaire connection questionnaire = do
  id <- liftIO generate
  let serializedQuestionnaire = serializeQuestionnaire (Identified id questionnaire)
      addQuestionnaire        = DB.add DB.questionnaireSchema [lit serializedQuestionnaire]
  ExceptT $ run (statement () . insert $ addQuestionnaire) connection
  pure id
```

---

Exercise for you: implement the `postgresAllQuestionnaires` function

```haskell
-- Infrastructure.Serializer

deserializeQuestionnaire :: Questionnaire Result -> Identified Domain.Questionnaire
deserializeQuestionnaire (Questionnaire id title) = Identified
  { id = id
  , entity = Domain.Questionnaire title
  }

-- Infrastructure.PostgresQuestionnaireRepository

postgresAllQuestionnaires :: Connection -> ExceptT QueryError IO [Identified Domain.Questionnaire]
postgresAllQuestionnaires connection = do
  questionnaires <- ExceptT $ run (statement () . select $ DB.allQuestionnaires) connection
  pure $ deserializeQuestionnaire <$> questionnaires
```

---

## Learned concepts

- higher kinded data
- deriving strategies and `newtype` deriving
- `do` works for any `Monad`
- separating interface from implementation
- monad transformers
