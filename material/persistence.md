# Forms - Persistence

---

```
https://gitpod.io#snapshot/1c93367c-4ed8-4ee1-9a10-9353a7834d3a
```

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

We can create this schema by running

```bash
docker-compose run -e PGPASSWORD=pwd -v $PWD/schema.sql:/schema.sql postgres psql -h postgres -U user -d db -w -f /schema.sql
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

We could do the same thing for `Question` and `Answer`, following the schema presented above.

---

```bash
git checkout chapter3.1
```

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

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
```

To compute the result of `ma >>= f` we need to use `f`, which needs to use as argument something that can be obtained only through `ma`. This forces `ma` to be computed before `f`.

---

`(>>=)` is actually equivalent (exercise: try to prove it!) to

```haskell
(<=<) :: (b -> m c) -> (a -> m b) -> a -> m c

-- compare it to
(.)   :: (b ->   c) -> (a ->   b) -> a ->   c
```

Monads allow us to compose functions which produce results in a given context.

---

As you might expect, `IO` is a monad, and `Query` is a monad.

---

Let's go back to our query.

We then filter only the `questions` which have the correct `Id Questionnaire`

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

Exercise for you: try to write a query to retrieve all the answers for a given question

```haskell
-- SELECT * FROM answer
-- WHERE question_id = :question_id
questionAnswers :: Id Domain.Question -> Query (Answer Expr)
questionAnswers questionId = do
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. lit questionId
  pure answer
```

---

```bash
git checkout chapter3.2
```

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
import Domain.Id

postgresQuestionnaireRepository :: QuestionnaireRepository IO

postgresAddQuestionnaire :: Domain.Questionnaire -> IO (Id Domain.Questionnaire)
postgresAddQuestionnaire questionnaire = do
  id <- generate
  _
```

---

Now we have all the data necessary to convert the domain representation to the database representation

We get all the conversion functions ready in the `Infrastructure.Serializer` module

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

where `add` is a query we defined in the `Persistence` module

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

Last thing we need to do is return the newly crafted `id`. We need to be careful though to manage the case in which the query actually failed. This is signalled by the `Either` returned by `run`.

The most sensible thing we can do is pass on the information back to the caller

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
postgresAllQuestionnaires :: Connection -> ExceptT QueryError IO [Identified Domain.Questionnaire]
postgresAllQuestionnaires connection = do
  questionnaires <- ExceptT $ run (statement () . select $ DB.allQuestionnaires) connection
  pure $ deserializeQuestionnaire <$> questionnaires
```

---

Similarly, we can implement all the other repositories `PostgresQuestionRepository`, `PostgresAnswerSetRepository` and `PostgresAnswerRepository`.

---

```bash
git checkout chapter3.3
```

---

Now we need to connect the domain to the concrete implementation of the repositories.

The place to do that is the `AppServices` argument to our `formsServer`.

We need to build a `AppServices` using our newly created repositories.

```haskell
postgresAppServices :: AppServices
postgresAppServices = AppServices
  { questionnaireRepository = _
  , questionRepository      = _
  , answerSetRepository     = _
  , answerRepository        = _
  }
```

---

`AppServices` works in the `Handler` context, while our concrete repositories work in the `ExceptT QueryError IO` context.

We need a way to a move from the latter to the former

---

What we want is a way to migrate between contexts

```haskell
hoist :: QuestionnaireRepository m -> QuestionnaireRepository n
```

---

Let's start implementing `hoist` following the types, to find out what we actually need

```haskell
hoist :: QuestionnaireRepository m -> QuestionnaireRepository n
hoist (QuestionnaireRepository add all) = QuestionnaireRepository _ _
```

---

The first hole has type

```haskell
Questionnaire -> n (Id Questionnaire)
```

while we have `add :: Questionnaire -> m (Id Questionnaire)`

---

The only sensible thing which we can do is first use `add`

```haskell
hoist (QuestionnaireRepository add all) = QuestionnaireRepository
  (_ . add)
  _
```

Now the hole has type `m (Id Questionnaire) -> n (Id Questionnaire)`

---

Let's turn our attention to the other hole, now. It has type `n [Identified Questionnaire]`.

We have `all :: m [Identified Questionnaire]`.

```haskell
hoist (QuestionnaireRepository add all) = QuestionnaireRepository
  (_ . add)
  (_ all)
```

If we use it we are left with a `m [Identified Questionnaire] -> n [Identified Questionnaire]` hole

---

Notice now the similarity in the structure of both holes; they both have the form `m a -> n a`. Let's try to pass such a function as an argument.

```haskell
hoist :: (m a -> n a) -> QuestionnaireRepository m -> QuestionnaireRepository n
hoist f (QuestionnaireRepository add all) = QuestionnaireRepository
  (f . add)
  (f all)
```

Ouch, it doesn't work!

---

This happens because when there is a type variable in a function signature, the caller has the ability to choose the actual value of that type variable.

On the contrary, here we would like a function which works uniformly for every `a`, and then the implementation chooses the concrete `a` (even different ones) every time.

---

To do this, we need to mention explicitly in our type that the provided function should work for all possible `a`.

We use the `forall` syntax to make that explicit.

```haskell
{-# LANGUAGE RankNTypes #-}

hoist :: (forall a. m a -> n a) -> QuestionnaireRepository m -> QuestionnaireRepository n
hoist f (QuestionnaireRepository add all) = QuestionnaireRepository
  (f . add)
  (f all)
```

`hoist` then becomes what is known as a rank-2 function.

---

Similarly, we need a `hoist` function for the other repositories.

---

```bash
git checkout chapter3.4
```

---

We can now progress with the definition of `postgresAppServices`

```haskell
import Domain.AnswerRepository as Answer
import Domain.AnswerSetRepository as AnswerSet
import Domain.QuestionnaireRepository as Questionnaire
import Domain.QuestionRepository as Question
import Infrastructure.PostgresAnswerRepository
import Infrastructure.PostgresAnswerSetRepository
import Infrastructure.PostgresQuestionRepository
import Infrastructure.PostgresQuestionnaireRepository

postgresAppServices connection = AppServices
  { questionnaireRepository = Questionnaire.hoist _ $ postgresQuestionnaireRepository connection
  , questionRepository      = Question.hoist      _ $ postgresQuestionRepository connection
  , answerSetRepository     = AnswerSet.hoist     _ $ postgresAnswerSetRepository connection
  , answerRepository        = Answer.hoist        _ $ postgresAnswerRepository connection
  }
```

---

Now all four holes require a function `ExceptT QueryError IO a -> Handler a`.

We can use the same function everywhere

```haskell
-- hasql
import Hasql.Session

-- transformers
import Control.Monad.Trans.Except

postgresAppServices connection = AppServices
  { questionnaireRepository = Questionnaire.hoist f $ postgresQuestionnaireRepository connection
  , questionRepository      = Question.hoist      f $ postgresQuestionRepository connection
  , answerSetRepository     = AnswerSet.hoist     f $ postgresAnswerSetRepository connection
  , answerRepository        = Answer.hoist        f $ postgresAnswerRepository connection
  }
  where
    f :: ExceptT QueryError IO a -> Handler a
    f exceptT = _
```

---

We can use the `Handler` constructor

```haskell
    f exceptT = Handler _
```

which leaves us with a `ExceptT ServerError IO a` hole. It looks similar to our `exceptT :: ExceptT QueryError IO a` type, but the error type is different.

---

We can use [`withExceptT`](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Except.html#v:withExceptT) to go from on type error to the other

```haskell
    f exceptT = Handler $ withExceptT _ exceptT
```

where the hole has type `QueryError -> ServerError`.

---

We need to decide how to handle the database errors at the server level.

To keep things simple, we always return a 500 response with the query error in the body.

```haskell
-- bytestring
import Data.ByteString.Lazy.Char8

f exceptT = Handler $ withExceptT (\queryError -> err500 {errBody = pack $ show queryError}) exceptT
```

---

Now we are left with actually serving our API.

Let's do it in a `src/Api/Application.hs` file.

```haskell
module Api.Application where
```

---

First we can define an `Application` as

```haskell
import Api.AppServices
import Api.Forms

-- base
import Data.Proxy

-- servant-server
import Servant

app :: AppServices -> Application
app appServices = serve (Proxy :: Proxy (NamedRoutes FormsApi)) (formsServer appServices)
```

---

`Proxy` is an interesting data type, using a phantom type to pass information at the type level

```haskell
data Proxy a = Proxy
```

---

At last, we can write our `main` function

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- base
import Data.Maybe

-- bytestring
import Data.ByteString.Char8

-- hasql
import Hasql.Connection

-- warp
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  connection <- acquire "host=postgres port=5432 dbname=db user=user password=pwd"
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (run 8080 . app . postgresAppServices)
    connection
```

We try to connect to the database.

If the connection fails we exit immediately with an error message.

Otherwise, we build our `AppServices`, and we run our application on port 8080.

---

```bash
git checkout chapter3.5
```

---

After all this, we can finally run our server!

```bash
stack exec forms
```

---

Once the server is running we can start sending requests

```bash
curl --request POST \
  --url http://localhost:8080/create-questionnaire \
  --header 'Content-Type: application/json' \
  --data '{
	"title": "a Questionnaire"
}'
```

```bash
curl --request GET \
  --url http://localhost:8080/questionnaires
```

```bash
curl --request POST \
  --url http://localhost:8080/add-question \
  --header 'Content-Type: application/json' \
  --data '{
	"title": "a question",
	"answerType": "Paragraph",
	"questionnaireId": "d0243d02-13e1-46cc-98a2-25ec61bcb203"
}'
```

```bash
curl --request GET \
  --url http://localhost:8080/questions/d0243d02-13e1-46cc-98a2-25ec61bcb203
```

```bash
curl --request POST \
  --url http://localhost:8080/record-answer-set \
  --header 'Content-Type: application/json' \
  --data '[
	{
		"content": {
			"tag": "Paragraph",
			"contents": "the answer"
		},
		"questionId": "b4d8a29e-de67-4b14-b2c5-049ecece89f5"
	}
]'
```

```bash
curl --request GET \
  --url http://localhost:8080/answer-sets/d0243d02-13e1-46cc-98a2-25ec61bcb203
```

```bash
curl --request GET \
  --url http://localhost:8080/set-answers/33c9522f-527a-4f3d-9e65-3873e79a4229
```

```bash
curl --request GET \
  --url http://localhost:8080/question-answers/b4d8a29e-de67-4b14-b2c5-049ecece89f5
```

---

## Learned concepts

- higher kinded data
- deriving strategies and `newtype` deriving
- `do` works for any `Monad`
- separating interface from implementation
- monad transformers
