# Forms - Web API

---

In this second chapter we want to transform our terminal application into a more structured web API which somehow resembles the functioning of Google Forms

---

Let's start to think about the web API endpoints we would like to have:

| request | method | request | response |
| ------- | ------ | ------- | -------- |
| create a new `questionnaire` | `POST` | `questionnaire` `title` | `questionnaire` `id` |
| retrieve all `questionnaire`s | `GET` || list of `questionnaire`s with their `id`s |
| add new `question` to a `questionnaire` | `POST` | `title`, `answerType`, `questionnaire` `id` | `question` `id` |
| retrieve `question`s for `questionnaire` | `GET` | `questionnaire` `id` | list of `question`s with their `id`s |
| record `set` of `answer`s for a `questionnaire` | `POST` | list of `answer`s | `id` for the `set` of `answer`s |
| retrieve `set`s of `answer`s for a `questionnaire` | `GET` | `questionnaire` `id` | list of `answer` `set` `id`s |
| retrieve all `answer`s for a given `set` | `GET` | `answer` `set` `id` | list of `answer`s with their `id`s |
| retrieve all `answer`s for a given `question` | `GET` | `question` `id` | list of `answer`s with their `id`s |

---

What we would like to do now is to encode this information in our code, so that we can use it to guide our implementation for a server providing these endpoints.

---

As a first step, we want to define some data types to describe the payloads of our requests and responses.

This will help us to work with a high level representation of the data we actually want to transmit on the network.

---

Let's start from a `Questionnaire`, which is characterized just by its title

```haskell
module Domain.Questionnaire where

-- text
import Data.Text

newtype Questionnaire = Questionnaire
  { title :: Text
  }
```

---

Next we want to think about `Question`s.

To add a new question to a `Questionnaire` we actually need the `Question` `title`, `answerType` and `Questionnaire` `id`

```haskell
module Domain.Question where

-- text
import Data.Text

data Question = Question
  { title           :: Text
  , answerType      :: AnswerType
  , questionnaireId :: QuestionnaireId
  }
```

---

We can copy the `AnsewrType` data type from our previous chapter

```haskell
data AnswerType
  = Paragraph
  | Number
```

---

For the `QuestionnaireId` we want to use a `UUID` using a newtype to distinguish it from other `id`s.

```haskell
-- uuid
import Data.UUID

newtype QuestionnaireId = QuestionnaireId UUID
```

---

We also need to import the `uuid` package

```yaml
dependencies:
  - uuid
```

---

The last relevant entity is `Answer`, which is identified by its `content`, a `set` `id` and a `question` `id`

```haskell
module Domain.Ansewr where

data Answer = Answer
  { content    :: Content
  , setId      :: SetId
  , questionId :: QuestionId
  }
```

---

The `Content` data type is just what we were calling `Answer` in our previous chapter

```haskell
-- text
import Data.Text

data Content
  = Paragraph Text
  | Number Int
```

---

We can define `SetId` and `QuestionId` similarly to how we defined `QuestionnaireId`

```haskell
--uuid
import Data.UUID

newtype SetId = SetId UUID

newtype QuestionId = QuestionId UUID
```

---

All these similarly defined `Id`s really call for a refactoring unifying them in a single type.

Still, we want to keep them distinct at the type level. This way, the type system will help us avoid any confusion among them.

We could use a [phantom type](https://wiki.haskell.org/Phantom_type), a type variable which is not referring to anything at the value level

```haskell
module Domain.Id where

--uuid
import Data.UUID

newtype Id a = Id UUID
```

---

Now `Id Questionnaire`, `Id Question` and `Id Answer` are all containing a `UUID`, while being distinct at the type level.

We can now ditch `QuestionnaireId` and `QuestionId`.

---

Actually also `SetId` has the same format, but we don't have a data type we could use to tag it.

Well, we can create it, can't we?

```haskell
data AnswerSet
```

And now we can use `Id AnswerSet`

---

---

Now that we completed defining our entities, we want to get back to the definition of the API.

We want to encode all the information regarding our API in a single Haskell data type!

This is work for the awesome [`Servant`](https://hackage.haskell.org/package/servant-0.19) library.

Servant is a Haskell library used to define web services API and serving them.

Warning: Servant internals are complicated! We will not dive into the details. We'll just learn how to use it.

---

Let's start by importing the `servant` and `servant-server` libraries

```yaml
-- package.yaml

dependencies:
  - servant
  - servant-server
```

---

Next, let's open a new file where we want to define our API

```haskell
module Api.Forms where
```

---

And let's start defining a new data type

```haskell
data FormsApi = FormsApi
  { createNewQuestionnaire :: _
  , questionnaires         :: _
  , addNewQuestion         :: _
  , questionnaireQuestions :: _
  , recordAnswerSet        :: _
  , answerSets             :: _
  , setIdAnswers           :: _
  , questionAnswers        :: _
  }
```

It's just a record with one field for every endpoint we want to have.

Mind that the holes do not work at the type level. It's just a placeholder for us.

---

Now it's time for a Servant technicality. To make the machinery work, the `FormsApi` data type needs a `mode` type variable which is then used on every route

```haskell
data FormsApi mode = FormsApi
  { createNewQuestionnaire :: mode :- _
  , questionnaires         :: mode :- _
  , addNewQuestion         :: mode :- _
  , questionnaireQuestions :: mode :- _
  , recordAnswerSet        :: mode :- _
  , answerSets             :: mode :- _
  , setIdAnswers           :: mode :- _
  , questionAnswers        :: mode :- _
  }
```

It will be used to specify is the API definition needs to be used for a server (that's our use case), a client, or something else.

---

We need to import the `:-` operator from the `Servant` library and enable the `TypeOperators` extensions to be able to use operators in a type definition.

```haskell
{-# LANGUAGE TypeOperators #-}

-- servant
import Servant.API.Generic
```

---

Next we can start defining the types of the actual endpoints.


```haskell
{-# LANGUAGE DataKinds #-}

-- servant
import Servant.API

data FormsApi mode = FormsApi
  { createNewQuestionnaire :: mode :- "create-questionnaire" :> ReqBody '[JSON] Questionnaire :> Post '[JSON] (Id Questionnaire)
  , ...
  }
```

---

Let's try to unpack this definition:

- `"create-questionnaire"` is the path
- `ReqBody '[JSON] Questionnaire` is saying that the request should contain a `Questionnaire` formatted as `JSON`
- `Post '[JSON] (Id Questionnaire)` is saying that it is a `Post` request which will return a `Id Questionnaire` formatted as `JSON`

---

Now we want to define the endpoint to retrieve all questionnaires

```haskell
data FormsApi mode = FormsApi
  { ...
  , questionnaires :: mode :- "questionnaires" :> Get '[JSON] [(Id Questionnaire, Questionnaire)]
  , ...
  }
```

---

Similarly to the previous endpoint we specify:

- `"questionnaires"` as the path
- the request does not contain any data, so we can omit it
- `Get  '[JSON] [(Id Questionnaire, Questionnaire)]` is saying that it is a `Get` request which will return a list of pairs of `Questionnaire`s and their `Id`s

---

Try to write yourself the endpoint to add a new question

```haskell
data FormsApi mode = FormsApi
  { ...
  , addNewQuestion :: mode :- "add-question" :> ReqBody '[JSON] Question :> Post '[JSON] (Id Question)
  , ...
  }
```

---

Let's next write together the endpoint to retrieve all the questions for a specific questionnaire

```haskell
data FormsApi mode = FormsApi
  { ...
  , questionnaireQuestions :: mode :- "questions" :> Capture "questionnaire" (Id Questionnaire) :> Get '[JSON] [(Id Question, Question)]
  , ...
  }
```

---

The new thing with respect to previous endpoints is the `Capture` keyword, which allows us to extract a value from a URL section.

This means that the actual path will be `questions/{questionnaireId}`.

---

You can try now to complete the definition of all the other routes

```haskell
data FormsApi mode = FormsApi
  { ...
  , recordAnswerSet :: mode :- "record-answer-set" :> ReqBody '[JSON] [Answer]                   :> Post '[JSON] (Id AnswerSet)
  , answerSets      :: mode :- "answer-sets"       :> Capture "questionnaire" (Id Questionnaire) :> Get  '[JSON] [Id AnswerSet]
  , setIdAnswers    :: mode :- "set-answers"       :> Capture "set" (Id AnswerSet)               :> Get  '[JSON] [(Id Answer, Answer)]
  , questionAnswers :: mode :- "question-answers"  :> Capture "question" (Id Question)           :> Get  '[JSON] [(Id Answer, Answer)]
  }
```

---

We realize that we're using many times the construct `(Id a, a)`.

It might make sense to define a data type to abstract that construct.

```haskell
-- Domain.Id
data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
```

We can then use it in our API definition.

---

---

Next, since we want to use `JSON` as our API language, we want to be able to encode to/decode from `JSON` all our domain entities.

To do this we will use the `aeson` library

```yaml
dependencies:
  - aeson
```

---

We'll take a look at the decoding phase.

---

The ability to decode a data type from JSON is encoded in the [`FromJSON`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#t:FromJSON) typeclass

```haskell
class FromJSON a where
  parseJSON :: Value -> Parser a
```

where `Value` is a data type representing a `JSON` value

```haskell
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
```

and [`Parser a`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#t:Parser) is the result of the parsing operation, which, if successful, returns a value of type `a`.

---

Let's try to decode a `Questionnaire`, first

```haskell
{-# LANGUAGE InstanceSigs #-}

-- aeson
import Data.Aeson.Types

instance FromJSON Questionnaire where
  parseJSON :: Value -> Parser Questionnaire
  parseJSON v = _
```

---

The first step is case splitting on the input `Value`

```haskell
instance FromJSON Questionnaire where
  parseJSON :: Value -> Parser Questionnaire
  parseJSON (Object o) = _wa
  parseJSON (Array a)  = _wb
  parseJSON (String s) = _wc
  parseJSON (Number n) = _wd
  parseJSON (Bool b)   = _we
  parseJSON Null       = _wf
```

---

We need to decide now what we want the JSON representation of a `Questionnaire` to be.

I'd say it makes sense to use an object with a `title` field.

Hence, all other options other than `Object` will need to fail.

The documentation of `FromJSON` describes the [`typeMismatch`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#v:typeMismatch) function exactly for this use case.

```haskell
instance FromJSON Questionnaire where
  parseJSON :: Value -> Parser Questionnaire
  parseJSON (Object o) = _wa
  parseJSON v          = typeMismatch "object" v
```

---

We can parse an object field with the [`.:`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#v:.:) operator

```haskell
{-# LANGUAGE OverloadedStrings #-}

  parseJSON (Object o) = _ (o .: "title" :: Parser Text)
```

We specify the type explicitly to avoid excessive polymorphism until we are not done with the implementation.

---

The hole now has type `Parser Text -> Parser Questionnaire`.

We can look at this as a map from `Text` to `Questionnaire` inside the `Parser` context.

---

We actually have a map from `Text` to `Questionnaire`, which is the `Questionnaire` constructor.

We need a way to lift it to the `Parser` context.

---

Haskell has a default mechanism to lift functions into contexts, and it is provided by the `Functor` type class

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

---

A `Functor` instance on a type constructor `f` means that we can take any function `g :: a -> b` and see it inside the context given by `f` as a function `fmap g :: f a -> f b`

---

It's better mention that there is also an infix version of `fmap`, which is the `<$>` operator

```haskell
(<$>) :: (a -> b) -> f a -> f b

-- compare it to
($)   :: (a -> b) ->   a ->   b
```

---

Now we can complete our definition for the `FromJSON Questionnaire` instance

```haskell
  parseJSON (Object o) = Questionnaire <$> o .: "title"
```

---

Next we would like to create an instance for `FromJSON Question`

```haskell
{-# LANGUAGE InstanceSigs #-}

--aeson
import Data.Aeson.Types

instance FromJSON Question where
  parseJSON :: Value -> Parser Question
  parseJSON v = _
```

---

Also in this case we want an object, so we can consider only that case

```haskell
  parseJSON (Object o) = _
  parseJSON v          = typeMismatch "Object" v
```

---

Similarly to what we did above, we want to parse first all the single fields and then combine them into the `Question` data type.

```haskell
  parseJSON (Object o) = _
    (o .: "title" :: Parser Text)
    (o .: "answer-type" :: Parser AnswerType)
    (o .: "questionnaire-id" :: Parser (Id Questionnaire))
```

---

To make this work we need first to have `FromJSON` instances for `AnswerType` and `Id Questionnaire`.

---

Let's start from the first

```haskell
instance FromJSON AnswerType where
  parseJSON :: Value -> Parser AnswerType
  parseJSON v = _
```

---

This time we want the JSON representation to be just a string

```haskell
  parseJSON (String s) = _wi
  parseJSON v          = typeMismatch "String" v
```

---

If the string equals `Number` or `Paragraph`, we return the appropriate value, otherwise we fail

```haskell
  parseJSON (String s) = case s of
    "Paragraph" -> pure Paragraph
    "Number"    -> pure Number
    _           -> fail "the only allowed values are Paragraph and Number"
```

Similarly to what we did for `IO`, we need to use the `pure` function to lift a value into the `Parser` context.

---

The instance for `Id Questionnaire` is actually even simpler, since we can use the already present instance for `UUID`

```haskell
{-# LANGUAGE InstanceSigs #-}

-- aeson
import Data.Aeson.Types

instance FromJSON (Id a) where
  parseJSON :: Value -> Parser (Id a)
  parseJSON v = Id <$> parseJSON v
```

---

Let's go back to our `FromJSON Question` instance

```haskell
  parseJSON (Object o) = _
    (o .: "title" :: Parser Text)
    (o .: "answer-type" :: Parser AnswerType)
    (o .: "questionnaire-id" :: Parser (Id Questionnaire))
```

The hole has type `Parser Text -> Parser AnswerType -> Parser (Id Questionnaire) -> Parser Question`

---

Similarly to what we did for `Questionnaire`, we can see this as a function `Text -> AnswerType -> Id Questionnaire -> Question` inside the `Parser` context.

We actually already have a `Text -> AnswerType -> Id Questionnaire -> Question`, which is the `Question` constructor.

What we would like to do is to lift it to the `Parser` context.

---

Previously we used a `Functor`. Does it work now?

Unluckily, it falls short

```haskell
f :: a -> b -> c -> d
fa :: f a
f <$> fa :: f (b -> c -> d)
```

---

To be able to lift functions of higher [arity](https://en.wikipedia.org/wiki/Arity) (i.e. with more arguments) we need something more powerful than `Functor`.

We need `Applicative`

```haskell
class Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Aha, here is where the `pure` lifting operation we were using comes from!

---

We are now interested in understanding how the `<*>` operator could be helpful.

It works like function application, but in a given context `f`

```haskell
(<*>) :: f (a -> b) -> f a -> f b

-- compare it to
($)   ::   (a -> b) ->   a ->   b
```

---

Notice that is consumes a function type `f (a -> b)` and returns a simpler type `f b`.

We can actually use it to simplify the `f <$> fa :: f (b -> c -> d)` we had above

```haskell
f :: a -> b -> c -> d
fa :: f a
f <$> fa :: f (b -> c -> d)

<*> :: f (b -> c -> d) -> f b -> f (c -> d)

fb :: f b
f <$> fa <*> fb :: f (c -> d)

fc :: f c
f <$> fa <*> fb <*> fc :: f d
```

---

To sum up, we can use `<$>` and `<*>` to lift a function of any arity into an `Applicative` context.

Exercise: try to write a `lift3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d` function in terms of `<$>` and `<*>`.

---

This allows us to complete the parser for `Question`.

```haskell
  parseJSON (Object o) = Question
    <$> (o .: "title" :: Parser Text)
    <*> (o .: "answer-type" :: Parser AnswerType)
    <*> (o .: "questionnaire-id" :: Parser (Id Questionnaire))
```

---

After all this hard work, a bad news: Haskell could generate all these instances for us!

---

We can use [`Generic` programming](https://www.youtube.com/watch?v=pwnrfREbhWY) to derive `FromJSON` instances.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- base
import GHC.Generics

newtype Questionnaire = Questionnaire
  { title :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON
```

and similarly for other data types.

---

Similarly, we can derive instances also for encoding data types to `JSON`

```haskell
newtype Questionnaire = Questionnaire
  { title :: Text
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)
```

---

---

At this point, we can generate the [OpenApi](https://www.openapis.org/) documentation for our API.

---

We need to add a new executable in `package.yaml`

```yaml
  openapi:
    source-dirs:    openapi
    main:           Main.hs
    dependencies:
      - haskell-training
      - aeson-pretty
      - bytestring
      - servant-openapi3
```

---

Add some little code to `openapi/Main.hs`

```haskell
module Main where

import Api.Forms

-- aeson-pretty
import Data.Aeson.Encode.Pretty

-- base
import Data.Proxy

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL8

-- servant
import Servant.API

-- servant-openapi3
import Servant.OpenApi

main :: IO ()
main = do
  BL8.putStrLn . encodePretty $ toOpenApi (Proxy :: Proxy (NamedRoutes FormsApi))
```

---

Deriving a `Generic` instance for our API definition

```haskell
{-# LANGUAGE DeriveGeneric #-}

-- base
import GHC.Generics

data FormsApi mode = FormsApi
  { ...
  }
  deriving Generic
```

---

Add `openapi3` package to our dependencies

```yaml
dependencies:
  - openapi3
```

---

And deriving `ToSchema` instances for our data types.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

newtype Questionnaire = Questionnaire
  { title :: Text
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, ToSchema)
```

---

And a `ToParamSchema` instance for `Id`

```haskell
newtype Id a = Id UUID
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToParamSchema)
```

---

Now we can generate the actual documentation

```bash
stack exec openapi > openapi.json
```

---

---

Now we're left with implementing a server which exposes the endpoints we defined above.

---

We need to implement a server for out API data type

```haskell
formsServer :: FormsApi AsServer
formsServer = FormsApi
  { createNewQuestionnaire = _
  , questionnaires         = _
  , addNewQuestion         = _
  , questionnaireQuestions = _
  , recordAnswerSet        = _
  , answerSets             = _
  , setIdAnswers           = _
  , questionAnswers        = _
  }
```

---

We will use the [Repository pattern](https://www.martinfowler.com/eaaCatalog/repository.html), to mediate between the domain and the persistence layers using collection-like interfaces for accessing and manipulating domain values.

It will be our interface to access and interact with the domain. It also allows defining multiple implementations (e.g. one for production and one for testing)

---

Let's start by creating a repository for `Questionnaire`s

```haskell
module Domain.QuestionnaireRepository where

data QuestionnaireRepository = QuestionnaireRepository
  {
  }
```

---

We need to look at the endpoints to know which actions we actually need in the repository.

The first two endpoints are dealing with `Questionnaire`.

They have type

```haskell
createNewQuestionnaire :: Questionnaire -> Handler (Id Questionnaire)

questionnaires :: Handler [Identified Questionnaire]
```

where [`Handler`](https://hackage.haskell.org/package/servant-server-0.19.1/docs/Servant-Server.html#t:Handler) is the context used by `Servant` to write endpoint handlers.

---

Hence, we need two methods in our repository to add a new `Questionnaire` and to retrieve all `Questionnaire`s.

```haskell
data QuestionnaireRepository = QuestionnaireRepository
  { add :: _
  , all :: _
  }
```

---

To remain detached from implementation details and allow for multiple implementations (e.g. production and testing) we will abstract over the context where we will operate.

Therefore, we add the context as a type variable to our repository

```haskell
import Domain.Id
import Domain.Questionnaire

data QuestionnaireRepository m = QuestionnaireRepository
  { add :: Questionnaire -> m (Id Questionnaire)
  , all :: m [Identified Questionnaire]
  }
```

---

At this point it is easy to implement the two first endpoints

```haskell
import Domain.QuestionnaireRepository

-- servant-server
import Servant.Server

formsServer :: QuestionnaireRepository Handler -> FormsApi AsServer
formsServer (QuestionnaireRepository addQuestionnaire allQuestionnaires) = FormsApi
  { createNewQuestionnaire = addQuestionnaire
  , questionnaires         = allQuestionnaires
  , ...
  }

---

Similarly, we can create the other repositories for the other entities, following the lead of our endpoints

```haskell
module Domain.QuestionRepository where

import Domain.Id
import Domain.Question
import Domain.Questionnaire

data QuestionRepository m = QuestionRepository
  { add                 :: Question         -> m (Id Question)
  , allForQuestionnaire :: Id Questionnaire -> m [Identified Question]
  }
```

```haskell
module Domain.AnswerSetRepository where

import Domain.Answer
import Domain.Id
import Domain.Questionnaire

data AnswerSetRepository m = AnswerSetRepository
  { record              :: [Answer]         -> m (Id AnswerSet)
  , allForQuestionnaire :: Id Questionnaire -> m [Id AnswerSet]
  }
```

```haskell
module Domain.AnswerRepository where

import Domain.Answer
import Domain.Id
import Domain.Question

data AnswerRepository m = AnswerRepository
  { allForSet      :: Id AnswerSet -> m [Identified Answer]
  , allForQuestion :: Id Question  -> m [Identified Answer]
  }
```

---

And we can use them to implement our API

```haskell
formsServer
  :: QuestionnaireRepository Handler
  -> QuestionRepository Handler
  -> AnswerSetRepository Handler
  -> AnswerRepository Handler
  -> FormsApi AsServer
formsServer questionnaireRepository questionRepository answerSetRepository answerRepository = FormsApi
  { createNewQuestionnaire = Questionnaire.add             questionnaireRepository
  , questionnaires         = Questionnaire.all             questionnaireRepository
  , addNewQuestion         = Question.add                  questionRepository
  , questionnaireQuestions = Question.allForQuestionnaire  questionRepository
  , recordAnswerSet        = AnswerSet.record              answerSetRepository
  , answerSets             = AnswerSet.allForQuestionnaire answerSetRepository
  , setIdAnswers           = Answer.allForSet              answerRepository
  , questionAnswers        = Answer.allForQuestion         answerRepository
  }
```

---

---

If we want, we could group all our dependencies into a single type

```haskell
module Api.AppServices where

import Domain.AnswerRepository
import Domain.AnswerSetRepository
import Domain.QuestionnaireRepository
import Domain.QuestionRepository

-- servant-server
import Servant

data AppServices = AppServices
  { questionnaireRepository :: QuestionnaireRepository Handler
  , questionRepository      :: QuestionRepository Handler
  , answerSetRepository     :: AnswerSetRepository Handler
  , answerRepository        :: AnswerRepository Handler
  }
```

---

And use to simply our server definition a bit

```haskell
import Api.AppServices

formsServer
  :: AppServices
  -> FormsApi AsServer
formsServer (AppServices questionnaireRepository questionRepository answerSetRepository answerRepository) = FormsApi
  { createNewQuestionnaire = Questionnaire.add             questionnaireRepository
  , questionnaires         = Questionnaire.all             questionnaireRepository
  , addNewQuestion         = Question.add                  questionRepository
  , questionnaireQuestions = Question.allForQuestionnaire  questionRepository
  , recordAnswerSet        = AnswerSet.record              answerSetRepository
  , answerSets             = AnswerSet.allForQuestionnaire answerSetRepository
  , setIdAnswers           = Answer.allForSet              answerRepository
  , questionAnswers        = Answer.allForQuestion         answerRepository
  }
```
