---
type: slide
tags: tweag, training
slideOptions:
  progress: true
  controls: false
  slideNumber: false
---

<style>
  .reveal pre {width: 100%; max-height: 600px;}
  .reveal pre code {max-height: 600px;}
  code {color: #c7254e;}
</style>

# Haskell at Work - Web API

---

```bash
git checkout chapter2
```

---

In this second chapter we want to transform our terminal application into a more structured web API.

---

Let's start to think about the web API endpoints we would like to have.

---

Create a new `questionnaire`

|||
| --- | --- |
| **method** | `POST` |
| **request** | `questionnaire` `title` |
| **response** | `questionnaire` `id` |
|||

---

Retrieve all `questionnaire`s

|||
| --- | --- |
| **method** | `GET` |
| **request** ||
| **response** | list of `questionnaire`s with their `id`s |
|||

---

Add new `question` to a `questionnaire`

|||
| --- | --- |
| **method** | `POST` |
| **request** | `title`, `answerType`, `questionnaire` `id` |
| **response** | `question` `id` |
|||

---

Retrieve `question`s for `questionnaire` 

|||
| --- | --- |
| **method** | `GET` |
| **request** | `questionnaire` `id` |
| **response** | list of `question`s with their `id`s |
|||

---

Record `set` of `answer`s for a `questionnaire`

|||
| --- | --- |
| **method** | `POST` |
| **request** | list of `answer`s with `content` and `question` `id` |
| **response** | `id` for the `set` of `answer`s |
|||

---

Retrieve `set`s of `answer`s for a `questionnaire`

|||
| --- | --- |
| **method** | `GET` |
| **request** | `questionnaire` `id` |
| **response** | list of `answer` `set` `id`s |
|||

---

Retrieve all `answer`s for a given `set`

|||
| --- | --- |
| **method** | `GET` |
| **request** | `answer` `set` `id` |
| **response** | list of `answer`s with their `id`s and `answer` `set` `id` |
|||

---


Retrieve all `answer`s for a given `question`

|||
| --- | --- |
| **method** | `GET` |
| **request** | `question` `id` |
| **response** | list of `answer`s with their `id`s and `answer` `set` `id` |
|||

---

What we would like to do now is to encode this information in our code, so that we can use it to guide our implementation for a server providing these endpoints.

---

As a first step, we want to define some data types to describe the payloads of our requests and responses.

---

This will help us to work with a high level representation of the data we actually want to transmit on the network.

---

Let's start from a `Questionnaire`, which is characterized just by its title

```haskell
module Domain.Questionnaire where

data Questionnaire = Questionnaire
  { title :: Text
  }
```

note:
```
-- text
import Data.Text
```

---

Next we want to think about `Question`s.

---

To add a new `Question` to a `Questionnaire` we actually need its `title`, `answerType` and `Questionnaire` `id`

```haskell
module Domain.Question where

data Question = Question
  { title           :: Text
  , answerType      :: AnswerType
  , questionnaireId :: QuestionnaireId
  }
```

note:
```
-- text
import Data.Text
```

---

We can copy the `AnswerType` data type from our previous chapter

```haskell
data AnswerType
  = Paragraph
  | Number
```

---

For the `QuestionnaireId` we want to use a `UUID` using a newtype to distinguish it from other `id`s.

```haskell
newtype QuestionnaireId = QuestionnaireId UUID
```

note:
```haskell
-- uuid
import Data.UUID
```

---

We are using a [`newtype`](https://wiki.haskell.org/Newtype). It wraps a single data type to create a new one with the same runtime representation.

```haskell
newtype Age = Age Int
```

It allows us to enlarge our domain language without incurring in any runtime overhead.

---

We also need to import the `uuid` package

```yaml
dependencies:
  - uuid
```

---

The last relevant entity is `Answer`, which has two versions.

---

One with the `Id AnswerSet`, when we are returning it.

```haskell
module Domain.Answer where

data Answer = Answer
  { content    :: Content
  , questionId :: QuestionId
  , setId      :: AnswerSetId
  }
```

---

One without, when we are receiving it.

```haskell
data AnswerData = AnswerData
  { contentData    :: Content
  , questionIdData :: QuestionId
  }
```

---

The `Content` data type is just what we were calling `Answer` in our previous chapter

```haskell
data Content
  = Paragraph Text
  | Number Int
```

note:
```
-- text
import Data.Text
```

---

We can define `QuestionId` and `AnswerSetId` similarly to how we defined `QuestionnaireId`

```haskell
newtype QuestionId = QuestionId UUID

newtype AnswerSetId = AnsewrSetId UUID
```

note:
```
-- uuid
import Data.UUID
```

---

All these similarly defined `Id`s really call for a refactoring unifying them in a single type.

Still, we want to keep them distinct at the type level. This way, the type system will help us avoid any confusion among them.

---

We could use a [phantom type](https://wiki.haskell.org/Phantom_type), a type variable which is not referring to anything at the value level

```haskell
module Domain.Id where

newtype Id a = Id UUID
```

note:
```
-- uuid
import Data.UUID
```

---

Now `Id Questionnaire`, `Id Question` and `Id AnswerSet` are all containing a `UUID`, while being distinct at the type level.

---

We can now ditch `QuestionnaireId` and `QuestionId`.

---

We are using `AnswerSet` to tag our `Id` type, but actually we never defined such a data type.

Since we are using it only as type level tag, it doesn't need to have any real constructor.

```haskell
data AnswerSet
```

---

```bash
git checkout chapter2.1
```

---

Now that we completed defining our entities, we want to get back to the definition of the API.

We want to encode all the information regarding our API in a single Haskell data type!

---

This is work for the awesome [`Servant`](https://hackage.haskell.org/package/servant-0.19) library.

Servant is a Haskell library used to define web services API and serving them.

---

Let's start by importing the `servant` and `servant-server` libraries

```yaml
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

We can describe our entire API in a single data type

```haskell
data FormsApi mode = FormsApi
  { ...
  }
```

note:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Domain.Answer
import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- servant
import Servant.API
import Servant.API.Generic
```

---

Every field describes a single endpoint

```haskell
  { createNewQuestionnaire
      :: mode :- "create-questionnaire"
      :> ReqBody '[JSON] Questionnaire
      :> Post '[JSON] (Id Questionnaire)
```

---

```haskell
  , questionnaires
      :: mode :- "questionnaires"
      :> Get '[JSON] [(Id Questionnaire, Questionnaire)]
```

---

```haskell
  , addNewQuestion
      :: mode :- "add-question"
      :> ReqBody '[JSON] Question
      :> Post '[JSON] (Id Question)
```

---

```haskell
  , questionnaireQuestions
      :: mode :- "questions"
      :> Capture "questionnaire" (Id Questionnaire)
      :> Get '[JSON] [(Id Question, Question)]
```

---

```haskell
  , recordAnswerSet
      :: mode :- "record-answer-set"
      :> ReqBody '[JSON] [AnswerData]
      :> Post '[JSON] (Id AnswerSet)
```

---

```haskell
  , answerSets
      :: mode :- "answer-sets"
      :> Capture "questionnaire" (Id Questionnaire)
      :> Get  '[JSON] [Id AnswerSet]
```

---

```haskell
  , setIdAnswers
      :: mode :- "set-answers"
      :> Capture "set" (Id AnswerSet)
      :> Get  '[JSON] [(Id Answer, Answer)]
```

---

```haskell
  , questionAnswers
      :: mode :- "question-answers"
      :> Capture "question" (Id Question)
      :> Get  '[JSON] [(Id Answer, Answer)]
  }
```

---

We realize that we're using many times the construct `(Id a, a)`.

---

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

```bash
git checkout chapter2.2
```

---

Next, since we want to use `JSON` as our API language, we want to be able to encode to/decode from `JSON` all our domain entities.

---

To do this we will use the `aeson` library

```yaml
dependencies:
  - aeson
```

---

We'll take a look at the decoding phase.

---

The ability to decode a data type from JSON is contained in the [`FromJSON`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#t:FromJSON) typeclass

```haskell
class FromJSON a where
  parseJSON :: Value -> Parser a
```

---

`Value` is a data type representing a `JSON` value

```haskell
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
```

---

[`Parser a`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#t:Parser) is the result of the parsing operation, which, if successful, returns a value of type `a`.

---

Let's try to decode a `Questionnaire`, first

```haskell
instance FromJSON Questionnaire where
  parseJSON :: Value -> Parser Questionnaire
  parseJSON v = _
```

note:
```haskell
{-# LANGUAGE InstanceSigs #-}

-- aeson
import Data.Aeson.Types
```

---

The first step is case splitting on the input `Value`

```haskell
instance FromJSON Questionnaire where
  parseJSON :: Value -> Parser Questionnaire
  parseJSON (Object o) = _
  parseJSON (Array a)  = _
  parseJSON (String s) = _
  parseJSON (Number n) = _
  parseJSON (Bool b)   = _
  parseJSON Null       = _
```

---

We need to decide now what we want the JSON representation of a `Questionnaire` to be.

I'd say it makes sense to use an object with a `title` field.

---

Hence, all other options other than `Object` will need to fail.

---

The documentation of `FromJSON` describes the [`typeMismatch`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#v:typeMismatch) function exactly for this use case.

```haskell
instance FromJSON Questionnaire where
  parseJSON :: Value -> Parser Questionnaire
  parseJSON (Object o) = _
  parseJSON v          = typeMismatch "Object" v
```

---

We can parse an object field with the [`.:`](https://hackage.haskell.org/package/aeson-2.1.0.0/docs/Data-Aeson-Types.html#v:.:) operator

```haskell
  parseJSON (Object o) = _ (o .: "title" :: Parser Text)
```

We specify the type explicitly to avoid excessive polymorphism until we are not done with the implementation.

note:
```haskell
{-# LANGUAGE OverloadedStrings #-}
```

---

The hole now has type

```haskell
Parser Text -> Parser Questionnaire
```

We can look at this as a map from `Text` to `Questionnaire` inside the `Parser` context.

---

We actually have a map from `Text` to `Questionnaire`, which is the `Questionnaire` constructor.

```haskell
Questionnaire :: Text -> Questionnaire
```

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

It's worth mentioning that there is also an infix version of `fmap`, which is the `<$>` operator

```haskell
(<$>) :: (a -> b) -> f a -> f b

-- compare it to
($)   :: (a -> b) ->   a ->   b
```

---

`fmap`

![functor](https://i.imgur.com/DKZ4ng9.png)

---

We're in luck since the `aeson` library provides a `Functor` instance for the `Parser` data type.

---

Now we can complete our definition for the `FromJSON Questionnaire` instance

```haskell
  parseJSON (Object o) = Questionnaire <$> o .: "title"
```

---

Next we would like to create an instance for `FromJSON Question`

```haskell
instance FromJSON Question where
  parseJSON :: Value -> Parser Question
  parseJSON v = _
```

note:
```haskell
{-# LANGUAGE InstanceSigs #-}

-- aeson
import Data.Aeson.Types
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

note:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Domain.Id
import Domain.Questionnaire
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
  parseJSON (String s) = _
  parseJSON v          = typeMismatch "String" v
```

---

If the string equals `Number` or `Paragraph`, we return the appropriate value, otherwise we fail

```haskell
  parseJSON (String s) = case s of
    "Paragraph" -> pure Paragraph
    "Number"    -> pure Number
    _           -> fail "allowed values are Paragraph and Number"
```

Similarly to what we did for `IO`, we need to use the `pure` function to lift a value into the `Parser` context.

---

The instance for `Id Questionnaire` is actually even simpler, since we can use the already present instance for `UUID`

```haskell
instance FromJSON (Id a) where
  parseJSON :: Value -> Parser (Id a)
  parseJSON v = Id <$> parseJSON v
```

note:
```haskell
{-# LANGUAGE InstanceSigs #-}

-- aeson
import Data.Aeson.Types
```

---

Let's go back to our `FromJSON Question` instance

```haskell
  parseJSON (Object o) = _
    (o .: "title" :: Parser Text)
    (o .: "answer-type" :: Parser AnswerType)
    (o .: "questionnaire-id" :: Parser (Id Questionnaire))
```

The hole has type
```haskell
   Parser Text
-> Parser AnswerType
-> Parser (Id Questionnaire)
-> Parser Question
```

---

Similarly to what we did for `Questionnaire`, we can see this as a function 

```haskell
   Text
-> AnswerType
-> Id Questionnaire
-> Question
```

inside the `Parser` context.

---

We actually already have a 

```haskell
   Text
-> AnswerType
-> Id Questionnaire
-> Question
```

function, which is the `Question` constructor.

What we would like to do is to lift it to the `Parser` context.

---

Previously we used a `Functor`. Does it work now?

Unluckily, it falls short

```haskell
f :: a -> b -> c -> d
fa :: f a
f <$> fa :: f (b -> c -> d)

-- we wanted :: f b -> f c -> f d
```

---

To be able to lift functions of higher [arity](https://en.wikipedia.org/wiki/Arity) (i.e. with more arguments) we need something more powerful than `Functor`.

---

We need `Applicative`

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Aha, here is where the `pure` lifting operation we were using comes from!

---

`pure`

![](https://i.imgur.com/Qdh6E8t.png)

---

`<*>`

![applicative apply](https://i.imgur.com/xNJdUP4.png)

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

---

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

To sum up, we can use `<$>` and `<*>` to lift a function of any arity into any `Applicative` context.

---

Exercise: try to write a

```haskell
lift3 :: Applicative f
      => ( a ->   b ->   c ->   d)
      -> f a -> f b -> f c -> f d
```

function in terms of `<$>` and `<*>`.

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
newtype Questionnaire = Questionnaire
  { title :: Text
  }
  deriving (Generic, FromJSON)
```

and similarly for other data types.

note:
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- base
import GHC.Generics
```

---

Similarly, we can derive instances also for encoding data types to `JSON`

```haskell
newtype Questionnaire = Questionnaire
  { title :: Text
  }
  deriving (Generic, FromJSON, ToJSON)
```

---

```bash
git checkout chapter2.3
```

---

Now we're left with implementing a server which exposes the endpoints we defined above.

---

We need to implement a server for our API data type

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

note:
```haskell
-- servant-server
import Servant.Server.Generic
```

---

We will use the [Repository pattern](https://www.martinfowler.com/eaaCatalog/repository.html), to mediate between the domain and the persistence layers using collection-like interfaces for accessing and manipulating domain values.

---

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

---

They have type

```haskell
createNewQuestionnaire
  :: Questionnaire
  -> Handler (Id Questionnaire)

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

---

Therefore, we add the context as a type variable to our repository

```haskell
data QuestionnaireRepository m = QuestionnaireRepository
  { add :: Questionnaire -> m (Id Questionnaire)
  , all :: m [Identified Questionnaire]
  }
```

note:
```haskell
import Domain.Id
import Domain.Questionnaire
```

---

This is an abstraction which is quite unique to statically typed functional programming languages, using `higher kinded types`. It is not achievable just by using generics.

---

At this point it is easy to implement the two first endpoints

```haskell
formsServer :: QuestionnaireRepository Handler -> FormsApi AsServer
formsServer questionnaireRepository = FormsApi
  { createNewQuestionnaire = add questionnaireRepository
  , questionnaires         = all questionnaireRepository
  , ...
  }
```

note:
```haskell
import Domain.QuestionnaireRepository

-- servant-server
import Servant.Server
```

---

Similarly, we can create the other repositories for the other entities, following the lead of our endpoints

```haskell
module Domain.QuestionRepository where

data QuestionRepository m = QuestionRepository
  { add                 :: Question
                        -> m (Id Question)
  , allForQuestionnaire :: Id Questionnaire
                        -> m [Identified Question]
  }
```

note:
```haskell
import Domain.Id
import Domain.Question
import Domain.Questionnaire
```

---

```haskell
module Domain.AnswerSetRepository where

data AnswerSetRepository m = AnswerSetRepository
  { record              :: [AnswerData]     -> m (Id AnswerSet)
  , allForQuestionnaire :: Id Questionnaire -> m [Id AnswerSet]
  }
```

note:
```haskell
import Domain.Answer
import Domain.Id
import Domain.Questionnaire
```

---

```haskell
module Domain.AnswerRepository where

data AnswerRepository m = AnswerRepository
  { allForSet      :: Id AnswerSet -> m [Identified Answer]
  , allForQuestion :: Id Question  -> m [Identified Answer]
  }
```

note:
```haskell
import Domain.Answer
import Domain.Id
import Domain.Question
```

---

And we can use them to implement our API

```haskell
formsServer
  questionnaireRepository
  questionRepository
  answerSetRepository
  answerRepository
  = FormsApi
    { createNewQuestionnaire
        = Questionnaire.add questionnaireRepository
    , questionnaires
        = Questionnaire.all questionnaireRepository
    , addNewQuestion
        = Question.add questionRepository
    , questionnaireQuestions
        = Question.allForQuestionnaire questionRepository
    , ...
    }
```

note:
```haskell
import Domain.AnswerRepository as Answer
import Domain.AnswerSetRepository as AnswerSet
import Domain.QuestionRepository as Question
import Domain.QuestionnaireRepository as Questionnaire

formsServer
  :: QuestionnaireRepository Handler
  -> QuestionRepository Handler
  -> AnswerSetRepository Handler
  -> AnswerRepository Handler
  -> FormsApi AsServer
```

---

```bash
git checkout chapter2.4
```

---

If we want, we could group all our dependencies into a single type

```haskell
module Api.AppServices where

data AppServices = AppServices
  { questionnaireRepository :: QuestionnaireRepository Handler
  , questionRepository      :: QuestionRepository Handler
  , answerSetRepository     :: AnswerSetRepository Handler
  , answerRepository        :: AnswerRepository Handler
  }
```

note:
```haskell
import Domain.AnswerRepository
import Domain.AnswerSetRepository
import Domain.QuestionnaireRepository
import Domain.QuestionRepository

-- servant-server
import Servant
```

---

And use it to simplify our server definition a bit

```haskell
formsServer :: AppServices -> FormsApi AsServer
formsServer (AppServices
  questionnaireRepository
  questionRepository
  answerSetRepository
  answerRepository)
  = FormsApi
    { createNewQuestionnaire
        = Questionnaire.add questionnaireRepository
    , questionnaires
        = Questionnaire.all questionnaireRepository
    , addNewQuestion
        = Question.add questionRepository
    , questionnaireQuestions
        = Question.allForQuestionnaire questionRepository
    , ...
    }
```

note:
```haskell
import Api.AppServices
```

---

## Appendix - Generate OpenAPI documentation

---

```bash
git checkout chapter2.5
```

---

Having the API described at the type level allows us to generate the [OpenApi](https://www.openapis.org/) documentation for our API.

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

main :: IO ()
main = do
  BL8.putStrLn . encodePretty $
    toOpenApi (Proxy :: Proxy (NamedRoutes FormsApi))
```

note:
```haskell
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
```

---

Deriving a `Generic` instance for our API definition

```haskell
data FormsApi mode = FormsApi
  { ...
  }
  deriving Generic
```

note:
```
{-# LANGUAGE DeriveGeneric #-}

-- base
import GHC.Generics
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
newtype Questionnaire = Questionnaire
  { title :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
```

note:
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi
```

---

And a `ToParamSchema` instance for `Id`

```haskell
newtype Id a = Id UUID
  deriving (Generic, FromJSON, ToJSON, ToSchema, ToParamSchema)
```

---

Now we can generate the actual documentation

```bash
stack exec openapi > openapi.json
```
