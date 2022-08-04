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

What we would like to do now is to encode this information in our code, so that we can use to guide our implementation for a server providing these endpoints.

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
--uuid
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

Still, we want to keep them distinct at the type level.

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
data AnswerSet = AnswerSet
```

And now we can use `Id AnswerSet`

---











---

Now we want to encode all the information regarding our API in a single Haskell data type!

This is work for the awesome [`Servant`](https://hackage.haskell.org/package/servant-0.19) library.

Servant is a Haskell library to define web services API and serving them.

Warning: Servant internals are complicated! We will not dive into the details.

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

Now it's time for the first Servant technicality. To make the machinery work, the `FormsApi` data type needs a `mode` type variable which is then used on every route

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
  }
```

---

Let's try to unpack this definition:

- `"create-questionnaire"` is the path
- `ReqBody '[JSON] Questionnaire` is saying that the request should contain a `Questionnaire` formatted as `JSON`
- `Post '[JSON] (Id Questionnaire)` is saying that it is a `Post` request which will return a `Id Questionnaire` formatted as `JSON`
