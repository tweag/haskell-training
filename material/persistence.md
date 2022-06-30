# Forms - Persistence

---

The next step we want to tackle is persisting our data.

---

In the Haskell ecosystem there are several libraries one could choose to interact with the database.

They differ a lot in the degree of abstraction they add on top of the database.

The simplest is [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)

---

We are going to use [rel8](https://hackage.haskell.org/package/rel8), a library which focuses on:

- conciseness
- inferrability
- familiarity

---

First off, We need to think about our database schema

---

```mermaid
erDiagram
  QUESTIONNAIRE ||--o{ QUESTION : contains
  QUESTION ||--o{ ANSWER : "is answered by"
  QUESTIONNAIRE {
    id   uuid
    title text
  }
  QUESTION {
    id               uuid
    questionnaire_id uuid
    title            text
    qtype            QuestionType
  }
  ANSWER {
    id          uuid
    question_id uuid
    set_id      uuid
    content     Answer
  }
```

---

Let's start working on a new file, say `Infrastructure/Persistnce.hs`

```haskell
module Infrastructure.Persistence where
```

---

We need to define the data types necessary to describe the database schema to out application

---

Let's start with questionnaires

```haskell
-- rel8
import Rel8

-- text
import Data.Text

-- uuid
import Data.UUID

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f UUID
  , questionnaireTitle :: Column f Text
  }
```

---

What is that `f`?

It is a unary type constructor with kind `f :: Type -> Type`, like `[]` and `IO`

It describes the wrapper/context in which the data need to be considered

---

This approach is usually called [Higher-kinded data](https://reasonablypolymorphic.com/blog/higher-kinded-data/).

It is useful to distinguish the shape of data from the context in which they are considered

---

To please `Rel8`, we need to add some deriving clauses

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- base
import GHC.Generics

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f UUID
  , questionnaireTitle :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able
```

---

We're explicitly telling the compiler which deriving strategy to use.

---

Next we need to define the schema for the questionnaire table

```haskell
{-# LANGUAGE OverloadedStrings #-}

questionnaireSchema :: TableSchema (Questionnaire Name)
```

---

A `TableSchema (Questionnaire Name)` requires us to provide a `name`, a `schema`, and a name of a column for every field of a `Questionnaire`

```haskell
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
type Name :: Type -> Type
newtype Name a = Name String
```

It is the constant function which always return a wrapped `String`.

It is useful for documentation.

---

This allows us to connect the Haskell representation of our data with the database representation, while keeping the two completely separate.

---

Now you could try to do the same thing for `Question` and `Answer`, following the schema presented above.

---

```haskell
data Question f = Question
  { questionId              :: Column f UUID
  , questionQuestionnaireId :: Column f UUID
  , questionTitle           :: Column f Text
  , questionType            :: Column f Domain.QuestionType
  }
  deriving stock Generic
  deriving anyclass Rel8able

questionSchema :: TableSchema (Question Name)
questionSchema = TableSchema
  { name    = "question"
  , schema  = Nothing
  , columns = Question
    { questionId              = "id"
    , questionQuestionnaireId = "questionnaire_id"
    , questionTitle           = "title"
    , questionType            = "qtype"
    }
  }

data Answer f = Answer
  { answerId         :: Column f UUID
  , answerQuestionId :: Column f UUID
  , answerSetId      :: Column f UUID
  , answerContent    :: Column f Domain.Answer
  }
  deriving stock Generic
  deriving anyclass Rel8able

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

Which interactions would we need?

- create a new questionnaire
- add a new question to a questionnaire
- get all questionnaires
- get all the questions for a questionnaire
- add all the answers of a questionnaire
- get all the answers for a questionnaire, grouped by
  - question
  - answer_set
- get all the answers for an answer_set
- get all the answers for a question

---

`groupBy`

```haskell
groupBy :: forall exprs aggregates. (EqTable exprs, Aggregates aggregates exprs) => exprs -> aggregates

Aggregates aggregates exprs => Transposes Aggregate Expr aggregates exprs

groupBy :: forall exprs aggregates. (EqTable exprs, Transposes Aggregate Expr aggregates exprs) => exprs -> aggregates

Transposes Aggregate Expr aggregates exprs => (Table Aggregate aggregates, Table Expr exprs, Congruent aggregates exprs, exprs ~ Transpose Expr aggregates, aggregates ~ Transpose Aggregate exprs)

groupBy :: forall exprs aggregates. (EqTable exprs, Table Aggregate aggregates, Table Expr exprs, Congruent aggregates exprs, exprs ~ Transpose Expr aggregates, aggregates ~ Transpose Aggregate exprs) => exprs -> aggregates

Congruent a b => Columns a ~ Columns b

groupBy :: forall exprs aggregates. (EqTable exprs, Table Aggregate aggregates, Table Expr exprs, Columns exprs ~ Columns aggregate, exprs ~ Transpose Expr aggregates, aggregates ~ Transpose Aggregate exprs) => exprs -> aggregates
```

does it work with `exprs ~ Expr a` and `aggregates ~ Aggregate a`

```
EqTable (Expr a) -- yes if `Sql DBEq a`

Table Aggregate (Aggregate a) -- yes if `Sql DBType a`

Table Expr (Expr a) -- yes if `Sql DBType a`

Columns (Expr a) ~ Columns (Aggregate a) -- yes

Expr a ~ Transpose Expr (Aggregate a) -- yes

Aggregate a ~ Transpose Aggregate (Expr a) -- yes
```

so we can consider

```haskell
groupBy :: (Sql DBEq a, SQL DBType a) => Expr a -> Aggregate a
́́```
