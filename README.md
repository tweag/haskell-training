# haskell-training

Material for a Haskell training course

## Haskell at work

The course proposes a practical approach to Haskell. The course develops around the creation and evolution of a concrete project. It will contain some theory and a lot of practice. Certain parts of the project will be pre-developed, but the student will have to interact with what's provided and expand upon it.

Some more advanced concepts will be introduced (e.g. type-level programming with Servant) but will not be examined in depth.

Everything will be developed with the good practices of software engineering in mind.

### Principles

- create a practical application which resembles what people could do at work;
- try not to over-abstract before it is needed;
- introduce concepts only when they solve an issue (see https://mkremins.github.io/blog/doors-headaches-intellectual-need/);
- introduce concepts one by one;

### Objectives

- give the participants the feeling of how it is to work with Haskell
- teach them the basics of Haskell development and how to solve practical problems
- teach correct and precise data modelling using types
- show them the best features of Haskell
- provide directions on how to develop a real-world project
- discuss shortcomings/limitations of the language

### Target

- professional developers
- already experienced with some other programming language
- possibly willing to use Haskell or FP concepts at work
- interested in why Haskell is a good language for the industry
- interested also in the practicalities

### Buyers

- single developers who want to get a practical approach to Haskell
- companies interested in starting using Haskell
- companies using Haskell wanting to train their junior developers

### Prerequisites

- a bit of software engineering experience
  - knowledge of what is required for a production system
- already felt the pains of mutation and side effects management
- some experience in other languages with basic FP concepts such as immutability and higher-order functions
- basic Haskell syntax

### Project

The course will unroll developing a simplified clone of a questionnaire app like Google Forms or Typeform.

It adapts well to the principles we stated above since:

- it is extremely practical and concrete
- allows to be modelled in simple way which still highlights the usage of algebraic data types
- requires user interaction
- requires persistence
- requires definition of a web API

### Outline

- [Domain definition and basic terminal interaction](https://hackmd.io/CiiOhZqPS8CmFJYxrJZSfg)
- Adding persistence
  - Postgresql
  - Rel8
- Exposing a web API with Servant
  - Servant
  - wai
  - warp
- Testing
  - property based testing
  - doctest
- Bonus chapter: tooling
  - static analysis
  - CI
- Bonus chapter: advanced type safety (to solve a concrete modelling problem)
  - GADTs
  - RankNTypes
  - Existential types
  - ...

### Length

The basic version of the course, without the bonus chapters, should take around two days, split in four chapters of ~ 4 hours each.

It could be enlarged to 3 days (6 chapters of ~ 4 hours each) adding the two bonus chapters
