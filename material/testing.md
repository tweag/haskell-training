# Forms - testing

---

There is a big elephant in the room for the code we wrote up to now.

We wrote the code, it compiles, but we are not checking that it behaves how we want.

Types are awesome and can encode a lot of information, but they are often not enough to guarantee the correct behavior of our application

---

The first level of testing is just manual testing.

Remember that you can use the REPL to test the code you wrote

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

To write tests we use the quite common `describe` and `it` syntax

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

There are several strategies to find good properties for our code. See for example:
- https://medium.com/@nicolasdubien/find-the-best-properties-for-property-based-testing-ee2ed9d442e1
- https://fsharpforfunandprofit.com/posts/property-based-testing-2/

---

For example, we would like the perimeter of a rectangle to remain constant if we swap the sides

```haskell
      it "does not change for rectangles with swapped sides" $ do
        perimeter (Rectangle s1 s2) `shouldBe` perimeter (Rectangle s2 s1)
```

---

Does not work because `s1` and `s2` are not in scope

Let's introduce them with an anonymous function

```haskell
      it "does not change for rectangles with swapped sides" $ do
        \(s1, s2) -> perimeter (Rectangle s1 s2) `shouldBe` perimeter (Rectangle s2 s1)
```

---

We're still not saying how these `s1` and `s2` should be generated.

That is where `QuickCheck` is coming into play.

```haskell
-- QuickCheck
import Test.QuickCheck
```

---

We can use the [`forAll`](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:forAll) function, which requires a [generator](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#t:Gen).

Since we want to test our property for every possible input, we would like it to be [`arbitrary`](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:arbitrary).

```haskell
      it "does not change for rectangles with swapped sides" $ do
        forAll arbitrary $
          \(s1, s2) -> perimeter (Rectangle s1 s2) `shouldBe` perimeter (Rectangle s2 s1)
```

---

We get

```
+++ OK, passed 100 tests.
```

This means the test suite tried for 100 times

---
