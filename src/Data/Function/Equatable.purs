-- | It would sometimes be useful to be able to compare two functions in order to
-- | determine whether they are equal. This is famously impossible, in the general
-- | case, in the sense that there is no general algorithm to determine whether one
-- | function is equivalent to another function.
-- |
-- | Now, when faced with the desire to test whether two functions are equal, the
-- | right answer is often: don't do that! Instead, refactor your code so that
-- | you the things you are comparing are just data.
-- |
-- | However, there are some patterns in which you treat a function as part of a
-- | larger data type, and in those cases you may want to be able to determine
-- | whether two values of that type are equal. Hence, you want to determine whether
-- | two functions are equal.
-- |
-- | So, how can we accommodate that, to some degree? This module provides a kind
-- | of "wrapper" type -- `EqFunc` -- which can be tested for equality. The typical
-- | life-cycle is:
-- |
-- | * Construct an `EqFunc` from a regular function.
-- | * Possibly manipulate the `EqFunc` or combine it with other `EqFunc`s.
-- | * Test the `EqFunc` for equality with another `EqFunc`.
-- | * Use `runEF` (and friends) to turn it back into a function.
-- |
-- | The manipulation of an `EqFunc` includes things such as composition, partial
-- | application, currying, uncurrying, and flipping -- all preserving the ability
-- | to check the results for equality with another `EqFunc`.
-- |
-- | ## Simple Functions
-- |
-- | Let's start with a simple function. Consider a function which adds two to an
-- | integer.
-- |
-- |     add2 :: Int -> Int
-- |     add2 x = x + 2
-- |
-- | We can turn this into an `EqFunc` using `eqFunc`, like this:
-- |
-- |     add2func :: EqFunc Int Int
-- |     add2func = eqFunc add2
-- |
-- | Now, we define `==>` as an infix type operator for `EqFunc`.  So, we can write
-- | `add2Func` like this instead, making it look a little like more like a function
-- | type. Isn't that nice?
-- |
-- |     nicerAdd2func :: Int ==> Int
-- |     nicerAdd2func = eqFunc add2
-- |
-- | `EqFunc` remembers that both `add2func` and `nicerAdd2func` were made from the
-- | very same function. So, it knows that they are equal.
-- |
-- |     add2func == nicerAdd2func    -- true
-- |
-- | But, this isn't magic ... if you separately define the original function,
-- | `EqFunc` won't know that they are equal, even if they are equivalent.
-- |
-- |     eqFunc (\x -> x + 2) == eqFunc (\x -> x + 2)    -- false
-- |
-- | So, the moral of the story is that for `EqFunc` to work as expected, you need
-- | to initially supply it with functions defined at the top-level, not functions
-- | separately defined each time. If this strikes you as a significant limitation,
-- | read ahead to "Partial Application" to see a workaround.
-- |
-- | But first, what about actually running the function? You can use `runEF` to
-- | extract the original function.
-- |
-- |     (runEF add2func) 5 == 7
-- |
-- | Or, we've defined `~` as an infix operator for function application.  So, you
-- | can do something like this:
-- |
-- |     add2func ~ 5 == 7
-- |
-- | So, that is how `EqFunc` deals with simple functions. But that's not all it can
-- | do!
-- |
-- | ## Partial Application
-- |
-- | What about functions with two parameters? Now, strictly speaking, there are no
-- | functions with two parameters. There are only functions with one parameter which,
-- | when given that parameter, return another function. But suppose you provide such
-- | a function to `eqFunc`. What will happen?
-- |
-- | Consider an `EqFunc` made from `+` itself, whith is `Int -> Int -> Int`
-- |
-- |     add :: Int ==> (Int -> Int)
-- |     add = eqFunc (+)
-- |
-- | Now, of course, basic `EqFunc` equality will work here, as expected.
-- |
-- |     eqFunc (+) == eqFunc (+)    -- true
-- |
-- | But what if we partially apply the parameters, to return a function?  Since we
-- | literally return a function, equality won't even compile.
-- |
-- |     add ~ 7 == add ~ 7   -- doesn't even compile
-- |
-- | So, what we want is something where partial application actually returns an
-- | `EqFunc`. For 2 parameters, we can get this with `eqFunc2`
-- |
-- |     nicerAdd :: Int ==> Int ==> Int
-- |     nicerAdd = eqFunc2 (+)
-- |
-- | And now, if we partially apply, the resulting `EqFunc`s can be compared
-- |
-- |     nicerAdd ~ 2 == nicerAdd ~ 2           -- true
-- |     nicerAdd ~ 2 == nicerAdd ~ 3           -- false
-- |
-- |     (eqFunc2 (+)) ~ 2 == (eqFunc (+)) ~ 2  -- true
-- |     (eqFunc2 (+)) ~ 2 == (eqFunc (+)) ~ 3  -- false
-- |
-- | To fully apply the two paramter `EqFunc` we can just keep using `~`.
-- |
-- |     nicerAdd ~ 2 ~ 4 == 6
-- |
-- | Or, we can use `runEF2` to extract a two parameter function.
-- |
-- |     (runEF2 nicerAdd) 2 4 == 6
-- |
-- | And, as you might have guessed, there is an `eqFunc3` through `eqFunc10`,
-- | as well as a `runEF3` to `runEF10`, that follow the same pattern, with
-- | the specified number of parameters.
-- |
-- | I mentioned earlier that partial application provides a workaround for
-- | the fact that `EqFunc` needs to start with functions defined at the
-- | top-level. The main reason you want to define functions inline is to
-- | capture values in a closure. For instance, you might use `\y -> x + y`
-- | somewhere, where `x` represents some computed value that is in scope.
-- | Making an `EqFunc` out of this won't work well, because the function
-- | gets generated separately each time, depending on what `x` is.
-- |
-- |     x = 5    -- imagine a more complex computation!
-- |
-- |     eqFunc (\y -> x + y) ==
-- |     eqFunc (\y -> x + y)       -- sadly, false
-- |
-- | However, you can get the same result via partial application -- e.g. `(+) x`.
-- | And you can represent that as an `EqFunc` which starts at the top-level.  Two
-- | `EqFunc`s defined in this way will be equal so long as `x` is equal.
-- |
-- |     x = 5    -- again, imagine a more complex computation
-- |
-- |     (eqFunc2 (+)) ~ x == (eqFunc2 (+)) ~ x   -- true
-- |     (eqFunc2 (+)) ~ x == (eqFunc2 (+)) ~ 5   -- true
-- |     (eqFunc2 (+)) ~ x == (eqFunc2 (+)) ~ 7   -- false
-- |
-- | So, the pattern you want to follow is to "lift" top-level functions
-- | into `EqFunc`, and then do your function manipulation at the `EqFunc`
-- | level. And partial application isn't the only manipulation you can do!
-- |
-- | ## Composition
-- |
-- | You will probably guess by now that `EqFunc` doesn't like functions
-- | that are already the product of composition, because it has no way
-- | of knowing that they are equal.
-- |
-- |     add3 :: Int -> Int
-- |     add3 = (+) 3
-- |
-- |     add6 :: Int -> Int
-- |     add6 = (+) 6
-- |
-- |     eqFunc (add3 >>> add6) ==
-- |     eqFunc (add3 >>> add6)                -- sadly, false
-- |
-- | However, `EqFunc` has a `Semigroupoid` instance, just like `Function` does.
-- | So, you can just construct the `EqFunc`s first, and then compose them:
-- |
-- |     (eqFunc add3) >>> (eqFunc add6) ==
-- |     (eqFunc add3) >>> (eqFunc add6)       -- true
-- |
-- | And `EqFunc` obeys the `Semigroupid` laws, so you can compose multiple times,
-- | and the order doesn't matter, etc. There is also a `Category` instance, so you
-- | can use `id` as well.
-- |
-- | But that's not all!
-- |
-- | ## `const`, `flip`, `curry` and `uncurry`
-- |
-- | In principle, I'd like to have `EqFunc` versions of any useful function that
-- | takes a function as a parameter and returns a function. So far, I've done
-- | `constEF`, `flipEF`, `curryEF` and `uncurryEF`.
-- |
-- | For `constEF`, the resulting functions are equal if the const value is equal.
-- |
-- |     constEF 5 == constEF 5    -- true
-- |     constEF 5 == constEF 8    -- false
-- |
-- | For `flipEF`, flipping equal `EqFunc`s preserves equality:
-- |
-- |     flipEF (eqFunc2 (-)) == flipEF (eqFunc2 (-))    -- true
-- |
-- | And, if you flip twice, it's equal to the original:
-- |
-- |     flipEF (flipEF (eqFunc2 (-))) == eqFunc2 (-)    -- true
-- |
-- | The same principle applies to `curryEF`, `uncurryEF`, and their friends
-- | with a higher parameter count.
-- |
-- |     add = eqFunc2 (+)
-- |
-- |     curryEF add == curryEF add       -- true
-- |     uncurryEF (curryEF add) == add   -- true
-- |
-- | If I'm missing other interesting functions that return functions, I'd
-- | be happy to add them!
-- |
-- | ## And More!
-- |
-- | There are also `EqFunc` instances for `Profunctor`, `Choice` and `Strong`.
-- | To be honest, I'm not entirely sure how one would use them, but they sound
-- | interesting.


module Data.Function.Equatable
    ( EqFunc, type (==>)
    , eqFunc, eqFunc2, eqFunc3, eqFunc4, eqFunc5, eqFunc6, eqFunc7, eqFunc8, eqFunc9, eqFunc10
    , runEF, (=$=), (~)
    , runEF2, runEF3, runEF4, runEF5, runEF6, runEF7, runEF8, runEF9, runEF10
    , runFlippedEF, (=#=)
    , constEF, flipEF
    , curryEF, curryEF3, curryEF4, curryEF5, curryEF6, curryEF7, curryEF8, curryEF9, curryEF10
    , uncurryEF, uncurryEF3, uncurryEF4, uncurryEF5, uncurryEF6, uncurryEF7, uncurryEF8, uncurryEF9, uncurryEF10
    ) where


import Data.List (List(..), (:), snoc)
import Data.Tuple (Tuple, curry, uncurry)
import Data.Exists.Eq (SomeEq, someEq)
import Data.Monoid (class Monoid)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice, left, right)
import Data.Profunctor.Strong (class Strong, first, second)

import Data.Tuple.Nested
    ( Tuple3, curry3, uncurry3
    , Tuple4, curry4, uncurry4
    , Tuple5, curry5, uncurry5
    , Tuple6, curry6, uncurry6
    , Tuple7, curry7, uncurry7
    , Tuple8, curry8, uncurry8
    , Tuple9, curry9, uncurry9
    , Tuple10, curry10, uncurry10
    )

import Prelude
    ( class Eq, eq, (==)
    , class Semigroup
    , class Semigroupoid, compose, (<<<), (>>>)
    , class Category, id
    , class Functor, map
    , class Show, show
    , (<>), flip, (#), ($), const, (&&), (+), negate
    )


-- | A wrapper for a function which remembers things about how
-- | the function was generated, allowing a test for equality.
-- |
-- | * Use `eqFunc` through `eqFunc10` to create an `EqFunc`
-- |   from an arbitrary top-level function.
-- |
-- | * Use partial application (via `~` or `runEF`), composition,
-- |   `flipEF`, `curryEF`, etc. to manipulate an `EqFunc`.
-- |
-- | * Use the `Eq` instance to compare two `EqFunc`s.
-- |
-- | * Use `~` or `runEF` to apply an argument to the `EqFunc`.
newtype EqFunc a b = EqFunc
    { func :: a -> b
    , tag :: Tag
    }


-- | An infix operator for the `EqFunc` type, so you can do
-- | things like this:
-- |
-- |     add :: Int ==> Int ==> Int
-- |     add = eqFunc2 (+)
infixr 4 type EqFunc as ==>


instance showEqFunc :: Show (EqFunc a b) where
    show (EqFunc {tag}) = "(EqFunc {tag: " <> show tag <> "})"


instance functorEqFunc :: Functor (EqFunc a) where
    map func1 func2 = compose (eqFunc func1) func2


instance profunctorEqFunc :: Profunctor EqFunc where
    dimap a2b c2d b2c = (eqFunc a2b) >>> b2c >>> (eqFunc c2d)


instance choiceEqFunc :: Choice EqFunc where
    left (EqFunc {func, tag}) =
        mkEqFunc
            (ChoseLeft tag)
            (left func)

    right (EqFunc {func, tag}) =
        mkEqFunc
            (ChoseRight tag)
            (right func)


instance strongEqFunc :: Strong EqFunc where
    first (EqFunc {func, tag}) =
        mkEqFunc
            (StrongFirst tag)
            (first func)

    second (EqFunc {func, tag}) =
        mkEqFunc
            (StrongSecond tag)
            (second func)


-- | Like `flip` for plain old functions, but equatable.
-- |
-- |      subtract :: Int ==> Int ==> Int
-- |      subtract = eqFunc2 (-)
-- |
-- |      flipEF subtract == flipEF subtract
-- |      flipEF (flipEF subtract)) == subtract
-- |
-- |      (flipEF subtract) ~ 2 ~ 7 == 5
-- |      (flipEF (flipEF subtract)) ~ 2 ~ 7 == (-5)
flipEF :: ∀ a b c. (Eq b) => (a ==> b ==> c) -> (b ==> a ==> c)
flipEF (EqFunc {tag, func}) =
    mkEqFunc2
        case tag of
            Flipped parent -> parent
            _ -> Flipped tag

        \b a ->
            (func a) ~ b


-- | Like `const` for plain old functions, but equatable.
-- |
-- |      constEF 5 == constEF 5
-- |      constEF 5 /= constEF 6
-- |
-- |      (constEF 5) ~ 7 == 5
constEF :: ∀ a b. (Eq a) => a -> (b ==> a)
constEF = runEF (eqFunc2 const)


curried :: Int -> Tag -> Tag
curried times (Curried level parent) | level + times == 0 = parent
curried times (Curried level parent) = Curried (level + times) parent
curried times parent = Curried times parent


-- | Like `curry` for plain old functions, but equatable.
-- |
-- |     add :: Tuple Int Int -> Int
-- |     add = uncurry (+)
-- |
-- |     curryEF (eqFunc add) == curryEF (eqFunc add)
-- |     uncurryEF (curryEF (eqFunc add)) == eqFunc add
-- |
-- |     (curryEF (eqFunc add)) ~ 2 ~ 3 == 5
curryEF :: ∀ a b c. (Eq a) => (Tuple a b ==> c) -> (a ==> b ==> c)
curryEF (EqFunc {tag, func}) =
    mkEqFunc2
        (curried 1 tag)
        (curry func)


curryEF3 :: ∀ a b c d. (Eq a, Eq b) => (Tuple3 a b c ==> d) -> (a ==> b ==> c ==> d)
curryEF3 (EqFunc {tag, func}) =
    mkEqFunc3
        (curried 2 tag)
        (curry3 func)


curryEF4 :: ∀ a b c d e. (Eq a, Eq b, Eq c) => (Tuple4 a b c d ==> e) -> (a ==> b ==> c ==> d ==> e)
curryEF4 (EqFunc {tag, func}) =
    mkEqFunc4
        (curried 3 tag)
        (curry4 func)


curryEF5 :: ∀ a b c d e f. (Eq a, Eq b, Eq c, Eq d) => (Tuple5 a b c d e ==> f) -> (a ==> b ==> c ==> d ==> e ==> f)
curryEF5 (EqFunc {tag, func}) =
    mkEqFunc5
        (curried 4 tag)
        (curry5 func)


curryEF6 :: ∀ a b c d e f g. (Eq a, Eq b, Eq c, Eq d, Eq e) => (Tuple6 a b c d e f ==> g) -> (a ==> b ==> c ==> d ==> e ==> f ==> g)
curryEF6 (EqFunc {tag, func}) =
    mkEqFunc6
        (curried 5 tag)
        (curry6 func)


curryEF7 :: ∀ a b c d e f g h. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => (Tuple7 a b c d e f g ==> h) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h)
curryEF7 (EqFunc {tag, func}) =
    mkEqFunc7
        (curried 6 tag)
        (curry7 func)


curryEF8 :: ∀ a b c d e f g h i. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => (Tuple8 a b c d e f g h ==> i) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i)
curryEF8 (EqFunc {tag, func}) =
    mkEqFunc8
        (curried 7 tag)
        (curry8 func)


curryEF9 :: ∀ a b c d e f g h i j. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => (Tuple9 a b c d e f g h i ==> j) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j)
curryEF9 (EqFunc {tag, func}) =
    mkEqFunc9
        (curried 8 tag)
        (curry9 func)


curryEF10 :: ∀ a b c d e f g h i j k. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => (Tuple10 a b c d e f g h i j ==> k) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j ==> k)
curryEF10 (EqFunc {tag, func}) =
    mkEqFunc10
        (curried 9 tag)
        (curry10 func)


-- | Like `uncurry` for plain old functions, but equatable.
-- |
-- |     add :: Int ==> Int ==> Int
-- |     add = eqFunc2 (+)
-- |
-- |     uncurryEF add == uncurryEF add
-- |     curryEF (uncurryEF add) == add
-- |
-- |     (uncurryEF add) ~ (Tuple 2 3) == 5
uncurryEF :: ∀ a b c. (a ==> b ==> c) -> (Tuple a b ==> c)
uncurryEF (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-1) tag)
        (uncurry (func >>> runEF))


uncurryEF3 :: ∀ a b c z. (a ==> b ==> c ==> z) -> (Tuple3 a b c ==> z)
uncurryEF3 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-2) tag)
        (uncurry3 (func >>> runEF2))


uncurryEF4 :: ∀ a b c d z. (a ==> b ==> c ==> d ==> z) -> (Tuple4 a b c d ==> z)
uncurryEF4 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-3) tag)
        (uncurry4 (func >>> runEF3))


uncurryEF5 :: ∀ a b c d e z. (a ==> b ==> c ==> d ==> e ==> z) -> (Tuple5 a b c d e ==> z)
uncurryEF5 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-4) tag)
        (uncurry5 (func >>> runEF4))


uncurryEF6 :: ∀ a b c d e f z. (a ==> b ==> c ==> d ==> e ==> f ==> z) -> (Tuple6 a b c d e f ==> z)
uncurryEF6 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-5) tag)
        (uncurry6 (func >>> runEF5))


uncurryEF7 :: ∀ a b c d e f g z. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> z) -> (Tuple7 a b c d e f g ==> z)
uncurryEF7 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-6) tag)
        (uncurry7 (func >>> runEF6))


uncurryEF8 :: ∀ a b c d e f g h z. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> z) -> (Tuple8 a b c d e f g h ==> z)
uncurryEF8 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-7) tag)
        (uncurry8 (func >>> runEF7))


uncurryEF9 :: ∀ a b c d e f g h i z. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> z) -> (Tuple9 a b c d e f g h i ==> z)
uncurryEF9 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-8) tag)
        (uncurry9 (func >>> runEF8))


uncurryEF10 :: ∀ a b c d e f g h i j z. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j ==> z) -> (Tuple10 a b c d e f g h i j ==> z)
uncurryEF10 (EqFunc {tag, func}) =
    mkEqFunc
        (curried (-9) tag)
        (uncurry10 (func >>> runEF9))


-- A tag that tracks things about how a function was created, so that
-- we have some chance of determining that two functions are equal,
-- even if they are not literally the same function.
--
-- The `Plain` tag covers the most primitive case, where we don't know
-- anything about how the function was made. In that case, we just tag
-- the function with a unique ID -- it is, after all, at least equal
-- to itself, and that's all we know.
--
-- The other tags cover cases where we do know something about how the
-- function was made, so we can make some inferences about which functions
-- are equal to each other. As a simple example, `const 5` must be equal to
-- `const 5`, even if they are produced separately (and thus not literally
-- the same function).
data Tag
    = Plain Int
    | Id
    | Composed (List Tag)
    | Applied AppliedRec
    | Flipped Tag
    | Curried Int Tag

    | ChoseLeft Tag
    | ChoseRight Tag

    | StrongFirst Tag
    | StrongSecond Tag


instance showTag :: Show Tag where
    show (Plain id) = "(Plain " <> show id <> ")"
    show (Id) = "Id"
    show (Composed tags) = "(Composed " <> show tags <> ")"
    show (Flipped tag) = "(Flipped " <> show tag <> ")"
    show (Applied rec) = "(Applied " <> show rec <> ")"
    show (Curried level parent) = "(Curried " <> show level <> " " <> show parent <> ")"

    show (ChoseLeft tag) = "(ChoseLeft " <> show tag <> ")"
    show (ChoseRight tag) = "(ChoseRight " <> show tag <> ")"

    show (StrongFirst tag) = "(StrongFirst " <> show tag <> ")"
    show (StrongSecond tag) = "(StrongSecond " <> show tag <> ")"


instance semigroupTag :: Semigroup Tag where
    -- This is special so that the Category laws are obeyed for EqFunc
    append Id other = other
    append other Id = other

    -- These are special again for obyeing the Category laws for EqFunc
    append (Composed list1) (Composed list2) = Composed (list1 <> list2)
    append (Composed list) tag2 = Composed (snoc list tag2)
    append tag1 (Composed list) = Composed (Cons tag1 list)

    append other1 other2 = Composed (other1 : other2 : Nil)


instance monoidTag :: Monoid Tag where
    mempty = Id


instance eqTag :: Eq Tag where
    eq (Plain id1) (Plain id2) = eq id1 id2
    eq (Composed list1) (Composed list2) = eq list1 list2
    eq (Flipped tag1) (Flipped tag2) = eq tag1 tag2
    eq (Applied a) (Applied b) = eq a b
    eq (Curried level1 parent1) (Curried level2 parent2) = level1 == level2 && parent1 == parent2
    eq Id Id = true

    eq (ChoseLeft tag1) (ChoseLeft tag2) = eq tag1 tag2
    eq (ChoseRight tag1) (ChoseRight tag2) = eq tag1 tag2

    eq (StrongFirst tag1) (StrongFirst tag2) = eq tag1 tag2
    eq (StrongSecond tag1) (StrongSecond tag2) = eq tag1 tag2

    eq _ _ = false


newtype AppliedRec = AppliedRec
    { parentTag :: Tag
    , param :: SomeEq
    }

instance eqAppliedRec :: Eq AppliedRec where
    eq (AppliedRec a) (AppliedRec b) =
        (a.parentTag == b.parentTag) &&
        (a.param == b.param)


instance showAppliedRec :: Show AppliedRec where
    show (AppliedRec {parentTag}) = "(AppliedRec {parentTag: " <> show parentTag <> "})"


-- | Given a top-level function, make an equatable function.
-- |
-- | For best results, the function you provide should be defined at the top-level
-- | of your module, rather than being the value returned by another function. The
-- | problem if your function is generated by another function is that you'll get a
-- | differnt `EqFunc` every time. So, they will never be equal to each other, even if
-- | we know that they must be.
-- |
-- | As far as I know, there isn't any way in the type system to enforce that the
-- | incoming function has not been produced by another function. So, you just have to
-- | verify that for yourself.
eqFunc :: ∀ a b. (a -> b) -> (a ==> b)
eqFunc func =
    mkEqFunc (uniqueTag func) func


-- | Given a function of two parameters, make an equatable function.
eqFunc2 :: ∀ a b c. (Eq a) => (a -> b -> c) -> (a ==> b ==> c)
eqFunc2 func =
    mkEqFunc2 (uniqueTag func) func


eqFunc3 :: ∀ a b c d. (Eq a, Eq b) => (a -> b -> c -> d) -> (a ==> b ==> c ==> d)
eqFunc3 func =
    mkEqFunc3 (uniqueTag func) func


eqFunc4 :: ∀ a b c d e. (Eq a, Eq b, Eq c) => (a -> b -> c -> d -> e) -> (a ==> b ==> c ==> d ==> e)
eqFunc4 func =
    mkEqFunc4 (uniqueTag func) func


eqFunc5 :: ∀ a b c d e f. (Eq a, Eq b, Eq c, Eq d) => (a -> b -> c -> d -> e -> f) -> (a ==> b ==> c ==> d ==> e ==> f)
eqFunc5 func =
    mkEqFunc5 (uniqueTag func) func


eqFunc6 :: ∀ a b c d e f g. (Eq a, Eq b, Eq c, Eq d, Eq e) => (a -> b -> c -> d -> e -> f -> g) -> (a ==> b ==> c ==> d ==> e ==> f ==> g)
eqFunc6 func =
    mkEqFunc6 (uniqueTag func) func


eqFunc7 :: ∀ a b c d e f g h. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => (a -> b -> c -> d -> e -> f -> g -> h) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h)
eqFunc7 func =
    mkEqFunc7 (uniqueTag func) func


eqFunc8 :: ∀ a b c d e f g h i. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i)
eqFunc8 func =
    mkEqFunc8 (uniqueTag func) func


eqFunc9 :: ∀ a b c d e f g h i j. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j)
eqFunc9 func =
    mkEqFunc9 (uniqueTag func) func


eqFunc10 :: ∀ a b c d e f g h i j k. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j ==> k)
eqFunc10 func =
    mkEqFunc10 (uniqueTag func) func


applied :: ∀ a. (Eq a) => Tag -> a -> Tag
applied parentTag param =
    Applied $ AppliedRec {parentTag, param: someEq param}


mkEqFunc :: ∀ a b. Tag -> (a -> b) -> (a ==> b)
mkEqFunc t func =
    EqFunc {tag: t, func}


mkEqFunc2 :: ∀ a b c. (Eq a) => Tag -> (a -> b -> c) -> (a ==> b ==> c)
mkEqFunc2 t func =
    mkEqFunc t \a ->
        mkEqFunc
            (applied t a)
            (func a)


mkEqFunc3 :: ∀ a b c d. (Eq a, Eq b) => Tag -> (a -> b -> c -> d) -> (a ==> b ==> c ==> d)
mkEqFunc3 t func =
    mkEqFunc t \a ->
        mkEqFunc2
            (applied t a)
            (func a)


mkEqFunc4 :: ∀ a b c d e. (Eq a, Eq b, Eq c) => Tag -> (a -> b -> c -> d -> e) -> (a ==> b ==> c ==> d ==> e)
mkEqFunc4 t func =
    mkEqFunc t \a ->
        mkEqFunc3
            (applied t a)
            (func a)


mkEqFunc5 :: ∀ a b c d e f. (Eq a, Eq b, Eq c, Eq d) => Tag -> (a -> b -> c -> d -> e -> f) -> (a ==> b ==> c ==> d ==> e ==> f)
mkEqFunc5 t func =
    mkEqFunc t \a ->
        mkEqFunc4
            (applied t a)
            (func a)


mkEqFunc6 :: ∀ a b c d e f g. (Eq a, Eq b, Eq c, Eq d, Eq e) => Tag -> (a -> b -> c -> d -> e -> f -> g) -> (a ==> b ==> c ==> d ==> e ==> f ==> g)
mkEqFunc6 t func =
    mkEqFunc t \a ->
        mkEqFunc5
            (applied t a)
            (func a)


mkEqFunc7 :: ∀ a b c d e f g h. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Tag -> (a -> b -> c -> d -> e -> f -> g -> h) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h)
mkEqFunc7 t func =
    mkEqFunc t \a ->
        mkEqFunc6
            (applied t a)
            (func a)


mkEqFunc8 :: ∀ a b c d e f g h i. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Tag -> (a -> b -> c -> d -> e -> f -> g -> h -> i) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i)
mkEqFunc8 t func =
    mkEqFunc t \a ->
        mkEqFunc7
            (applied t a)
            (func a)


mkEqFunc9 :: ∀ a b c d e f g h i j. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Tag -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j)
mkEqFunc9 t func =
    mkEqFunc t \a ->
        mkEqFunc8
            (applied t a)
            (func a)


mkEqFunc10 :: ∀ a b c d e f g h i j k. (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Tag -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j ==> k)
mkEqFunc10 t func =
    mkEqFunc t \a ->
        mkEqFunc9
            (applied t a)
            (func a)


-- Generates a unique tag for a function, unless it already has a tag, in which
-- case it returns that tag.
foreign import uniqueTagImpl :: ∀ a b. (Int -> Tag) -> (a -> b) -> Tag


uniqueTag :: ∀ a b. (a -> b) -> Tag
uniqueTag = uniqueTagImpl Plain


-- | Applies an `EqFunc` to an argument.
-- |
-- | You can also use `~` as an infix operator, with the precedence of
-- } ordinary function application. Or, you can use `=$=`, with a precedence
-- | like `$`.
runEF :: ∀ a b. (a ==> b) -> (a -> b)
runEF (EqFunc func) = func.func


infixr 0 runEF as =$=
infixl 9 runEF as ~


-- | Like `runEF`, but with its arguments flipped. You can also use the
-- | infix operator `=#=`, with a precedence like `#`.
runFlippedEF :: ∀ a b. a -> (a ==> b) -> b
runFlippedEF = flip runEF

infixl 1 runFlippedEF as =#=


runEF2 :: ∀ a b c. (a ==> b ==> c) -> (a -> b -> c)
runEF2 (EqFunc {func}) = func >>> runEF

runEF3 :: ∀ a b c d. (a ==> b ==> c ==> d) -> (a -> b -> c -> d)
runEF3 (EqFunc {func}) = func >>> runEF2

runEF4 :: ∀ a b c d e. (a ==> b ==> c ==> d ==> e) -> (a -> b -> c -> d -> e)
runEF4 (EqFunc {func}) = func >>> runEF3

runEF5 :: ∀ a b c d e f. (a ==> b ==> c ==> d ==> e ==> f) -> (a -> b -> c -> d -> e -> f)
runEF5 (EqFunc {func}) = func >>> runEF4

runEF6 :: ∀ a b c d e f g. (a ==> b ==> c ==> d ==> e ==> f ==> g) -> (a -> b -> c -> d -> e -> f -> g)
runEF6 (EqFunc {func}) = func >>> runEF5

runEF7 :: ∀ a b c d e f g h. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h) -> (a -> b -> c -> d -> e -> f -> g -> h)
runEF7 (EqFunc {func}) = func >>> runEF6

runEF8 :: ∀ a b c d e f g h i. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i) -> (a -> b -> c -> d -> e -> f -> g -> h -> i)
runEF8 (EqFunc {func}) = func >>> runEF7

runEF9 :: ∀ a b c d e f g h i j. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j)
runEF9 (EqFunc {func}) = func >>> runEF8

runEF10 :: ∀ a b c d e f g h i j k. (a ==> b ==> c ==> d ==> e ==> f ==> g ==> h ==> i ==> j ==> k) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k)
runEF10 (EqFunc {func}) = func >>> runEF9


instance eqEqFunc :: Eq (EqFunc a b) where
    eq (EqFunc func1) (EqFunc func2) = eq func1.tag func2.tag


instance semigroupoidEqFunc :: Semigroupoid EqFunc where
    compose (EqFunc func1) (EqFunc func2) =
        EqFunc
            { func: compose func1.func func2.func
            , tag: func1.tag <> func2.tag
            }


instance categoryEqFunc :: Category EqFunc where
    -- So, our func is the usual `id`. The tag is a special
    -- tag that, when cmoposed, will simply take on the tag
    -- of the other function.
    id =
        EqFunc
            { func: id
            , tag: Id
            }


mapEF :: ∀ f a b. Functor f => (a ==> b) -> f a -> f b
mapEF = map <<< runEF
