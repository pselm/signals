
-- | One sometimes wants to compare two functions in order to
-- | determine whether they are equal. This is, sadly, impossible, in the general
-- | case -- that is, impossible if you know nothing about where the function came
-- | from.
-- |
-- | However, it is possible to determine that two functions came from the same
-- | place -- that is, that they are the same function. One way to do this is via
-- | referential equality in the FFI. However, there are at least two problems
-- | with this:
-- |
-- | * It is, in principle, fragile in the face of optimizations that the compiler
-- |   may or may not do.
-- |
-- | * It does not always compose well. Of course, if two referentially-equal functions are
-- |   composed at the top-level, that is done once, and the resulting function will
-- |   be referentially equal to itself. However, if the referentially-equal functions
-- |   are composed under other circumstances, it is possible for a new function to
-- |   be created each time -- not referentially equal, even though we could infer
-- |   equality.
-- |
-- | So, what to do? The idea this module expresses is that we can make a kind
-- | of reference equality less fragile by "wrapping" a function with a new type
-- | -- `EqFunc` -- which contains a unique tag. Given the unique tag, we
-- | can equate the wrapper -- and thus, by necessary implication, the function inside.
-- |
-- | We can also provide for robust composition of the `EqFunc`, preserving
-- | the ability to equate the composed function.
-- |
-- | So, in cases where you need to equate functions, you can ask for a `EqFunc`
-- | instead. If two `EqFunc` are equal, then the functions inside are
-- | necessarily equal.
-- |
-- | Now, if two `EqFunc` are unequal, it is still possible for the wrapped
-- | functions to be equal, in the sense that they may be defined (separately) in the
-- | same way. So, for best results, you need to follow certain rules which
-- | are specified in the documentation for `mkEF` and `runEqFunc`.
-- |
-- | If possible, it's even better to refactor so that you don't have to compare
-- | functions. But, at least `EqFunc` gives you some opportunity to do so when needed.

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
import Data.Eq.Any (AnyEq, anyEq)
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


newtype EqFunc a b = EqFunc
    { func :: a -> b
    , tag :: Tag
    }


-- A nice type operator, so you can do things like ...
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


flipEF :: ∀ a b c. (Eq b) => (a ==> b ==> c) -> (b ==> a ==> c)
flipEF (EqFunc {tag, func}) =
    mkEqFunc2
        case tag of
            Flipped parent -> parent
            _ -> Flipped tag

        \b a ->
            (func a) ~ b


constEF :: ∀ a b. (Eq a) => a -> (b ==> a)
constEF = runEF (eqFunc2 const)


curried :: Int -> Tag -> Tag
curried times (Curried level parent) | level + times == 0 = parent
curried times (Curried level parent) = Curried (level + times) parent
curried times parent = Curried times parent


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
--
-- The b type parameter represents the return type of the function.
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
    append Id other = other
    append other Id = other

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
    , param :: AnyEq
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
    Applied $ AppliedRec {parentTag, param: anyEq param}


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
runEF :: ∀ a b. (a ==> b) -> (a -> b)
runEF (EqFunc func) = func.func


infixr 0 runEF as =$=
infixl 9 runEF as ~


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
