
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
    , eqFunc, eqFunc2, eqFunc3, eqFunc4
    , applyEF, (=$=)
    , applyFlippedEF, (=#=)
    , constEF, flipEF
    ) where


import Data.List (List(..), (:), snoc)
import Data.Exists (Exists, mkExists, runExists)
import Data.Monoid (class Monoid)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice, left, right)
import Data.Profunctor.Strong (class Strong, first, second)
import Unsafe.Coerce (unsafeCoerce)

import Prelude
    ( class Eq, eq, (==)
    , class Semigroup
    , class Semigroupoid, compose, (<<<), (>>>)
    , class Category, id
    , class Functor, map
    , class Show, show
    , (<>), flip, (#), ($), const
    )


newtype EqFunc a b = EqFunc
    { func :: a -> b
    , tag :: Tag
    }


-- A nice type operator, so you can do things like ...
infixr 4 type EqFunc as ==>


instance showEqFunc :: Show (EqFunc a b) where
    show (EqFunc func) = "(EqFunc {tag: " <> show func.tag <> "})"


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


flipEF :: forall a b c. (Eq b) => (a ==> b ==> c) -> (b ==> a ==> c)
flipEF (EqFunc {tag, func}) =
    mkEqFunc2
        case tag of
            Flipped parent -> parent
            _ -> Flipped tag

        \b a ->
            (applyEF (func a)) b


constEF :: forall a b. (Eq a) => a -> (b ==> a)
constEF = applyEF (eqFunc2 const)


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
    | Applied (Exists AppliedRec)
    | Flipped Tag

    | ChoseLeft Tag
    | ChoseRight Tag

    | StrongFirst Tag
    | StrongSecond Tag


instance showTag :: Show Tag where
    show (Plain id) = "(Plain " <> show id <> ")"
    show (Id) = "Id"
    show (Composed tags) = "(Composed " <> show tags <> ")"
    show (Flipped tag) = "(Flipped " <> show tag <> ")"

    show (ChoseLeft tag) = "(ChoseLeft " <> show tag <> ")"
    show (ChoseRight tag) = "(ChoseRight " <> show tag <> ")"

    show (StrongFirst tag) = "(StrongFirst " <> show tag <> ")"
    show (StrongSecond tag) = "(StrongSecond " <> show tag <> ")"

    show (Applied rec) =
        rec # runExists \r ->
            "(Applied " <> show r <> ")"


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
    eq Id Id = true

    eq (ChoseLeft tag1) (ChoseLeft tag2) = eq tag1 tag2
    eq (ChoseRight tag1) (ChoseRight tag2) = eq tag1 tag2

    eq (StrongFirst tag1) (StrongFirst tag2) = eq tag1 tag2
    eq (StrongSecond tag1) (StrongSecond tag2) = eq tag1 tag2

    eq (Applied a) (Applied b) =
        a # runExists \(AppliedRec aRec) ->
            b # runExists \(AppliedRec bRec) ->
                if aRec.parentTag == bRec.parentTag
                    then multiEq aRec.eqParam aRec.param bRec.eqParam bRec.param
                    else false

    eq _ _ = false


newtype AppliedRec a = AppliedRec
    { parentTag :: Tag
    , param :: a
    , eqParam :: a -> a -> Boolean
    }


instance showAppliedRec :: Show (AppliedRec a) where
    show (AppliedRec rec) = "(AppliedRec {parentTag: " <> show rec.parentTag <> "})"


foreign import refEq :: ∀ a b. a -> b -> Boolean


multiEq :: ∀ a b. (a -> a -> Boolean) -> a -> (b -> b -> Boolean) -> b -> Boolean
multiEq eqA a eqB b =
    if eqA `refEq` eqB
        then a `eqA` (unsafeCoerce b)
        else false


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


applied :: ∀ a. Tag -> a -> (a -> a -> Boolean) -> Tag
applied parentTag param eqParam =
    Applied $ mkExists $ AppliedRec {parentTag, param, eqParam}


mkEqFunc :: ∀ a b. Tag -> (a -> b) -> (a ==> b)
mkEqFunc t func =
    EqFunc {tag: t, func}


mkEqFunc2 :: ∀ a b c. (Eq a) => Tag -> (a -> b -> c) -> (a ==> b ==> c)
mkEqFunc2 t func =
    mkEqFunc t \a ->
        mkEqFunc
            (applied t a eq)
            (func a)


mkEqFunc3 :: ∀ a b c d. (Eq a, Eq b) => Tag -> (a -> b -> c -> d) -> (a ==> b ==> c ==> d)
mkEqFunc3 t func =
    mkEqFunc t \a ->
        mkEqFunc2
            (applied t a eq)
            (func a)


mkEqFunc4 :: ∀ a b c d e. (Eq a, Eq b, Eq c) => Tag -> (a -> b -> c -> d -> e) -> (a ==> b ==> c ==> d ==> e)
mkEqFunc4 t func =
    mkEqFunc t \a ->
        mkEqFunc3
            (applied t a eq)
            (func a)


mkEqFunc5 :: ∀ a b c d e f. (Eq a, Eq b, Eq c, Eq d) => Tag -> (a -> b -> c -> d -> e -> f) -> (a ==> b ==> c ==> d ==> e ==> f)
mkEqFunc5 t func =
    mkEqFunc t \a ->
        mkEqFunc4
            (applied t a eq)
            (func a)


mkEqFunc6 :: ∀ a b c d e f g. (Eq a, Eq b, Eq c, Eq d, Eq e) => Tag -> (a -> b -> c -> d -> e -> f -> g) -> (a ==> b ==> c ==> d ==> e ==> f ==> g)
mkEqFunc6 t func =
    mkEqFunc t \a ->
        mkEqFunc5
            (applied t a eq)
            (func a)


-- Generates a unique tag for a function, unless it already has a tag, in which
-- case it returns that tag.
foreign import uniqueTagImpl :: ∀ a b. (Int -> Tag) -> (a -> b) -> Tag


uniqueTag :: ∀ a b. (a -> b) -> Tag
uniqueTag = uniqueTagImpl Plain


-- | Applies an `EqFunc` to an argument.
applyEF :: ∀ a b. (a ==> b) -> (a -> b)
applyEF (EqFunc func) = func.func


infixr 0 applyEF as =$=


applyFlippedEF :: ∀ a b. a -> (a ==> b) -> b
applyFlippedEF = flip applyEF

infixl 1 applyFlippedEF as =#=


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
mapEF = map <<< applyEF
