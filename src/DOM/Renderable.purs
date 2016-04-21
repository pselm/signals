
-- | A module which ties together some general concepts for rendering things to the
-- | DOM.
-- |
-- | The general idea is that there are multiple approaches to taking some data
-- | structure rendering something to the DOM as a result.  One common element is
-- | the desire to be able to "update" the DOM based on changes (to avoid entirely
-- | replacing the DOM every time).  This module provides a general interface for
-- | that sort of thing.
-- |
-- | The hope is that this enables a degree of mix-and-match between different
-- | approaches. If each uses `DOM.Renderable`, and provides an instance for
-- | `DOM.Renderable`, then you imagine using them together in a nice way, even if
-- | they don't know about each other.  In any event, that's the hope.
-- |
-- | So, this module doesn't provide very much functionality as such. It is mostly
-- | meant as a possible rendezvous for other modules.
-- |
-- | ## Making something `Renderable`
-- |
-- | The easy way to make something `Renderable` is to make it an instance
-- | of the `Renderable` class. So, you just implement the `render` and `update`
-- | methods ... see the discussion below for details.
-- |
-- | If you can implement the `render` and `update` methods, but it's awkward to
-- | create an instance, then you can also use the `makeDynamic` function to
-- | make a `DynamicRenderable`.
-- |
-- | Note that the best architecture is typically not to go straight from your
-- | own specialized data types to `Renderable`, but instead to have a kind of
-- | virtual-DOM layer in between -- it is the virtual DOM types that would be
-- | `Renderable`, rather than your own more specialized types. So, you would
-- | typically have a set of functions to convert from your specialized types
-- | to a virtual-DOM layer, and it is those virtual DOM types which would have
-- | a `Renderable` instance.
-- |
-- | ## Using a `Renderable`
-- |
-- | To use a `Renderable`, you can, of course, just use the `render` or
-- | `update` functions.
-- |
-- | However, neither of those methods necessarily modify the DOM, so you'll
-- | often want `renderIntoDOM` or `updateDOM` instead. (The `update` method
-- | may simply modify the node you pass in, but it may instead return a new node ...
-- | so you need to call `updateDOM` if you want to be sure to, well, update
-- | the DOM).
-- |
-- | ## Making it more dynamic
-- |
-- | Now, a `Renderable` instance is parameterized by the type of the original
-- | data. This is a shame, in a way, since you might want to do things like keep
-- | a list of `Renderable` things -- that is, a list of things that are all
-- | `Renderable`, but of different types. Furthermore, you might, in traversing
-- | a virtual DOM structure, want to use `update` in a situation where the previous
-- | `Renderable` may or may not have had the same underlying type.
-- |
-- | To facilitate this sort of thing, we also have a `DynamicRenderable` type which,
-- | in effect, "erases" the type of the underlying data. This leaves you with a
-- | "simple" type that you can use in lists etc. Even better, the `DynamicRenderable`
-- | is itself an instance of `Renderable` -- isn't that nice? So you can use the usual
-- | functions, even if the underlying data type doesn't match.
-- |
-- | To get a `DynamicRenderable` from a `Renderable`, take a look at `toDynamic`.
-- | Alternative, you can also use `makeDynamic`, if it's awkward to make an instance.

module DOM.Renderable
    ( class Renderable, render, update, defaultUpdate
    , Rendered, Position(..), renderIntoDOM, updateDOM
    , DynamicRenderable, toDynamic, makeDynamic
    ) where

import Control.Monad (when)
import Control.Monad.Eff (Eff, untilE)
import DOM (DOM)
import DOM.Node.Types (Node)
import DOM.Node.Node (parentNode, firstChild, appendChild, replaceChild, removeChild, insertBefore)
import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..))
import Data.Exists (Exists, runExists, mkExists)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Unit, bind, pure, not, void, ($), (#))

-- | A `Renderable` is somethng that knows how to render some data type as a DOM
-- | node. If given a previous version of the data, and the previously rendered
-- | DOM node, it may also know how to efficiently `update` that node.
-- |
-- | * The `render` method takes some data and produces a newly-created DOM node.
-- |
-- | * The `update` method takes a previously `Rendered` structure, with the previous
-- |   data and the node previously created from that data.
-- |
-- |   You can either modify the provided node "in place" or create a new one. If you
-- |   modify the node in place, then you should return it. If you create a new node,
-- |   you should return that.  It is the caller's job to stitch whatever you return
-- |   into whatever larger structure is involved -- you shouldn't do anything outside
-- |   of the DOM you are given.
-- |
-- |   You can use the `defaultUpdate` function to implement `update` if you're in a
-- |   situation where no optimization is possible.  It simply calls `render`,
-- |   ignoring the previous data and node.
-- |
-- | * So, calling `render` and `update` should produce the same results,
-- |   so long as the `Rendered` parameter provided to `update` follows the rules.
-- |
-- |   Now, it is always possible that some extraneous code has modified the DOM in
-- |   the meantime. However, ideally you won't do that except in ways that your
-- |   `Renderable` explicitly permits (and is able to detect or compensate for in its
-- |   `update` method).
class Renderable a where
    render :: ∀ e. a -> Eff (dom :: DOM | e) Node
    update :: ∀ e. Rendered a -> a -> Eff (dom :: DOM | e) Node


-- | A default implementation of `update`, which just calls `render` on the
-- | new data, ignoring the previous state.
defaultUpdate :: ∀ a e. (Renderable a) => Rendered a -> a -> Eff (dom :: DOM | e) Node
defaultUpdate rendered = render


-- | A record that associates some data with the DOM rendered from that data.  Note
-- | that the users of this type assume that the `result` really was generated by
-- | rendering the `value`. If this is not true, behaviour is undefined.
type Rendered a =
    { value :: a
    , result :: Node
    }


-- Packages up a value with the render and update methods that can act on it
newtype RenderableValue a = RenderableValue
    { value :: a
    , render :: ∀ e. a -> Eff (dom :: DOM | e) Node
    , update :: ∀ e. Rendered a -> a -> Eff (dom :: DOM | e) Node
    }


-- Perhaps perverse, but take the package and make a `Renderable` instance
instance renderableRenderableValue :: Renderable (RenderableValue a) where
    render (RenderableValue a) = a.render a.value

    update {value: (RenderableValue oldValue), result} (RenderableValue new) =
        new.update
            { value: oldValue.value
            , result
            }
            new.value


-- Makes a RenderableValue dynamic by stripping the underlying type
newtype DynamicRenderable = DynamicRenderable (Exists RenderableValue)


-- Now, to heap perversity upon perversity, make a renderable instance for that!
instance renderableDynamicRenderable :: Renderable DynamicRenderable where
    render (DynamicRenderable dynamicRenderable) =
        runExists render dynamicRenderable

    -- This is the interesting bit ... since we've stripped the underlying type, we don't
    -- know whether we can use the `update` optimization ... or do we?
    update {value: (DynamicRenderable oldValue), result} (DynamicRenderable newValue) =
        newValue #
            runExists \(RenderableValue new) ->
                oldValue #
                    runExists \(RenderableValue old) ->
                        -- Check whether they use the same `update` function. This is basically
                        -- a proxy for determining whether they are the same "type". The theory
                        -- is that this is OK, since if they use the same `update` function,
                        -- we don't technically care if they are the same "type" -- the update
                        -- function should work.
                        --
                        -- One alternative would be to rely on purescript-generics in some way.
                        if same old.update new.update
                            then
                                new.update
                                    { value: unsafeCoerce old.value
                                    , result
                                    }
                                    new.value

                            else
                                new.render new.value


-- Whether one thing is the same as another ... that is, the identical thing, not equality.
-- Note that we allow the types to differ, since the compiler might think of them as different
-- types, for one reason or another.
foreign import same :: ∀ a b. a -> b -> Boolean


-- | Given a `Renderable` value, return a dynamic value that can be used
-- | without knowing the type of the original value.
toDynamic :: ∀ a. (Renderable a) => a -> DynamicRenderable
toDynamic = makeDynamic render update


-- | An alternative to `toDynamic`, in cases where you don't have a
-- | `Renderable` instance, but you do have the necessary functions.
makeDynamic :: ∀ a.
    (∀ e. a -> Eff (dom :: DOM | e) Node) ->
    (∀ e. Rendered a -> a -> Eff (dom :: DOM | e) Node) ->
    a ->
    DynamicRenderable

makeDynamic render update value =
    DynamicRenderable $
        mkExists $
            RenderableValue {value, render, update}


-- | Indicate a position for rendering via `renderIntoDOM`.
data Position
    = BeforeFirstChild
    | AfterLastChild
    | ReplacingChildren
    | ReplacingItself


renderIntoDOM :: ∀ e a. (Renderable a) => Position -> Node -> a -> Eff (dom :: DOM | e) (Rendered a)
renderIntoDOM position node value = do
    result <- render value

    case position of
        BeforeFirstChild -> do
            existingChild <- firstChild node

            case toMaybe existingChild of
                Just child ->
                    insertBefore result child node

                Nothing ->
                    appendChild result node

        AfterLastChild ->
            appendChild result node

        ReplacingChildren -> do
            removeChildren node
            appendChild result node

        ReplacingItself ->
            replaceNode node result

    pure { value, result }


removeChildren :: ∀ e. Node -> Eff (dom :: DOM | e) Unit
removeChildren parent =
    untilE do
        child <- firstChild parent

        case toMaybe child of
            Just c -> do
                removeChild c parent
                pure false

            Nothing ->
                pure true


replaceNode :: ∀ e. Node -> Node -> Eff (dom :: DOM | e) Node
replaceNode old new = do
    parent <- parentNode old

    case toMaybe parent of
        Just p ->
            replaceChild new old p

        Nothing -> do
            removeChildren old
            appendChild new old


-- | Updates previously rendered data, replacing the originally rendered node in the DOM.
updateDOM :: ∀ e a. (Renderable a) => Rendered a -> a -> Eff (dom :: DOM | e) (Rendered a)
updateDOM rendered value = do
    result <- update rendered value

    -- Update may return the same node that it was given
    when (not $ same rendered.result result) $ void $
        replaceNode rendered.result result

    pure { value, result }

