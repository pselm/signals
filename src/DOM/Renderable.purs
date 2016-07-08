
-- | This module is intened to co-ordinate multiple approaches to taking a data
-- | structure which represents a virtual DOM, and rendering it to the real DOM.
-- | One common element is the desire to be able to "update" the DOM based on
-- | differences between the previous data and the new data (to avoid entirely
-- | replacing the DOM every time). This module provides a high-level interface for
-- | that sort of thing.
-- |
-- | The hope is that this enables a degree of mix-and-match between different
-- | approaches to a virtual DOM. If each uses `DOM.Renderable`, and provides an
-- | instance for `DOM.Renderable`, then you imagine using them together in a nice
-- | way, even if they don't know about each other.
-- |
-- | ## Making something `Renderable`
-- |
-- | To make something `Renderable`, make it an instance of the `Renderable` class.
-- | So, you just implement the `render` function (and possibly the `update`
-- | function -- see below for details). Note that the instance is only responsible
-- | for returning the new (or updated) DOM -- it is not responsible for stitching
-- | it into the real DOM.
-- |
-- | If you can implement the `render` and `update` methods, but it's awkward to
-- | create an instance, then you can use the `makeAnyRenderable` function to create
-- | an `AnyRenderable`.
-- |
-- | Note that the best architecture is often not to go straight from your
-- | own specialized data types to `Renderable`, but instead to have a kind of
-- | virtual-DOM layer in between. It is the virtual DOM types that would be
-- | `Renderable`, rather than your own more specialized types. So, you would
-- | typically have a set of functions to convert from your specialized types
-- | to a virtual-DOM layer, and it is those virtual DOM types which would have
-- | a `Renderable` instance.
-- |
-- | ## Using a `Renderable`
-- |
-- | To use a `Renderable`, you can either:
-- |
-- | * use the instance functions (`render` or `update`) directly
-- | * use `renderIntoDOM` or `updateDOM` to actually render or update the DOM
-- | * use `renderOrUpdate` to automatically remember the previous data
-- |
-- | ### Using the instance directly.
-- |
-- | To use a `Renderable`, you can, of course, just use the `render` or
-- | `update` functions directly.
-- |
-- | However, neither of those methods necessarily modify the DOM. The `render`
-- | method only returns the new DOM, not yet inserted into the real DOM. The
-- | `update` method may modify the node you pass in. but it may instead return a
-- | new node.
-- |
-- | So, you may prefer to use the helper methods instead.
-- |
-- | ### Using the helpers
-- |
-- | You can use `renderIntoDOM` or `updateDOM` to actually modify the DOM.
-- |
-- | The `renderIntoDOM` function is fairly straightforward -- it uses the
-- | `render` function to produce the new DOM, and then inserts it into the
-- | real DOM in the specified manner.
-- |
-- | To use `updateDOM`, you'll need to remember the previous `Renderable` data somehow.
-- | Now, a `Renderable` instance is parameterized by the type of the original
-- | data. This is a shame, in a way, since you might want to do things like keep
-- | a list of `Renderable` things -- that is, a list of things that are all
-- | `Renderable`, but of different types. Furthermore, you might, in traversing
-- | a virtual DOM structure, want to use `updateDOM` in a situation where the previous
-- | `Renderable` may or may not have had the same underlying type.
-- |
-- | To facilitate this sort of thing, we also have an `AnyRenderable` type
-- | which, in effect, "erases" the type of the underlying `Renderable` data. This
-- | leaves you with a "simple" `AnyRenderable` type that you can use in lists
-- | etc. Even better, the `AnyRenderable` is itself an instance of `Renderable`.
-- | So you can use the usual functions, even if the underlying data
-- | type doesn't match.
-- |
-- | To get an `AnyRenderable` from a `Renderable`, take a look at `toAnyRenderable`.
-- | Alternatively, you can also use `makeAnyRenderable`, if it's awkward to use an instance.
-- |
-- | ### Automatically remembering the previous data
-- |
-- | For the ultimate in convenience, there is the `renderOrUpdate` method. It stores
-- | the provided data in the DOM itself. Thus, you don't need to do anything to
-- | remember the previous data -- `renderOrUpdate` will use it if it finds it where
-- | it put it.

module DOM.Renderable
    ( class Renderable, render, update, defaultUpdate
    , Position(..), renderIntoDOM, documentForNode
    , Rendered, updateDOM
    , AnyRenderable, toAnyRenderable, makeAnyRenderable
    , renderOrUpdate
    ) where


import Control.Monad (when)
import Control.Monad.Eff (Eff, untilE)
import DOM (DOM)
import DOM.Node.Types (Document, Element, Node, elementToNode)
import DOM.Node.Node (parentNode, firstChild, appendChild, replaceChild, removeChild, insertBefore, ownerDocument)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Exists (Exists, runExists, mkExists)
import Unsafe.Coerce (unsafeCoerce)
import Graphics.Canvas (CANVAS)
import Prelude (Unit, bind, pure, unit, not, void, ($), (#), (<$>), (<#>))


-- | A `Renderable` is somethng that knows how to render some data type as a DOM
-- | node. If given a previous version of the data, and the previously rendered
-- | DOM node, it may also know how to efficiently `update` that node.
-- |
-- | * The `render` method takes some data and produces a newly-created DOM node
-- |   for the specified docuemnt.
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
-- |   Note that actually using `defaultUpdate` doesn't seem to be working, for
-- |   some reason. However, you can manually write out a trivial update function
-- |   in this way:
-- |
-- |       update rendered = render
-- |
-- | * So, calling `render` and `update` should produce the same results,
-- |   so long as the `Rendered` parameter provided to `update` follows the rules.
-- |
-- |   Now, it is always possible that some extraneous code has modified the DOM in
-- |   the meantime. However, ideally you won't do that except in ways that your
-- |   `Renderable` explicitly permits (and is able to detect or compensate for in its
-- |   `update` method).
class Renderable a where
    render :: ∀ e. Document -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node
    update :: ∀ e. Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node

-- Originally, I didn't have `Canvas` in the row of effects, but it looks as though
-- you need to list anything that an instance might want to use. In a way,
-- perhaps it is nice -- to be able to guarantee that an instance can't do any
-- effects other than DOM or Canvas.

-- Also, I didn't originally provide the `Document` to `render` as an argument.
-- However, one really ought to. Otherwise, you have to assume some document,
-- for things like `createElement` to work, which could be mistaken.


-- | A default implementation of `update`, which just calls `render` on the
-- | new data, ignoring the previous state.
-- |
-- | Note that the compiler doesn't seem to be happy if you use this in implementing
-- | an instance. However, you can manually write out a trivial `update` implementation
-- | as follows:
-- |
-- |       update rendered = render rendered.document
defaultUpdate :: ∀ a e. (Renderable a) => Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node
defaultUpdate rendered =
    render rendered.document


-- TODO: This isn't actually working ... if you try to use defaultUpdate to implement
-- update, you get a complaint from the compiler that the instance isn't defined yet
-- ... that is, it complains about a circular definition.


-- | A record that associates some data with the DOM rendered from that data.  Note
-- | that the users of this type assume that the `result` really was generated by
-- | rendering the `value`. If this is not true, behaviour is undefined. Also, the
-- | `document` must be the document which contains the node.
type Rendered a =
    { value :: a
    , result :: Node
    , document :: Document
    }


-- Packages up a value with the render and update methods that can act on it
newtype RenderableValue a = RenderableValue
    { value :: a
    , render :: ∀ e. Document -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node
    , update :: ∀ e. Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node
    }


-- Perhaps perverse, but take the package and make a `Renderable` instance
instance renderableRenderableValue :: Renderable (RenderableValue a) where
    render doc (RenderableValue a) = a.render doc a.value

    update {value: (RenderableValue oldValue), result, document} (RenderableValue new) =
        new.update
            { value: oldValue.value
            , result
            , document
            }
            new.value


-- | Strips the underlying type from a `Renderable`.
-- |
-- | You can create an `AnyRenderable` via `toAnyRenderable` or `makeAnyRenerable`.
-- |
-- | It's useful if you want to keep values in a `List` (so they need to
-- | to have the same type), or if you are remembering previous values
-- | for use via `update`, and they may or may not have the same underlying
-- | type.
-- |
-- | There is a `Renderable` instance for `AnyRenderable`, so you can
-- | use it with the regular functions.
newtype AnyRenderable = AnyRenderable (Exists RenderableValue)


-- Now, to heap perversity upon perversity, make a renderable instance for that!
instance renderableAnyRenderable :: Renderable AnyRenderable where
    render document (AnyRenderable anyRenderable) =
        runExists (render document) anyRenderable

    -- This is the interesting bit ... since we've stripped the underlying type, we don't
    -- know whether we can use the `update` optimization ... or do we?
    update {value: (AnyRenderable oldValue), result, document} (AnyRenderable newValue) =
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
                                    , document
                                    }
                                    new.value

                            else
                                new.render document new.value


-- Whether one thing is the same as another ... that is, the identical thing, not equality.
-- Note that we allow the types to differ, since the compiler might think of them as different
-- types, for one reason or another.
foreign import same :: ∀ a b. a -> b -> Boolean


-- | Given a `Renderable` value, return a value that can be used
-- | without knowing the type of the original data.
toAnyRenderable :: ∀ a. (Renderable a) => a -> AnyRenderable
toAnyRenderable = makeAnyRenderable render update


-- | An alternative to `toAnyRenderable`, in cases where you don't have a
-- | `Renderable` instance, but you do have the necessary functions.
makeAnyRenderable :: ∀ a.
    (∀ e. Document -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node) ->
    (∀ e. Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node) ->
    a ->
    AnyRenderable

makeAnyRenderable render update value =
    AnyRenderable $
        mkExists $
            RenderableValue {value, render, update}


-- | Indicate a position for rendering via `renderIntoDOM`.
data Position
    = BeforeFirstChild
    | AfterLastChild
    | ReplacingChildren
    | ReplacingItself


-- | Given a node, returns the document which the node belongs to.
documentForNode :: ∀ e. Node -> Eff (dom :: DOM | e) Document
documentForNode node =
    -- The unsafeCoerce should be safe, because if `ownerDocument`
    -- returns null, then the node itself must be the document.
    ownerDocument node
        <#> toMaybe
        <#> fromMaybe (unsafeCoerce node)


-- | Calls `render` on the provided value, and inserts the result into the DOM,
-- | at the given position, relative to the provided node.
-- |
-- | This is unlike `render`, in that `render` merely produces the new DOM,
-- | without inserting it anywhere.
-- |
-- | Once you've used `renderIntoDOM`, you could save the resulting `Rendered`
-- | structure, and supply it to `updateDOM` when your data changes.
-- |
-- | Or, for even more convenience, you could consider `renderOrUpdate`, which
-- | keeps track of the previous data for you.
renderIntoDOM :: ∀ e a. (Renderable a) => Position -> Node -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) (Rendered a)
renderIntoDOM position node value = do
    document <- documentForNode node
    result <- render document value

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

        ReplacingItself -> do
            replaceNode node result
            pure node

    pure { value, result, document }


removeChildren :: ∀ e. Node -> Eff (canvas :: CANVAS, dom :: DOM | e) Unit
removeChildren parent =
    untilE do
        child <- firstChild parent

        case toMaybe child of
            Just c -> do
                removeChild c parent
                pure false

            Nothing ->
                pure true


replaceNode :: ∀ e. Node -> Node -> Eff (canvas :: CANVAS, dom :: DOM | e) Unit
replaceNode old new = do
    parent <- parentNode old

    case toMaybe parent of
        Just p ->
            void $ replaceChild new old p

        Nothing ->
            -- If the old node has been removed from the DOM, assume that
            -- someone has already stitched the new one in place.
            pure unit


-- | Updates previously data previously rendered via `renderToDOM`, replacing
-- | the originally rendered node in the DOM. Returns a structure that can be
-- | provided to `updateDOM` for future changes in the value.
updateDOM :: ∀ e a. (Renderable a) => Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) (Rendered a)
updateDOM rendered value = do
    result <- update rendered value

    -- Update may return the same node that it was given
    when (not $ same rendered.result result) $
        replaceNode rendered.result result

    pure { value, result, document: rendered.document }


foreign import setRenderable :: ∀ e. Element -> AnyRenderable -> Eff (dom :: DOM | e) Unit

foreign import getRenderable :: ∀ e. Element -> Eff (dom :: DOM | e) (Nullable AnyRenderable)


-- | Renders the provided `Renderable` and inserts the result as a child of the
-- | provided `Element`, replacing all other chidlren.
-- |
-- | This stores the provided `Renderable` on the `Element`. So, when you call this again
-- | with the same `Element`, we may be able to use the renderable's `update`
-- | method, to efficiently update the DOM.
renderOrUpdate :: ∀ e a. (Renderable a) => Element -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Unit
renderOrUpdate element renderable = do
    let
        new =
            toAnyRenderable renderable

    old <-
        toMaybe <$> getRenderable element

    node <-
        toMaybe <$> firstChild (elementToNode element)

    case {old, node} of
        { old: Just value
        , node: Just result
        } -> do
            -- We've got both an old value and a child, so try an update
            document <- documentForNode result
            updateDOM {value, result, document} new

        _ ->
            -- Otherwise, we need a render
            renderIntoDOM ReplacingChildren (elementToNode element) new

    -- In either case, remember the anyRenderable
    setRenderable element new

