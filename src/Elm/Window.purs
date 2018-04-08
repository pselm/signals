
-- | Provides information about the container that your Elm program lives in.
-- | When you embed Elm in a `<div>` it gives the dimensions of the container, not
-- | the whole window.

module Elm.Window
    ( dimensions, width, height
    -- The rest are subject to change
    , WindowCallback, WindowState
    , setupWindow, setupGlobalWindow
    ) where


import Elm.Signal (Signal, DELAY, GraphState, Graph, send, mailbox, map, current, delay)

import Prelude (pure, ($), bind, discard, unit, const, (<<<), (>>=), (==), (&&))
import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)

import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLElement, windowToEventTarget)
import DOM.HTML.Event.EventTypes (resize)
import DOM.Event.EventTarget (eventListener, addEventListener)


-- This needs a base "node" for the container, as documented above, like Elm.Mouse.
-- TODO: Should integrate ... that is, there should be a base ReaderT type for
-- specifying the container ... then the various things that need to refer to
-- the container can be implemented to require it.
--
-- In fact, what I probably should do is have a *single* StateT/ReaderT that, in effect,
-- represents the "Elm runtime", integrating at least some of the setup stuff, including
-- Signal, Mouse, Window, and possibly others ... that would really simplify
-- the use.


-- | The Window API uses some hidden state, managed by this type.
type WindowCallback m a = ReaderT WindowState (StateT Graph m) a


{- In `Elm.Mouse`, we treated the "fullscreen" case as if one had embedded with
the <body>, which probably works (though now I should really check -- it may be
that there is a difference there I didn't originally appreciate).

It's different here, though, because the Elm implementation of `dimensions`
behaves fundamentally differently in "fullscreen" and "embed" modes. In
"fullscreen" mode, it gives you the window's height, whereas in "embed" mode,
it gives you the container's height. Of course, the height of the window is
fundamentally different from the height of the body ... e.g. if the body flows
off-screen, the body will have more height than the window. So, we actually do
need to distinguish between the two modes, it seems.

This probably means that eventually we need an `Elm.Runtime` wrapper that
specifies this sort of thing ... see comment above, and, for that matter,
https://github.com/rgrempel/purescript-elm/issues/3.

Also, note that the Elm runtime, in fullscreen mode, actually creates a
`container` node as a <div> which is the child of <body>. I don't think this
matters so far, but presumably it will when I get to the stuff that actually
*modifies* the DOM ... presumably the point is to isolate the DOM changes,
starting from a "fresh" div.

Note that unlike height, `dimensions` always uses the width of the container,
rather than the width of the window. This is a little curious, since they
aren't necessarily the same. Well, actually the case in which they are not the
same is a "zoomed" case in Safari -- oddly enough, the window.width goes
*down*, but the container width does not. I suppose that makes a kind of sense
... the container is still the same size in CSS pixels, but the window's
viewport is actually *smaller* in terms of CSS pixels, given the zooming.  In
any event, may as well just do what Elm does.

In any event, for now `node` is a `Maybe` ... eventually, I'll need to rejig
this in concert with an actual `Elm.Runtime` module.
-}
type WindowState =
    { dimensions :: Signal (Tuple Int Int)
    , node :: Maybe HTMLElement
    }

foreign import clientWidth :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Int
foreign import clientHeight :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Int
foreign import innerHeight :: ∀ e. Eff (dom :: DOM | e) Int

makeWindowState :: ∀ e m.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m) =>
    Maybe HTMLElement -> GraphState m WindowState

makeWindowState node = do
    win <- liftEff window

    container <-
        case node of
            Just n ->
                pure n

            Nothing -> do
                nullableBody <- liftEff $ document win >>= body

                case nullableBody of
                    Just nb -> pure nb
                    Nothing -> unsafeCrashWith "Couldn't find document.body"

    let
        getWidth =
            clientWidth container

        getHeight =
            case node of
                Just _ ->
                    clientHeight container

                Nothing ->
                    innerHeight

    initialWidth <- liftEff $ getWidth
    initialHeight <- liftEff $ getHeight

    mbox <- mailbox (Tuple 0 0)

    let
        listener =
            eventListener $
                const do
                    -- Check if the dimensions have really changed
                    newWidth <- getWidth
                    newHeight <- getHeight
                    old <- current mbox.signal

                    if newWidth == (fst old) && newHeight == (snd old)
                        then pure unit
                        else
                            -- Even if it appears to have changed, check
                            -- again on next cycle to see if it really changed.
                            delay 0 do
                                evenNewerWidth <- getWidth
                                evenNewerHeight <- getHeight

                                if evenNewerWidth == (fst old) && evenNewerHeight == (snd old)
                                    then pure unit
                                    else send mbox.address (Tuple evenNewerWidth evenNewerHeight)


    liftEff $
        addEventListener resize listener false (windowToEventTarget win)

    pure
        { dimensions: mbox.signal
        , node: node
        }


-- | Setup window signals.
setupWindow :: ∀ e m a.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m) =>
    HTMLElement -> WindowCallback m a -> GraphState m a

setupWindow node cb =
    makeWindowState (Just node) >>= runReaderT cb


-- | Setup window signals.
setupGlobalWindow :: ∀ e m a.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m) =>
    WindowCallback m a -> GraphState m a

setupGlobalWindow cb = do
    makeWindowState Nothing >>= runReaderT cb


-- | The current width and height of the window (i.e. the area viewable to the
-- | user, not including scroll bars).
dimensions :: ∀ e m. (MonadEff (ref :: REF | e) m) => WindowCallback m (Signal (Tuple Int Int))
dimensions = asks _.dimensions


-- | The current width of the window.
width :: ∀ e m. (MonadEff (ref :: REF | e) m) => WindowCallback m (Signal Int)
width = dimensions >>= lift <<< map fst


-- | The current height of the window.
height :: ∀ e m. (MonadEff (ref :: REF | e) m) => WindowCallback m (Signal Int)
height = dimensions >>= lift <<< map snd
