module Examples.Graphics.UpdateElement where


import Elm.Graphics.Element
import Elm.Text (fromString)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.ParentNode (firstElementChild) as ParentNode
import DOM.Node.Types (elementToNode, elementToParentNode, ElementId(..))
import DOM.Node.Node (appendChild)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (keydown)
import DOM.HTML.Types (htmlDocumentToEventTarget)
import Prelude (bind, Unit, unit, (>>=), ($), (>>>), pure)
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.List (List(..), (:))


main :: ∀ e. Eff (dom :: DOM, ref :: REF | e) Unit
main = do
    doc <-
        window >>= document

    nullableContainer <-
        getElementById (ElementId "contents") (htmlDocumentToNonElementParentNode doc)

    for_ (toMaybe nullableContainer) \container -> do
        element <- render scene1
        appendChild (elementToNode element) (elementToNode container)

        remainingScenes <- newRef scenes

        let
            listener =
                eventListener \event -> do
                    s <- readRef remainingScenes

                    -- We have to get the first child every time, as it will change!
                    nullableChild <- ParentNode.firstElementChild (elementToParentNode container)
                    for_ (toMaybe nullableChild) \child ->
                        case s of
                            Cons old (Cons next rest) -> do
                                updateAndReplace child old next
                                writeRef remainingScenes (Cons next rest)

                            _ ->
                                pure unit

        addEventListener keydown listener false (htmlDocumentToEventTarget doc)


scenes :: List Element
scenes =
    ( scene1 : scene2 : scene3 : scene4 : scene5 : scene6
    : scene7 : scene8 : scene9 : scene10 : scene11 : scene12
    : scene13 : scene14 : scene15 : scene16 : scene17
    : Nil
    )


scene1 :: Element
scene1 =
    title
        """This is scene 1, a simple element to start us off.
        Press any key to move to the next scene."""


scene2 :: Element
scene2 =
    title
        """Hopefully we have transitioned to scene 2. How did that go?
        Keep pressing a key to move to the next scene."""


scene3 :: Element
scene3 =
    title
        """This is scene 3 and scene 4. So, when you hit a key the first time,
        you shouldn't notice any change. The second time, you should transition
        to scene 5."""


scene4 :: Element
scene4 = scene3


scene5 :: Element
scene5 =
    link "http://apple.com" $
        title
            """As promised, here is scene5. The whole thing should be a link to
            http://apple.com/"""


scene6 :: Element
scene6 =
    title """Now, for scene 6, we'll remove the link ... how did that go?"""


scene7 :: Element
scene7 =
    title """I hope removing the link worked OK."""


blueBox :: Element
blueBox =
    color Elm.Color.blue (spacer 20 20)


title :: String -> Element
title = fromString >>> leftAligned


scene8 :: Element
scene8 =
    flow down
        ( title "Here's a little blue box. I'll change its properties and we'll see what happens."
        : blueBox
        : Nil
        )

scene9 :: Element
scene9 =
    flow down
        ( title "Ha! I didn't actually change any properties this time. Fooled you!"
        : blueBox
        : Nil
        )


scene10 :: Element
scene10 =
    flow down
        ( title "This time I made it wider."
        : width 50 blueBox
        : Nil
        )


scene11 :: Element
scene11 =
    flow down
        ( title "Now I made it taller."
        : height 50 blueBox
        : Nil
        )


scene12 :: Element
scene12 =
    flow down
        ( title "Now, reduced opacity."
        : opacity 0.5 blueBox
        : Nil
        )


scene13 :: Element
scene13 =
    flow down
        ( title "And different color."
        : color Elm.Color.red blueBox
        : Nil
        )


scene14 :: Element
scene14 =
    flow down
        ( title "Give it an ID"
        : tag "blueBox" blueBox
        : Nil
        )


scene15 :: Element
scene15 =
    flow down
        ( title "Make it a link to apple.com"
        : link "http://apple.com" blueBox
        : Nil
        )


scene16 :: Element
scene16 =
    flow down
        ( title "Change the link to purescript.org"
        : link "http://purescript.org" blueBox
        : Nil
        )


scene17 :: Element
scene17 =
    flow down
        ( title "And just a blue box again"
        : blueBox
        : Nil
        )