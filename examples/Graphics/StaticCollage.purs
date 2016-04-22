module Examples.Graphics.StaticCollage where


import Elm.Graphics.Collage
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Renderable (render)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Node, elementToNode, ElementId(..))
import DOM.Node.Node (appendChild)
import Prelude (bind, Unit, (>>=), ($))
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Graphics.Canvas (Canvas)
import Elm.Color (red)


main :: ∀ e. Eff (canvas :: Canvas, dom :: DOM | e) Unit
main = do
    doc <-
        window >>= document

    nullableContainer <-
        getElementById (ElementId "contents") (htmlDocumentToNonElementParentNode doc)

    for_ (toMaybe nullableContainer) \container ->
        for_ [collage1] \child -> do
            c <- child
            appendChild c (elementToNode container)


collage1 :: ∀ e. Eff (canvas :: Canvas, dom :: DOM | e) Node
collage1 =
    render $
        makeCollage 50 50
            ( filled red (rect 10.0 10.0)
            : Nil
            )





