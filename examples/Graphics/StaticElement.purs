module Examples.Graphics.StaticElement where


import Elm.Graphics.Element
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..))
import DOM.Node.Node (appendChild)
import Prelude (bind, Unit, (>>=))
import Data.Nullable (toMaybe)
import Data.Foldable (for_)


main :: ∀ e. Eff (dom :: DOM | e) Unit
main = do
    doc <-
        window >>= document

    nullableContainer <-
        getElementById (ElementId "contents") (htmlDocumentToNonElementParentNode doc)

    for_ (toMaybe nullableContainer) \container -> do
        element <- render scene
        appendChild (elementToNode element) (elementToNode container)


scene :: Element
scene =
    show "Hello World!"
