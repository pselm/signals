module Test.Internal.Graphics where


import DOM (DOM)
import DOM.Node.Types (Element)
import Control.Monad.Eff (Eff)


foreign import innerHtml :: ∀ e. Element -> Eff (dom :: DOM | e) String

foreign import outerHtml :: ∀ e. Element -> Eff (dom :: DOM | e) String
