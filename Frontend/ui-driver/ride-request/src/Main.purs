module Main where

import Effect.Uncurried
import Prelude

import Effect (Effect)
import Effect.Aff (forkAff, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Flow (startOverlay)
import Helpers.Commons (bootDriverInParallel)
import Helpers.Commons (flowRunner)
import Types (defaultOverlayData)

main :: Effect Unit
main = void $ launchAff $ flowRunner defaultOverlayData $ startOverlay

initiateDriverMapp :: Effect Unit
initiateDriverMapp = launchAff_ $ void $ forkAff $ liftEffect $ runEffectFn1 bootDriverInParallel ""