module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff)
import Flow (startOverlay)
import Helpers.Commons (flowRunner)
import Types (defaultOverlayData)

main :: Effect Unit
main = void $ launchAff $ flowRunner defaultOverlayData $ startOverlay
