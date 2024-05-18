module Main where

import Effect.Uncurried
import Prelude

import Api.Types (NearBySearchRequestRes(..), SearchRequest(..))
import Data.Either (Either(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (forkAff, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Flow (startOverlay)
import Helpers.Commons (bootDriverInParallel)
import Helpers.Commons (flowRunner, liftFlow)
import Services.Backend (nearBySearchRequest)
import Types (defaultOverlayData)

main :: Effect Unit
main = void $ launchAff $ flowRunner defaultOverlayData $ startOverlay


checkAndPushRideRequest :: (Array SearchRequest-> Effect Unit) -> (String -> String -> Effect Unit) -> String -> String -> Effect Unit
checkAndPushRideRequest cb nCb nType id 
  | nType == "CLEARED_FARE" || nType == "CANCELLED_SEARCH_REQUEST" = nCb nType id
  | otherwise = void $ launchAff $ flowRunner defaultOverlayData $ do
                      let _ = spy "checkAndPushRideRequest" id
                      eiResp <- nearBySearchRequest id
                      case eiResp of
                        Right (NearBySearchRequestRes resp) -> do 
                          liftFlow $ cb resp.searchRequestsForDriver
                        Left err -> do
                          let _ = spy "Left err ->" err
                          pure unit
 

initiateDriverMapp :: Effect Unit
initiateDriverMapp = launchAff_ $ void $ forkAff $ liftEffect $ runEffectFn1 bootDriverInParallel ""