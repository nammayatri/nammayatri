module Main where

import Effect.Uncurried
import Prelude

import Api.Types (NearBySearchRequestRes(..), OfferType(..), SearchRequest(..))
import Data.Either (Either(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (forkAff, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Flow (startOverlay)
import Helpers.Commons (bootDriverInParallel)
import Helpers.Commons (flowRunner, liftFlow)
import Services.Backend (mkQuoteOffer, nearBySearchRequest, quoteOfferApi)
import Types (defaultOverlayData)

main :: String -> Effect Unit
main parent = void $ launchAff $ flowRunner defaultOverlayData $ startOverlay parent


checkAndPushRideRequest :: (Array SearchRequest-> Effect Unit) -> (String -> String -> Effect Unit) -> String -> String -> Effect Unit
checkAndPushRideRequest cb nCb nType id 
  | nType == "CLEARED_FARE" || nType == "CANCELLED_SEARCH_REQUEST" = nCb nType id
  | otherwise = void $ launchAff $ flowRunner defaultOverlayData $ do
                      eiResp <- nearBySearchRequest id
                      case eiResp of
                        Right (NearBySearchRequestRes resp) -> do 
                          liftFlow $ cb resp.searchRequestsForDriver
                        Left err -> do
                          let _ = spy "Left err ->" err
                          pure unit
 

initiateDriverMapp :: Effect Unit
initiateDriverMapp = launchAff_ $ void $ forkAff $ liftEffect $ runEffectFn1 bootDriverInParallel ""

callDecline :: String -> Effect Unit
callDecline id = void $ launchAff $ flowRunner defaultOverlayData $ quoteOfferApi $ mkQuoteOffer id Decline