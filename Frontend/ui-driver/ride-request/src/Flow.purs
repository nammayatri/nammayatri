module Flow where

import Prelude

import Api.Types (NearBySearchRequestRes(..), SearchRequest(..))
import Control.Monad.Trans.Class (lift)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Uncurried (runEffectFn1, runEffectFn3)
import Foreign (unsafeToForeign)
import Helpers.Commons (bootDriverInParallel, emitEvent, getSearchRequestId, liftFlow, openDriverApp, waitTillDriverAppBoot)
import Presto.Core.Types.Language.Flow (Flow, await, doAff, fork, loadS, modifyState)
import PrestoDOM (initUIWithNameSpace)
import PrestoDOM.Core (terminateUI)
import Screens.RideRequestPopUp.Handler (rideRequestPopUp)
import Screens.RideRequestPopUp.TransFormer (toPopupProp)
import Services.Backend (nearBySearchRequest)
import Types (OverlayData(..))

startOverlay :: Flow OverlayData Unit
startOverlay = do
  control <- fork $ loadDriverAppInParallel
  regToken <- loadS "REGISTERATION_TOKEN"
  if validateToken regToken then do
    void $ liftFlow $ initUIWithNameSpace "RideRequestPopUp" Nothing
    void $ liftFlow $ initUIWithNameSpace "TopPriceView" Nothing
    void $ fork $ liftFlow $ runEffectFn3 emitEvent "java" "onEvent" (unsafeToForeign { event: "start_location_updates" })
    getSearchId <- liftFlow $ runEffectFn1 getSearchRequestId ""
    eiRespWithId <- nearBySearchRequest getSearchId
    case eiRespWithId of
      Right (NearBySearchRequestRes resp) -> if DA.null resp.searchRequestsForDriver then pure unit else showPopUp resp.searchRequestsForDriver
      Left _ -> pure unit
    eiResp <- nearBySearchRequest ""
    case eiResp of
      Right (NearBySearchRequestRes resp) -> if DA.null resp.searchRequestsForDriver then pure unit else showPopUp resp.searchRequestsForDriver
      Left _ -> pure unit
  else
    pure unit
  await control
  where
  validateToken mbToken = case mbToken of
    Nothing -> false
    Just token -> not $ DA.elem token [ "__failed", "(null)" ]

  loadDriverAppInParallel = do
    pure unit
  showPopUp :: Array SearchRequest -> Flow OverlayData Unit
  showPopUp list = do 
    void $ modifyState \(OverlayData oState) -> OverlayData oState { rideRequestPopUpScreen { holderData = toPopupProp list , rideRequests = list} }
    void $ rideRequestPopUp
    void $ liftFlow $ terminateUI $ Just "TopPriceView"
    void $ liftFlow $ terminateUI $ Just "RideRequestPopUp"
    -- void $ liftFlow $ runEffectFn1 bootDriverInParallel ""
    -- void $ doAff $ makeAff \cb -> runEffectFn1 waitTillDriverAppBoot (cb <<< Right) $> nonCanceler
    -- void $ liftFlow $ runEffectFn1 openDriverApp ""
