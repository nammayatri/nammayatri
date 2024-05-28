module Flow where

import Prelude

import Api.Types (NearBySearchRequestRes(..), OfferType(..), SearchRequest(..))
import Control.Monad.Trans.Class (lift)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Uncurried (runEffectFn1, runEffectFn3)
import Foreign (unsafeToForeign)
import Helpers.Commons (bootDriverInParallel, emitEvent, getSearchRequestId, liftFlow, openDriverApp, waitTillDriverAppBoot, isDriverAppBooted)
import Presto.Core.Types.Language.Flow (Flow, await, doAff, fork, loadS, modifyState)
import PrestoDOM (initUIWithNameSpace, toast)
import PrestoDOM.Core (terminateUI)
import Screens.RideRequestPopUp.Controller (ScreenOutput(..))
import Screens.RideRequestPopUp.Handler (rideRequestPopUp)
import Screens.RideRequestPopUp.TransFormer (toPopupProp)
import Screens.DriverApp.Handler (renderDriverApp)
import Services.Backend (mkQuoteOffer, nearBySearchRequest, quoteOfferApi)
import Types (OverlayData(..))

startOverlay :: String -> Flow OverlayData Unit
startOverlay parent = do
  regToken <- loadS "REGISTERATION_TOKEN"
  if validateToken regToken then do
    when (parent == "java" && (not $ isDriverAppBooted "")) $ renderDriverApp
    void $ liftFlow $ initUIWithNameSpace "RideRequestPopUp" Nothing
    void $ liftFlow $ initUIWithNameSpace "TopPriceView" Nothing
    void $ fork $ liftFlow $ runEffectFn3 emitEvent "java" "onEvent" (unsafeToForeign { event: "start_location_updates" })
    getSearchId <- liftFlow $ runEffectFn1 getSearchRequestId ""
    eiRespWithId <- nearBySearchRequest getSearchId
    case eiRespWithId of
      Right (NearBySearchRequestRes resp) -> if DA.null resp.searchRequestsForDriver then checkFullList else showPopUp resp.searchRequestsForDriver
      Left _ -> checkFullList
  else
    pure unit
  where
  validateToken mbToken = case mbToken of
    Nothing -> false
    Just token -> not $ DA.elem token [ "__failed", "(null)" ]
  checkFullList  =  do
    eiResp <- nearBySearchRequest ""
    case eiResp of
      Right (NearBySearchRequestRes resp) -> if DA.null resp.searchRequestsForDriver then pure unit else showPopUp resp.searchRequestsForDriver
      Left _ -> pure unit
  showPopUp :: Array SearchRequest -> Flow OverlayData Unit
  showPopUp list = do 
    void $ modifyState \(OverlayData oState) -> OverlayData oState { rideRequestPopUpScreen { holderData = toPopupProp list , rideRequests = list} }
    out <- rideRequestPopUp
    popupOutputHandler out
    void $ liftFlow $ terminateUI $ Just "TopPriceView"
    void $ liftFlow $ terminateUI $ Just "RideRequestPopUp"

popupOutputHandler :: ScreenOutput -> Flow OverlayData Unit
popupOutputHandler out = 
  case out of
    AcceptRequest (SearchRequest item) -> void $ quoteOfferApi $ mkQuoteOffer item.searchTryId Accept
    _ -> pure unit
