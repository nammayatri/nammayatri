module Flow where

import Prelude
import Api.Types (NearBySearchRequestRes(..))
import Data.Array (elem, foldM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Uncurried (runEffectFn1, runEffectFn3)
import Foreign (unsafeToForeign)
import Helpers.Commons (bootDriverInParallel, emitEvent, liftFlow, openDriverApp, waitTillDriverAppBoot)
import Presto.Core.Types.Language.Flow (Flow, doAff, fork, loadS, modifyState)
import PrestoDOM (initUIWithNameSpace)
import Screens.RideRequestPopUp.Handler (rideRequestPopUp)
import Screens.RideRequestPopUp.TransFormer (toPopupProp)
import Services.Backend (nearBySearchRequest)
import Types (OverlayData(..))

startOverlay :: Flow OverlayData Unit
startOverlay = do
  void $ liftFlow $ runEffectFn1 bootDriverInParallel ""
  regToken <- loadS "REGISTERATION_TOKEN"
  if validateToken regToken then do
    void $ liftFlow $ initUIWithNameSpace "RideRequestPopUp" Nothing
    void $ fork $ liftFlow $ runEffectFn3 emitEvent "java" "onEvent" (unsafeToForeign { event: "start_location_updates" })
    eiResp <- nearBySearchRequest
    case eiResp of
      Right (NearBySearchRequestRes resp) -> do
        let
          _ = spy "Response ->" resp
        void $ modifyState \(OverlayData oState) -> OverlayData oState { rideRequestPopUpScreen { holderData = toPopupProp resp.searchRequestsForDriver } }
        void $ rideRequestPopUp
        openDriverMicroApp
      Left _ -> openDriverMicroApp
  else
    openDriverMicroApp
  where
  validateToken mbToken = case mbToken of
    Nothing -> false
    Just token -> not $ elem token [ "__failed", "(null)" ]

  openDriverMicroApp = do
    void $ doAff $ makeAff \cb -> runEffectFn1 waitTillDriverAppBoot (cb <<< Right) $> nonCanceler
    void $ liftFlow $ runEffectFn1 openDriverApp ""
