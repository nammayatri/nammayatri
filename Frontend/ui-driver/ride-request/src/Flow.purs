module Flow where

import Prelude

import Api.Types (NearBySearchRequestRes(..))
import Control.Monad.Trans.Class (lift)
import Data.Array (elem, foldM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Uncurried (runEffectFn1, runEffectFn3)
import Foreign (unsafeToForeign)
import Helpers.Commons (bootDriverInParallel, emitEvent, liftFlow, openDriverApp, waitTillDriverAppBoot)
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
    void $ fork $ liftFlow $ runEffectFn3 emitEvent "java" "onEvent" (unsafeToForeign { event: "start_location_updates" })
    eiResp <- nearBySearchRequest
    case eiResp of
      Right (NearBySearchRequestRes resp) -> do
        void $ modifyState \(OverlayData oState) -> OverlayData oState { rideRequestPopUpScreen { holderData = toPopupProp resp.searchRequestsForDriver , rideRequests = resp.searchRequestsForDriver} }
        void $ rideRequestPopUp
        void $ liftFlow $ terminateUI $ Just "RideRequestPopUp"
      Left _ -> pure unit
  else
    pure unit
  await control
  where
  validateToken mbToken = case mbToken of
    Nothing -> false
    Just token -> not $ elem token [ "__failed", "(null)" ]

  loadDriverAppInParallel = do
    pure unit
    -- void $ liftFlow $ runEffectFn1 bootDriverInParallel ""
    -- void $ doAff $ makeAff \cb -> runEffectFn1 waitTillDriverAppBoot (cb <<< Right) $> nonCanceler
    -- void $ liftFlow $ runEffectFn1 openDriverApp ""
