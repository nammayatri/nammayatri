{-# LANGUAGE OverloadedLabels #-}

module Product.TrackTrip where

import App.Types
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Error
import Beckn.Types.Core.Tracking
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import Types.Common (fromBeckn)
import Types.ProductInfo as ProductInfo
import qualified Utils.Notifications as Notify

track :: Person.Person -> TrackTripReq -> FlowHandler TrackTripRes
track _ req = withFlowHandler $ do
  let context = req ^. #context
      prodInstId = req ^. #message . #order . #id
  prdInst <- MPI.findById $ ProductInstanceId prodInstId
  case decodeFromText =<< (prdInst ^. #_info) of
    Nothing -> return $ AckResponse context (ack "NACK") $ Just $ domainError "No product to track"
    Just (info :: ProductInfo) ->
      case ProductInfo._tracker info of
        Nothing -> return $ AckResponse context (ack "NACK") $ Just $ domainError "No product to track"
        Just tracker -> do
          let gTripId = tracker ^. #_trip . #id
          gatewayUrl <- Gateway.getProviderBaseUrl
          Gateway.track gatewayUrl $ req & ((#message . #tracking . #id) .~ gTripId)

trackCb :: OnTrackTripReq -> FlowHandler OnTrackTripRes
trackCb req = withFlowHandler $ do
  -- TODO: verify api key
  let context = req ^. #context
      tracking = req ^. #message . #tracking
      caseId = CaseId $ req ^. #context . #_transaction_id
  case_ <- MC.findById caseId
  prodInst <- MPI.listAllProductInstance (ByApplicationId caseId) [ProductInstance.CONFIRMED]
  let confirmedProducts = prodInst
  res <-
    case length confirmedProducts of
      0 -> return $ Right ()
      1 -> do
        let productInst = head confirmedProducts
            personId = Case._requestor case_
        mtracker <- updateTracker productInst tracking
        whenJust mtracker (\t -> Notify.notifyOnTrackCb personId t case_)
        return $ Right ()
      _ -> return $ Left "Multiple products confirmed, ambiguous selection"
  case res of
    Left err -> return $ AckResponse context (ack "NACK") $ Just $ domainError err
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

updateTracker :: ProductInstance.ProductInstance -> Maybe Tracking -> Flow (Maybe Tracker)
updateTracker prodInst mtracking = do
  let minfo = decodeFromText =<< prodInst ^. #_info
  case minfo of
    Nothing -> return Nothing
    Just info -> do
      let mtracker = updTracker info mtracking
          uInfo = info {ProductInfo._tracker = mtracker}
          updatedPrd = prodInst {ProductInstance._info = Just $ encodeToText uInfo}
      MPI.updateMultiple (prodInst ^. #_id) updatedPrd
      return mtracker
  where
    updTracker info tracking =
      case info ^. #_tracker of
        Just tracker -> Just (Tracker (tracker ^. #_trip) $ fromBeckn <$> tracking)
        Nothing -> Nothing
