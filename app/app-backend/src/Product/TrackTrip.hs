{-# LANGUAGE OverloadedLabels #-}

module Product.TrackTrip where

import App.Types
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Track as API
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Tracking
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Organization as OQ
import Types.API.Track
import Types.Error
import Types.ProductInfo as ProductInfo
import Utils.Common
import qualified Utils.Notifications as Notify

track :: Person.Person -> TrackTripReq -> FlowHandler TrackTripRes
track person req = withFlowHandlerAPI $ do
  let prodInstId = req ^. #rideId
  prodInst <- MPI.findById prodInstId
  case_ <- MC.findIdByPerson person (prodInst ^. #caseId)
  let txnId = getId $ case_ ^. #id
  context <- buildContext "feedback" txnId Nothing Nothing
  organization <-
    OQ.findOrganizationById (prodInst ^. #organizationId)
      >>= fromMaybeM OrgNotFound
  (info :: ProductInfo) <-
    (decodeFromText =<< (prodInst ^. #info))
      & fromMaybeM (PIFieldNotPresent "info")
  tracker <- info ^. #tracker & fromMaybeM (InternalError "PI.info has no tracker field")
  let gTripId = tracker ^. #trip . #id
  gatewayUrl <- organization ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  ExternalAPI.track gatewayUrl (API.TrackTripReq context $ API.TrackReqMessage gTripId Nothing)
  return Success

trackCb :: Organization.Organization -> API.OnTrackTripReq -> FlowHandler API.OnTrackTripRes
trackCb _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_track" $ req ^. #context
    let context = req ^. #context
    case req ^. #contents of
      Right msg -> do
        let tracking = msg ^. #tracking
            caseId = Id $ context ^. #transaction_id
        case_ <- MC.findById caseId
        prodInst <- MPI.listAllProductInstance (ProductInstance.ByApplicationId caseId) [ProductInstance.CONFIRMED]
        let confirmedProducts = prodInst
        res <-
          case length confirmedProducts of
            0 -> return $ Right ()
            1 -> do
              let productInst = head confirmedProducts
                  personId = Case.requestor case_
              orderPi <- MPI.findByParentIdType (productInst ^. #id) Case.RIDEORDER
              mtracker <- updateTracker orderPi tracking
              whenJust mtracker (\t -> Notify.notifyOnTrackCb personId t case_)
              return $ Right ()
            _ -> return $ Left "Multiple products confirmed, ambiguous selection"
        res & fromEitherM InvalidRequest
        return Ack
      Left err -> do
        logTagError "on_track_trip req" $ "on_track_trip error: " <> show err
        return Ack

updateTracker :: ProductInstance.ProductInstance -> Maybe Tracking -> Flow (Maybe Tracker)
updateTracker prodInst mtracking = do
  let minfo = decodeFromText =<< prodInst ^. #info
  case minfo of
    Nothing -> return Nothing
    Just info -> do
      let mtracker = updTracker info mtracking
          uInfo = info {ProductInfo.tracker = mtracker}
          updatedPrd = prodInst {ProductInstance.info = Just $ encodeToText uInfo}
      MPI.updateMultiple (prodInst ^. #id) updatedPrd
      return mtracker
  where
    updTracker info tracking =
      case info ^. #tracker of
        Just tracker -> Just (Tracker (tracker ^. #trip) $ fromBeckn <$> tracking)
        Nothing -> Nothing
