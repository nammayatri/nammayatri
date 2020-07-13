{-# LANGUAGE OverloadedLabels #-}

module Product.TrackTrip where

import App.Types
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import Types.ProductInfo as ProductInfo
import qualified Utils.Notifications as Notify

track :: Person.Person -> TrackTripReq -> FlowHandler TrackTripRes
track _ req = withFlowHandler $ do
  let context = req ^. #context
      tripId = req ^. #message . #id
  prdInst <- ProductInstance.findById $ ProductInstanceId tripId
  ack <-
    case decodeFromText =<< (prdInst ^. #_info) of
      Nothing -> return $ Ack "Error" "No product to track"
      Just (info :: ProductInfo) ->
        case ProductInfo._tracker info of
          Nothing -> return $ Ack "Error" "No product to track"
          Just tracker -> do
            let gTripId = tracker ^. #trip . #id
            gatewayUrl <- Gateway.getBaseUrl
            eres <- Gateway.track gatewayUrl $ req & #message . #id .~ gTripId
            case eres of
              Left err -> return $ Ack "Error" (show err)
              Right _ -> return $ Ack "Successful" "Tracking initiated"
  return $ AckResponse context ack

trackCb :: OnTrackTripReq -> FlowHandler OnTrackTripRes
trackCb req = withFlowHandler $ do
  -- TODO: verify api key
  let context = req ^. #context
      tracking = req ^. #message
      caseId = CaseId $ req ^. #context . #transaction_id
  case_ <- Case.findById caseId
  pi <- ProductInstance.listAllProductInstance (ProductInstance.ByApplicationId caseId) [ProductInstance.CONFIRMED]
  let confirmedProducts = pi
  res <-
    case length confirmedProducts of
      0 -> return $ Right ()
      1 -> do
        let productInst = head confirmedProducts
            personId = Case._requestor case_
        Notify.notifyOnTrackCb personId tracking case_
        updateTracker productInst tracking
      _ -> return $ Left "Multiple products confirmed, ambiguous selection"
  case res of
    Left err -> return $ AckResponse context (Ack "Error" err)
    Right _ -> return $ AckResponse context (Ack "Successful" "Ok")

updateTracker :: ProductInstance.ProductInstance -> Tracker -> Flow (Either Text ())
updateTracker prodInst tracker = do
  let minfo = decodeFromText =<< prodInst ^. #_info
  let uInfo = (\info -> info {ProductInfo._tracker = Just tracker}) <$> minfo
  let updatedPrd =
        prodInst {ProductInstance._info = Just $ encodeToText uInfo}
  ProductInstance.updateMultiple (_getProductInstanceId $ prodInst ^. #_id) updatedPrd
  return $ Right ()
