{-# LANGUAGE OverloadedLabels #-}

module Product.TrackTrip where

import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Trip
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Products as Products
import Utils.Common (verifyToken)

track :: Maybe Text -> TrackTripReq -> FlowHandler TrackTripRes
track regToken req = withFlowHandler $ do
  verifyToken regToken
  let context = req ^. #context
      tripId = req ^. #message ^. #id
  prd <- Products.findById $ ProductsId tripId
  ack <-
    case decodeFromText =<< prd ^. #_info of
      Nothing -> return $ Ack "Error" "No product to track"
      Just (tracker :: Tracker) -> do
        let gTripId = tracker ^. #trip ^. #id
        gatewayUrl <- Gateway.getBaseUrl
        eres <- Gateway.track gatewayUrl $ req & (#message . #id) .~ gTripId
        case eres of
          Left err -> return $ Ack "Error" (show err)
          Right _ -> return $ Ack "Successful" "Tracking initiated"
  return $ AckResponse context ack

track_cb :: Maybe Text -> OnTrackTripReq -> FlowHandler OnTrackTripRes
track_cb apiKey req = withFlowHandler $ do
  -- TODO: verify api key
  let context = req ^. #context
      tracking = req ^. #message
      caseId = CaseId $ req ^. #context ^. #transaction_id
  case_ <- Case.findById caseId
  cp <- CaseProduct.findAllByCaseId caseId
  let pids = map CaseProduct._productId cp
  products <- Products.findAllByIds pids
  let confirmedProducts = filter (\prd -> Products.CONFIRMED == Products._status prd) products

  res <-
    case length confirmedProducts of
      0 -> return $ Right ()
      1 -> do
        let product = head confirmedProducts
        updateTracker product tracking
      _ -> return $ Left "Multiple products confirmed, ambiguous selection"

  case res of
    Left err -> return $ AckResponse context (Ack "Error" err)
    Right _ -> return $ AckResponse context (Ack "Successful" "Ok")

updateTracker :: Products.Products -> Tracker -> L.Flow (Either Text ())
updateTracker product tracker = do
  let info = product ^. #_info
  let updatedPrd =
        product {Products._info = Just $ encodeToText tracker}
  Products.updateMultiple (_getProductsId $ product ^. #_id) updatedPrd
  return $ Right ()
