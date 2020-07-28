{-# LANGUAGE OverloadedLabels #-}

module Product.Update where

import App.Types
import Beckn.Types.API.Update
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Tracking
import Beckn.Types.Mobility.Trip
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Models.ProductInstance as MPI
import qualified Types.ProductInfo as ProdInfo

onUpdate :: OnUpdateReq -> FlowHandler AckResponse
onUpdate req = withFlowHandler $ do
  -- TODO: Verify api key here
  L.logInfo "on_update req" (show req)
  let trip = req ^. #message . #order . #_trip
      pid = ProductInstanceId $ req ^. #message . #order . #_id
      tracker = flip ProdInfo.Tracker Nothing <$> trip
  prdInst <- MPI.findById pid
  let mprdInfo = decodeFromText =<< (prdInst ^. #_info)
      uInfo = getUpdatedProdInfo trip mprdInfo (ProdInfo._tracking =<< ProdInfo._tracker =<< mprdInfo)
      uPrd =
        prdInst
          { SPI._info = encodeToText <$> uInfo
          }
  productInstance <- MPI.findById pid -- TODO: can have multiple cases linked, fix this
  MPI.updateMultiple pid uPrd
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing
  where
    getUpdatedProdInfo :: Maybe Trip -> Maybe ProdInfo.ProductInfo -> Maybe Tracking -> Maybe ProdInfo.ProductInfo
    getUpdatedProdInfo (Just trip) (Just prdInfo) mtracking =
      let utracker = ProdInfo.Tracker {_trip = trip, _tracking = mtracking}
       in Just $ prdInfo {ProdInfo._tracker = Just utracker}
    getUpdatedProdInfo _ _ _ = Nothing
