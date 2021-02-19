{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Update where

import App.Types
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Tracking
import Beckn.Types.Mobility.Trip
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import Beckn.Utils.Logging (Log (..))
import EulerHS.Prelude
import qualified Models.ProductInstance as MPI
import qualified Types.ProductInfo as ProdInfo
import Utils.Common (validateContext)

onUpdate :: Organization.Organization -> OnUpdateReq -> FlowHandler AckResponse
onUpdate _org req = withFlowHandler $ do
  -- TODO: Verify api key here
  logInfo "on_update req" (show req)
  validateContext "on_update" $ req ^. #context
  case req ^. #contents of
    Right msg -> do
      let trip = msg ^. #order . #_trip
          pid = ProductInstanceId $ msg ^. #order . #_id
      orderPi <- MPI.findByParentIdType (Just pid) Case.RIDEORDER
      let mprdInfo = decodeFromText =<< (orderPi ^. #_info)
          uInfo = getUpdatedProdInfo trip mprdInfo $ toBeckn <$> (ProdInfo._tracking =<< ProdInfo._tracker =<< mprdInfo)
          uPrd =
            orderPi
              { SPI._info = encodeToText <$> uInfo
              }
      MPI.updateMultiple (orderPi ^. #_id) uPrd
    Left err -> logError "on_update req" $ "on_update error: " <> show err
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing
  where
    getUpdatedProdInfo :: Maybe Trip -> Maybe ProdInfo.ProductInfo -> Maybe Tracking -> Maybe ProdInfo.ProductInfo
    getUpdatedProdInfo (Just trip) (Just prdInfo) mtracking =
      let utracker = ProdInfo.Tracker {_trip = fromBeckn trip, _tracking = fromBeckn <$> mtracking}
       in Just $ prdInfo {ProdInfo._tracker = Just utracker}
    getUpdatedProdInfo _ _ _ = Nothing
