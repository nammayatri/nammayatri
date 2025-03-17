module Beckn.OnDemand.Utils.Init where

import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import Data.Text as T
import qualified Domain.Types.MerchantPaymentMethod as DMPM (PaymentCollector (..), PaymentInstrument (..), PaymentMethodInfo (..), PaymentType (..))
import qualified Domain.Types.VehicleVariant as VehVar
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (decodeFromText, fromMaybeM)

castVehicleVariant :: Maybe Text -> Maybe Text -> Maybe VehVar.VehicleVariant
castVehicleVariant mbVehCategory mbVehVariant = case (mbVehCategory, mbVehVariant) of
  (Just "CAB", Just "SEDAN") -> Just VehVar.SEDAN
  (Just "CAB", Just "SUV") -> Just VehVar.SUV
  (Just "CAB", Just "SUV_PLUS") -> Just VehVar.SUV_PLUS
  (Just "CAB", Just "HERITAGE_CAB") -> Just VehVar.HERITAGE_CAB
  (Just "CAB", Just "HATCHBACK") -> Just VehVar.HATCHBACK
  (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just VehVar.AUTO_RICKSHAW
  (Just "AUTO_RICKSHAW", Just "EV_AUTO_RICKSHAW") -> Just VehVar.EV_AUTO_RICKSHAW
  (Just "CAB", Just "TAXI") -> Just VehVar.TAXI
  (Just "CAB", Just "TAXI_PLUS") -> Just VehVar.TAXI_PLUS
  (Just "MOTORCYCLE", Just "BIKE") -> Just VehVar.BIKE -- becomes redundant, TODO : remove in next release
  (Just "MOTORCYCLE", Just "DELIVERY_BIKE") -> Just VehVar.DELIVERY_BIKE -- becomes redundant, TODO : remove in next release
  (Just "TWO_WHEELER", Just "BIKE") -> Just VehVar.BIKE
  (Just "TWO_WHEELER", Just "DELIVERY_BIKE") -> Just VehVar.DELIVERY_BIKE
  (Just "AMBULANCE", Just "AMBULANCE_TAXI") -> Just VehVar.AMBULANCE_TAXI
  (Just "AMBULANCE", Just "AMBULANCE_TAXI_OXY") -> Just VehVar.AMBULANCE_TAXI_OXY
  (Just "AMBULANCE", Just "AMBULANCE_AC") -> Just VehVar.AMBULANCE_AC
  (Just "AMBULANCE", Just "AMBULANCE_AC_OXY") -> Just VehVar.AMBULANCE_AC_OXY
  (Just "AMBULANCE", Just "AMBULANCE_VENTILATOR") -> Just VehVar.AMBULANCE_VENTILATOR
  (Just "TRUCK", Just "DELIVERY_LIGHT_GOODS_VEHICLE") -> Just VehVar.DELIVERY_LIGHT_GOODS_VEHICLE
  (Just "TRUCK", Just "DELIVERY_TRUCK_MINI") -> Just VehVar.DELIVERY_TRUCK_MINI
  (Just "TRUCK", Just "DELIVERY_TRUCK_SMALL") -> Just VehVar.DELIVERY_TRUCK_SMALL
  (Just "TRUCK", Just "DELIVERY_TRUCK_MEDIUM") -> Just VehVar.DELIVERY_TRUCK_MEDIUM
  (Just "TRUCK", Just "DELIVERY_TRUCK_LARGE") -> Just VehVar.DELIVERY_TRUCK_LARGE
  (Just "TRUCK", Just "DELIVERY_TRUCK_ULTRA_LARGE") -> Just VehVar.DELIVERY_TRUCK_ULTRA_LARGE
  (Just "BUS", Just "BUS_NON_AC") -> Just VehVar.BUS_NON_AC
  (Just "BUS", Just "BUS_AC") -> Just VehVar.BUS_AC
  _ -> Nothing

castPaymentCollector :: MonadFlow m => Text -> m DMPM.PaymentCollector
castPaymentCollector "BAP" = return DMPM.BAP
castPaymentCollector "BPP" = return DMPM.BPP
castPaymentCollector _ = throwM $ InvalidRequest "Unknown Payment Collector"

castPaymentInstrument :: MonadFlow m => Spec.PaymentParams -> m DMPM.PaymentInstrument
castPaymentInstrument params = do
  if isJust $ params.paymentParamsVirtualPaymentAddress
    then return DMPM.UPI
    else return DMPM.Cash -- TODO: add other payment instruments supported by ONDC

getMaxEstimateDistance :: [Spec.TagGroup] -> Maybe HighPrecMeters
getMaxEstimateDistance tagGroups = do
  tagValue <- Utils.getTagV2 Tag.ESTIMATIONS Tag.MAX_ESTIMATED_DISTANCE (Just tagGroups)
  maxEstimatedDistance <- readMaybe $ T.unpack tagValue
  Just $ HighPrecMeters maxEstimatedDistance

mkPaymentMethodInfo :: MonadFlow m => Spec.Payment -> m (Maybe DMPM.PaymentMethodInfo)
mkPaymentMethodInfo Spec.Payment {..} = do
  _params <- paymentParams & fromMaybeM (InvalidRequest "Payment Params not found")
  collectedBy <- paymentCollectedBy & fromMaybeM (InvalidRequest "Payment Params not found") >>= castPaymentCollector
  pType <- fmap (fromMaybe DMPM.ON_FULFILLMENT . decodeFromText) (paymentType & fromMaybeM (InvalidRequest "Payment Params not found"))
  paymentInstrument <- castPaymentInstrument _params
  return $ Just $ DMPM.PaymentMethodInfo {paymentType = pType, ..}
