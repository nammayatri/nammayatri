module Beckn.OnDemand.Utils.Init where

import Beckn.ACL.Common (getTagV2)
import qualified BecknV2.OnDemand.Types as Spec
import Data.Text as T
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM (PaymentCollector (..), PaymentInstrument (..), PaymentMethodInfo (..), PaymentType (..))
import qualified Domain.Types.Vehicle.Variant as VehVar
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (fromMaybeM)

castVehicleVariant :: Maybe Text -> Maybe Text -> Maybe VehVar.Variant
castVehicleVariant mbVehCategory mbVehVariant = case (mbVehCategory, mbVehVariant) of
  (Just "CAB", Just "SEDAN") -> Just VehVar.SEDAN
  (Just "CAB", Just "SUV") -> Just VehVar.SUV
  (Just "CAB", Just "HATCHBACK") -> Just VehVar.HATCHBACK
  (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just VehVar.AUTO_RICKSHAW
  (Just "CAB", Just "TAXI") -> Just VehVar.TAXI
  (Just "CAB", Just "TAXI_PLUS") -> Just VehVar.TAXI_PLUS
  _ -> Nothing

castPaymentType :: MonadFlow m => Text -> m DMPM.PaymentType
castPaymentType "ON_ORDER" = return DMPM.PREPAID -- TODO::Beckn, this is not present in spec
castPaymentType "ON_FULFILLMENT" = return DMPM.POSTPAID
castPaymentType _ = throwM $ InvalidRequest "Unknown Payment Type"

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
  tagValue <- getTagV2 "estimations" "max_estimated_distance" tagGroups
  maxEstimatedDistance <- readMaybe $ T.unpack tagValue
  Just $ HighPrecMeters maxEstimatedDistance

mkPaymentMethodInfo :: MonadFlow m => Spec.Payment -> m (Maybe DMPM.PaymentMethodInfo)
mkPaymentMethodInfo Spec.Payment {..} = do
  _params <- paymentParams & fromMaybeM (InvalidRequest "Payment Params not found")
  collectedBy <- paymentCollectedBy & fromMaybeM (InvalidRequest "Payment Params not found") >>= castPaymentCollector
  pType <- paymentType & fromMaybeM (InvalidRequest "Payment Params not found") >>= castPaymentType
  paymentInstrument <- castPaymentInstrument _params
  return $ Just $ DMPM.PaymentMethodInfo {paymentType = pType, ..}
