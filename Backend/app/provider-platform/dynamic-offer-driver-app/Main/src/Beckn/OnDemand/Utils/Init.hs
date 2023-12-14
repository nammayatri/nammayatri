module Beckn.OnDemand.Utils.Init where

import Beckn.ACL.Common (getTagV2)
import qualified BecknV2.OnDemand.Types as Spec
import Data.Text as T
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM (PaymentCollector (..), PaymentInstrument (..), PaymentMethodInfo (..), PaymentType (..))
import qualified Domain.Types.Vehicle.Variant as VehVar
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (fromMaybeM)

castVehicleVariant :: MonadFlow m => Text -> m VehVar.Variant
castVehicleVariant = \case
  "SEDAN" -> return VehVar.SEDAN
  "SUV" -> return VehVar.SUV
  "HATCHBACK" -> return VehVar.HATCHBACK
  "AUTO_RICKSHAW" -> return VehVar.AUTO_RICKSHAW
  "TAXI" -> return VehVar.TAXI
  "TAXI_PLUS" -> return VehVar.TAXI_PLUS
  _ -> throwM $ InvalidRequest "Unknown vehicle variant"

castPaymentType :: MonadFlow m => Text -> m DMPM.PaymentType
castPaymentType "ON_ORDER" = return DMPM.PREPAID
castPaymentType "ON_FULFILLMENT" = return DMPM.POSTPAID
castPaymentType _ = throwM $ InvalidRequest "Unknown Payment Type"

castPaymentCollector :: MonadFlow m => Text -> m DMPM.PaymentCollector
castPaymentCollector "BAP" = return DMPM.BAP
castPaymentCollector "BPP" = return DMPM.BPP
castPaymentCollector _ = throwM $ InvalidRequest "Unknown Payment Collector"

castPaymentInstrument :: MonadFlow m => Spec.PaymentParams -> m DMPM.PaymentInstrument
castPaymentInstrument params = do
  if isJust $ params.paymentParamsVirtualPaymentAddress
    then return $ DMPM.UPI
    else return DMPM.Cash -- TODO: add other payment instruments supported by ONDC

buildInitTypeReq :: MonadFlow m => Text -> m DInit.InitTypeReq
buildInitTypeReq = \case
  "RIDE_OTP" -> return DInit.InitSpecialZoneReq
  "RIDE" -> return DInit.InitNormalReq
  _ -> throwM $ InvalidRequest "Unknown init type"

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
