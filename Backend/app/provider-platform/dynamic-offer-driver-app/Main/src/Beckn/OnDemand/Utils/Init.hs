module Beckn.OnDemand.Utils.Init where

import qualified BecknV2.OnDemand.Enums as Spec
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import Data.Text as T
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (decodeFromText, fromMaybeM)

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
mkPaymentMethodInfo Spec.Payment {..}
  | isStripePayment =
    -- only this payment method supported for Stripe
    return $ Just $ DMPM.PaymentMethodInfo {paymentInstrument = DMPM.Card DMPM.DefaultCardType, collectedBy = DMPM.BAP, paymentType = DMPM.ON_FULFILLMENT}
  where
    isStripePayment = (paymentTlMethod >>= (\method -> decodeFromText $ "\"" <> method <> "\"")) == Just Spec.StripeSdk
mkPaymentMethodInfo Spec.Payment {..} = do
  _params <- paymentParams & fromMaybeM (InvalidRequest "Payment Params not found")
  collectedBy <- paymentCollectedBy & fromMaybeM (InvalidRequest "Payment Params not found") >>= castPaymentCollector
  pType <- fmap (fromMaybe DMPM.ON_FULFILLMENT . decodeFromText) (paymentType & fromMaybeM (InvalidRequest "Payment Params not found"))
  paymentInstrument <- castPaymentInstrument _params
  return $ Just $ DMPM.PaymentMethodInfo {paymentType = pType, ..}

mkPaymentMode :: Spec.Payment -> Maybe DMPM.PaymentMode
mkPaymentMode Spec.Payment {paymentTags} = do
  isTestMode <- readMaybe . T.unpack =<< Utils.getTagV2 Tag.SETTLEMENT_TERMS Tag.STRIPE_TEST paymentTags
  pure $ if isTestMode then DMPM.TEST else DMPM.LIVE
