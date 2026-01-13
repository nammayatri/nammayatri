{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Tools.Payment
  ( module Reexport,
    createOrder,
    updateOrder,
    orderStatus,
    refundOrder,
    PaymentServiceType (..),
    createCustomer,
    getCustomer,
    createEphemeralKeys,
    getCardList,
    createPaymentIntent,
    updatePaymentMethodInIntent,
    capturePaymentIntent,
    -- updateAmountInPaymentIntent,
    createSetupIntent,
    deleteCard,
    getPaymentIntent,
    cancelPaymentIntent,
    verifyVpa,
    VendorSplitDetails (..),
    SplitType (..),
    mkSplitSettlementDetails,
    mkUnaggregatedSplitSettlementDetails,
    mkUnaggregatedRefundSplitSettlementDetails,
    groupSumVendorSplits,
    roundVendorFee,
    getIsSplitEnabled,
    getIsRefundSplitEnabled,
    getIsPercentageSplit,
    roundToTwoDecimalPlaces,
    fetchGatewayReferenceId,
    fetchOfferSKUConfig,
    extractSplitSettlementDetailsAmount,
    getPaymentOrderValidity,
    offerList,
    createRefund,
    getRefund,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.List (groupBy, sortBy, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import Domain.Types.TicketPlace
import qualified EulerHS.Language as L
import Kernel.External.Payment.Interface as Reexport hiding
  ( Wallet,
    autoRefunds,
    cancelPaymentIntent,
    capturePaymentIntent,
    createCustomer,
    createEphemeralKeys,
    createOrder,
    createPaymentIntent,
    createRefund,
    createSetupIntent,
    deleteCard,
    getCardList,
    getCustomer,
    getPaymentIntent,
    getRefund,
    isSplitEnabled,
    offerList,
    orderStatus,
    -- updateAmountInPaymentIntent,
    updateOrder,
    updatePaymentMethodInIntent,
  )
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Juspay.Config as JuspayConfig
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Version
import Lib.Payment.Domain.Types.PaymentOrder
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.PlaceBasedServiceConfig as CQPBSC
import System.Environment as SE

createOrder :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id TicketPlace) -> PaymentServiceType -> Maybe Text -> Maybe Version -> Maybe Bool -> Payment.CreateOrderReq -> m Payment.CreateOrderResp
createOrder = runWithServiceConfigAndServiceName Payment.createOrder

updateOrder :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id TicketPlace) -> PaymentServiceType -> Maybe Text -> Maybe Version -> Maybe Bool -> Payment.OrderUpdateReq -> m Payment.OrderUpdateResp
updateOrder = runWithServiceConfigAndServiceName Payment.updateOrder

orderStatus :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id TicketPlace) -> PaymentServiceType -> Maybe Text -> Maybe Version -> Maybe Bool -> Payment.OrderStatusReq -> m Payment.OrderStatusResp
orderStatus = runWithServiceConfigAndServiceName Payment.orderStatus

offerList :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id TicketPlace) -> PaymentServiceType -> Maybe Text -> Maybe Version -> Payment.OfferListReq -> m Payment.OfferListResp
offerList merchantId merchantOperatingCityId mbPlaceId paymentServiceType mRoutingId clientSdkVersion = runWithServiceConfigAndServiceName Payment.offerList merchantId merchantOperatingCityId mbPlaceId paymentServiceType mRoutingId clientSdkVersion Nothing

refundOrder :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id TicketPlace) -> PaymentServiceType -> Maybe Text -> Maybe Version -> Payment.AutoRefundReq -> m Payment.AutoRefundResp
refundOrder merchantId merchantOperatingCityId mbPlaceId paymentServiceType mRoutingId clientSdkVersion = runWithServiceConfigAndServiceName Payment.autoRefunds merchantId merchantOperatingCityId mbPlaceId paymentServiceType mRoutingId clientSdkVersion Nothing

verifyVpa :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id TicketPlace) -> PaymentServiceType -> Maybe Text -> Maybe Version -> Payment.VerifyVPAReq -> m Payment.VerifyVPAResp
verifyVpa merchantId merchantOperatingCityId mbPlaceId paymentServiceType mRoutingId clientSdkVersion = runWithServiceConfigAndServiceName Payment.verifyVPA merchantId merchantOperatingCityId mbPlaceId paymentServiceType mRoutingId clientSdkVersion Nothing

---- Ride Payment Related Functions (mostly stripe) ---
createCustomer :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> CreateCustomerReq -> m CreateCustomerResp
createCustomer = runWithServiceConfig1 Payment.createCustomer (.createPaymentCustomer)

getCustomer :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> CustomerId -> m CreateCustomerResp
getCustomer = runWithServiceConfig1 Payment.getCustomer (.createPaymentCustomer)

createEphemeralKeys :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> CustomerId -> m Text
createEphemeralKeys = runWithServiceConfig1 Payment.createEphemeralKeys (.createEphemeralKeys)

getCardList :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> CustomerId -> m CustomerCardListResp
getCardList = runWithServiceConfig1 Payment.getCardList (.getCardList)

createPaymentIntent :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> CreatePaymentIntentReq -> m CreatePaymentIntentResp
createPaymentIntent = runWithServiceConfig1 Payment.createPaymentIntent (.createPaymentIntent)

cancelPaymentIntent :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> PaymentIntentId -> m CreatePaymentIntentResp
cancelPaymentIntent = runWithServiceConfig1 Payment.cancelPaymentIntent (.cancelPaymentIntent)

getPaymentIntent :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> PaymentIntentId -> m CreatePaymentIntentResp
getPaymentIntent = runWithServiceConfig1 Payment.getPaymentIntent (.createPaymentIntent)

updatePaymentMethodInIntent :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> PaymentIntentId -> PaymentMethodId -> m ()
updatePaymentMethodInIntent = runWithServiceConfig2 Payment.updatePaymentMethodInIntent (.updatePaymentMethodInIntent)

capturePaymentIntent :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> PaymentIntentId -> HighPrecMoney -> HighPrecMoney -> m ()
capturePaymentIntent = runWithServiceConfig3 Payment.capturePaymentIntent (.capturePaymentIntent)

-- currently we don't support incremental authorization, so this is not used
-- updateAmountInPaymentIntent :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> PaymentIntentId -> HighPrecMoney -> HighPrecMoney -> m ()
-- updateAmountInPaymentIntent = runWithServiceConfig3 Payment.updateAmountInPaymentIntent (.updateAmountInPaymentIntent)

createSetupIntent :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> CustomerId -> m CreateSetupIntentResp
createSetupIntent = runWithServiceConfig1 Payment.createSetupIntent (.createSetupIntent)

deleteCard :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> PaymentMethodId -> m ()
deleteCard = runWithServiceConfig1 Payment.deleteCard (.deleteCard)

createRefund :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> Payment.CreateRefundReq -> m Payment.CreateRefundResp
createRefund = runWithServiceConfig1 Payment.createRefund (.createRefunds)

getRefund :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DMPM.PaymentMode -> Payment.GetRefundReq -> m Payment.GetRefundResp
getRefund = runWithServiceConfig1 Payment.getRefund (.getRefunds)

runWithServiceConfigAndServiceName ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> Maybe Text -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id TicketPlace) ->
  PaymentServiceType ->
  Maybe Text ->
  Maybe Version ->
  Maybe Bool ->
  req ->
  m resp
runWithServiceConfigAndServiceName func merchantId merchantOperatingCityId mbPlaceId paymentServiceType mRoutingId clientSdkVersion mbIsMockPayment req = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> do
      paymentServiceName <- decidePaymentService (DMSC.PaymentService Payment.Juspay) clientSdkVersion
      CQPBSC.findByPlaceIdAndServiceName id paymentServiceName
    Nothing -> return Nothing
  paymentServiceName <- getPaymentServiceByType paymentServiceType
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId paymentServiceName
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> func (overrideMockUrlIfNeeded vsc mbIsMockPayment) mRoutingId req
    Just (DMSC.MetroPaymentServiceConfig vsc) -> func (overrideMockUrlIfNeeded vsc mbIsMockPayment) mRoutingId req
    Just (DMSC.BusPaymentServiceConfig vsc) -> func (overrideMockUrlIfNeeded vsc mbIsMockPayment) mRoutingId req
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> func (overrideMockUrlIfNeeded vsc mbIsMockPayment) mRoutingId req
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> func (overrideMockUrlIfNeeded vsc mbIsMockPayment) mRoutingId req
    Just (DMSC.PassPaymentServiceConfig vsc) -> func (overrideMockUrlIfNeeded vsc mbIsMockPayment) mRoutingId req
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> func (overrideMockUrlIfNeeded vsc mbIsMockPayment) mRoutingId req
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    overrideMockUrlIfNeeded :: Payment.PaymentServiceConfig -> Maybe Bool -> Payment.PaymentServiceConfig
    overrideMockUrlIfNeeded vsc (Just True) = vsc
    overrideMockUrlIfNeeded vsc _ =
      case vsc of
        Payment.JuspayConfig cfg -> Payment.JuspayConfig $ cfg {JuspayConfig.mockStatusUrl = Nothing}
        _ -> vsc
    getPaymentServiceByType = \case
      Normal -> decidePaymentService (DMSC.PaymentService Payment.Juspay) clientSdkVersion
      Wallet -> pure $ DMSC.JuspayWalletService Payment.Juspay
      BBPS -> pure $ DMSC.BbpsPaymentService Payment.Juspay
      FRFSBooking -> pure $ DMSC.MetroPaymentService Payment.Juspay
      FRFSBusBooking -> pure $ DMSC.BusPaymentService Payment.Juspay
      FRFSMultiModalBooking -> pure $ DMSC.MultiModalPaymentService Payment.Juspay
      FRFSPassPurchase -> pure $ DMSC.PassPaymentService Payment.Juspay
      ParkingBooking -> pure $ DMSC.ParkingPaymentService Payment.Juspay

decidePaymentService :: (ServiceFlow m r) => DMSC.ServiceName -> Maybe Version -> m DMSC.ServiceName
decidePaymentService paymentServiceName clientSdkVersion = do
  aaClientSdkVersion <- L.runIO $ (T.pack . (fromMaybe "999.999.999") <$> SE.lookupEnv "AA_ENABLED_CLIENT_SDK_VERSION")
  return $ case clientSdkVersion of
    Just v
      | v >= textToVersionDefault aaClientSdkVersion -> DMSC.PaymentService Payment.AAJuspay
    _ -> paymentServiceName

modifyPaymentServiceByMode :: PaymentService -> DMPM.PaymentMode -> PaymentService
modifyPaymentServiceByMode Payment.Stripe DMPM.LIVE = Payment.Stripe
modifyPaymentServiceByMode Payment.Stripe DMPM.TEST = Payment.StripeTest
modifyPaymentServiceByMode Payment.StripeTest _ = Payment.StripeTest
modifyPaymentServiceByMode Payment.Juspay _ = Payment.Juspay
modifyPaymentServiceByMode Payment.AAJuspay _ = Payment.AAJuspay

runWithServiceConfig1 ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> PaymentService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  req ->
  m resp
runWithServiceConfig1 func getCfg merchantId merchantOperatingCityId paymentMode req = do
  merchantConfig <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  let paymentService = modifyPaymentServiceByMode (getCfg merchantConfig) (fromMaybe DMPM.LIVE paymentMode)
  merchantPaymentServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.PaymentService paymentService)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show paymentService))
  case merchantPaymentServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

runWithServiceConfig2 ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req1 -> req2 -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> PaymentService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  req1 ->
  req2 ->
  m resp
runWithServiceConfig2 func getCfg merchantId merchantOperatingCityId paymentMode req1 req2 = do
  merchantConfig <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  let paymentService = modifyPaymentServiceByMode (getCfg merchantConfig) (fromMaybe DMPM.LIVE paymentMode)
  merchantPaymentServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.PaymentService paymentService)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show paymentService))
  case merchantPaymentServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig msc -> func msc req1 req2
    _ -> throwError $ InternalError "Unknown Service Config"

runWithServiceConfig3 ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req1 -> req2 -> req3 -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> PaymentService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  req1 ->
  req2 ->
  req3 ->
  m resp
runWithServiceConfig3 func getCfg merchantId merchantOperatingCityId paymentMode req1 req2 req3 = do
  merchantConfig <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  let paymentService = modifyPaymentServiceByMode (getCfg merchantConfig) (fromMaybe DMPM.LIVE paymentMode)
  merchantPaymentServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.PaymentService paymentService)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show paymentService))
  case merchantPaymentServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig msc -> func msc req1 req2 req3
    _ -> throwError $ InternalError "Unknown Service Config"

data SplitType = FIXED | FLEXIBLE deriving (Eq, Ord, Read, Show, Generic, ToSchema, ToParamSchema)

instance ToJSON SplitType where
  toJSON = String . show

instance FromJSON SplitType where
  parseJSON = fmap read . parseJSON

$(mkHttpInstancesForEnum ''SplitType)

data VendorSplitDetails = VendorSplitDetails {splitAmount :: HighPrecMoney, splitType :: SplitType, vendorId :: Text, ticketId :: Maybe Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

roundToTwoDecimalPlaces :: HighPrecMoney -> HighPrecMoney
roundToTwoDecimalPlaces x = fromIntegral (round (x * 100) :: Integer) / 100

-- Round Double percentage to 2 decimal places
roundPercentageToTwoDecimals :: Double -> Double
roundPercentageToTwoDecimals x = fromIntegral (round (x * 100) :: Integer) / 100

roundVendorFee :: VendorSplitDetails -> VendorSplitDetails
roundVendorFee vf = vf {splitAmount = roundToTwoDecimalPlaces vf.splitAmount}

-- Convert absolute amount to percentage based on total amount
-- Formula: (amount / totalAmount) * 100
convertToPercentage :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney
convertToPercentage amount totalAmount = roundToTwoDecimalPlaces $ (amount / totalAmount) * 100

mkSplitSettlementDetails :: (MonadFlow m) => Bool -> HighPrecMoney -> [VendorSplitDetails] -> Bool -> Bool -> m (Maybe SplitSettlementDetails)
mkSplitSettlementDetails isSplitEnabled totalAmount vendorFees isPercentageSplitEnabled isSingleMode = case isSplitEnabled of
  False -> return Nothing
  True -> do
    uuid <- L.generateGUID
    when (isPercentageSplitEnabled && totalAmount <= 0) $ do
      logError $ "Percentage split requested with non-positive total amount: " <> show totalAmount
      throwError (InternalError "Percentage split requires total amount > 0")
    if isPercentageSplitEnabled && isSingleMode
      then do
        -- Create percentage-based splits with aggregation
        let sortedVendorFees = sortBy (compare `on` (\p -> (p.vendorId, p.ticketId))) (roundVendorFee <$> vendorFees)
            groupedVendorFees = groupBy ((==) `on` (\p -> (p.vendorId, p.ticketId))) sortedVendorFees
            mbVendorSplits = map (computePercentageSplit uuid) groupedVendorFees
            vendorPercentageSplits = catMaybes mbVendorSplits
            -- Round each vendor percentage to 2 decimals and sum them
            totalVendorPercentage = sum $ map (\SplitPercentage {amountPercentage} -> amountPercentage) vendorPercentageSplits
            -- Calculate marketplace percentage (100% - sum of vendor percentages) and round to 2 decimals
            -- This ensures the total is exactly 100.00
            marketplacePercentage = roundPercentageToTwoDecimals (100.0 - totalVendorPercentage)

        when (marketplacePercentage < 0) $ do
          logError $ "Marketplace percentage is negative: " <> show marketplacePercentage <> " for vendorFees: " <> show vendorFees <> "totalVendorPercentage: " <> show totalVendorPercentage
          throwError (InternalError "Marketplace percentage is negative")

        logInfo $ "Creating aggregated percentage-based split settlement details - vendorPercentageSplits: " <> show vendorPercentageSplits <> " marketplacePercentage: " <> show marketplacePercentage
        logInfo $ "Aggregated percentage-based split settlement details created successfully with totalAmount: " <> show totalAmount <> " and " <> show (length vendorPercentageSplits) <> " vendor splits"

        return $
          Just $
            PercentageBased $
              SplitSettlementDetailsPercentage
                { marketplace = MarketplacePercentage marketplacePercentage,
                  mdrBorneBy = ALL,
                  vendor = VendorPercentage vendorPercentageSplits
                }
      else do
        -- Create amount-based splits with aggregation (existing logic)
        let sortedVendorFees = sortBy (compare `on` (\p -> (p.vendorId, p.ticketId))) (roundVendorFee <$> vendorFees)
            groupedVendorFees = groupBy ((==) `on` (\p -> (p.vendorId, p.ticketId))) sortedVendorFees
            mbVendorSplits = map (computeSplit uuid) groupedVendorFees
            vendorSplits = catMaybes mbVendorSplits
            totalVendorAmount = roundToTwoDecimalPlaces $ sum $ map (\Split {amount} -> amount) vendorSplits
            marketplaceAmount = roundToTwoDecimalPlaces (totalAmount - totalVendorAmount)
        when (marketplaceAmount < 0) $ do
          logError $ "Marketplace amount is negative: " <> show marketplaceAmount <> " for vendorFees: " <> show vendorFees <> " totalVendorAmount: " <> show totalVendorAmount <> " totalAmount: " <> show totalAmount
          throwError (InternalError "Marketplace amount is negative")

        logInfo $ "Creating aggregated amount-based split settlement details - vendorSplits: " <> show vendorSplits <> " marketplaceAmount: " <> show marketplaceAmount
        logInfo $ "Aggregated amount-based split settlement details created successfully with totalAmount: " <> show totalAmount <> " totalVendorAmount: " <> show totalVendorAmount <> " and " <> show (length vendorSplits) <> " vendor splits"

        return $
          Just $
            AmountBased $
              SplitSettlementDetailsAmount
                { marketplace = Marketplace marketplaceAmount,
                  mdrBorneBy = ALL,
                  vendor = Vendor vendorSplits
                }
  where
    computeSplit uniqueId feesForVendor =
      case feesForVendor of
        [] -> Nothing
        (firstFee : _) ->
          Just $
            Split
              { amount = roundToTwoDecimalPlaces $ sum $ map (\fee -> splitAmount fee) feesForVendor,
                merchantCommission = 0,
                subMid = firstFee.vendorId,
                uniqueSplitId = fromMaybe uniqueId firstFee.ticketId
              }

    computePercentageSplit uniqueId feesForVendor =
      case feesForVendor of
        [] -> Nothing
        (firstFee : _) ->
          let aggregatedAmount = roundToTwoDecimalPlaces $ sum (map splitAmount feesForVendor)
              percentageAmount = convertToPercentage aggregatedAmount totalAmount
              roundedPercentage = roundPercentageToTwoDecimals (realToFrac percentageAmount)
           in Just $
                SplitPercentage
                  { amountPercentage = roundedPercentage,
                    merchantCommissionPercentage = 0,
                    subMid = firstFee.vendorId,
                    uniqueSplitId = fromMaybe uniqueId firstFee.ticketId
                  }

mkUnaggregatedSplitSettlementDetails :: (MonadFlow m) => Bool -> HighPrecMoney -> [VendorSplitDetails] -> Bool -> Bool -> m (Maybe SplitSettlementDetails)
mkUnaggregatedSplitSettlementDetails isSplitEnabled totalAmount vendorFees isPercentageSplitEnabled isSingleMode = case isSplitEnabled of
  False -> return Nothing
  True -> do
    uuid <- L.generateGUID
    when (isPercentageSplitEnabled && totalAmount <= 0) $ do
      logError $ "Percentage split requested with non-positive total amount: " <> show totalAmount
      throwError (InternalError "Percentage split requires total amount > 0")
    if isPercentageSplitEnabled && isSingleMode
      then do
        -- Create percentage-based splits
        let vendorPercentageSplits =
              map
                ( \fee ->
                    let roundedFee = roundVendorFee fee
                        -- Convert absolute amount to percentage: (amount / totalAmount) * 100
                        percentageAmount = convertToPercentage (splitAmount roundedFee) totalAmount
                        roundedPercentage = roundPercentageToTwoDecimals (realToFrac percentageAmount)
                     in SplitPercentage
                          { amountPercentage = roundedPercentage,
                            merchantCommissionPercentage = 0,
                            subMid = vendorId roundedFee,
                            uniqueSplitId = fromMaybe uuid fee.ticketId
                          }
                )
                vendorFees

            -- Calculate marketplace percentage (100% - sum of vendor percentages)
            -- Round each vendor percentage to 2 decimals and sum them
            totalVendorPercentage = sum $ map (\SplitPercentage {amountPercentage} -> amountPercentage) vendorPercentageSplits
            -- Round marketplace percentage to 2 decimals to ensure total is exactly 100.00
            marketplacePercentage = roundPercentageToTwoDecimals (100.0 - totalVendorPercentage)

        when (marketplacePercentage < 0) $ do
          logError $ "Marketplace percentage is negative: " <> show marketplacePercentage <> " for vendorFees: " <> show vendorFees <> "totalVendorPercentage: " <> show totalVendorPercentage
          throwError (InternalError "Marketplace percentage is negative")

        logInfo $ "Creating percentage-based split settlement details - vendorPercentageSplits: " <> show vendorPercentageSplits <> " marketplacePercentage: " <> show marketplacePercentage
        logInfo $ "Split settlement details created successfully with totalAmount: " <> show totalAmount <> " and " <> show (length vendorPercentageSplits) <> " vendor splits"

        return $
          Just $
            PercentageBased $
              SplitSettlementDetailsPercentage
                { marketplace = MarketplacePercentage marketplacePercentage,
                  mdrBorneBy = ALL,
                  vendor = VendorPercentage vendorPercentageSplits
                }
      else do
        let vendorSplits =
              map
                ( \fee ->
                    let roundedFee = roundVendorFee fee
                     in Split
                          { amount = splitAmount roundedFee,
                            merchantCommission = 0,
                            subMid = vendorId roundedFee,
                            uniqueSplitId = fromMaybe uuid fee.ticketId
                          }
                )
                vendorFees
            totalVendorAmount = roundToTwoDecimalPlaces $ sum $ map (\Split {amount} -> amount) vendorSplits
            marketplaceAmount = roundToTwoDecimalPlaces (totalAmount - totalVendorAmount)
        when (marketplaceAmount < 0) $ do
          logError $ "Marketplace amount is negative: " <> show marketplaceAmount <> " for vendorFees: " <> show vendorFees <> "totalVendorAmount: " <> show totalVendorAmount <> " totalAmount: " <> show totalAmount
          throwError (InternalError "Marketplace amount is negative")

        logInfo $ "Creating amount-based split settlement details - vendorSplits: " <> show vendorSplits <> " marketplaceAmount: " <> show marketplaceAmount
        logInfo $ "Amount-based split settlement details created successfully with totalAmount: " <> show totalAmount <> " totalVendorAmount: " <> show totalVendorAmount <> " and " <> show (length vendorSplits) <> " vendor splits"

        return $
          Just $
            AmountBased $
              SplitSettlementDetailsAmount
                { marketplace = Marketplace marketplaceAmount,
                  mdrBorneBy = ALL,
                  vendor = Vendor vendorSplits
                }

mkUnaggregatedRefundSplitSettlementDetails :: (MonadFlow m) => Bool -> HighPrecMoney -> [VendorSplitDetails] -> m (Maybe RefundSplitSettlementDetails)
mkUnaggregatedRefundSplitSettlementDetails isSplitEnabled totalAmount vendorFees = case isSplitEnabled of
  False -> return Nothing
  True -> do
    uuid <- L.generateGUID
    let vendorSplits =
          map
            ( \fee ->
                let roundedFee = roundVendorFee fee
                 in RefundSplit
                      { refundAmount = splitAmount roundedFee,
                        subMid = vendorId roundedFee,
                        uniqueSplitId = fromMaybe uuid fee.ticketId
                      }
            )
            vendorFees
        totalVendorAmount = roundToTwoDecimalPlaces $ sum $ map (\RefundSplit {refundAmount} -> refundAmount) vendorSplits
        marketplaceAmount = roundToTwoDecimalPlaces (totalAmount - totalVendorAmount)
    when (marketplaceAmount < 0) $ do
      logError $ "Marketplace amount is negative: " <> show marketplaceAmount <> " for vendorFees: " <> show vendorFees <> "totalVendorAmount: " <> show totalVendorAmount <> " totalAmount: " <> show totalAmount
      throwError (InternalError "Marketplace amount is negative")

    logInfo $ "Creating refund split settlement details - vendorSplits: " <> show vendorSplits <> " marketplaceAmount: " <> show marketplaceAmount
    logInfo $ "Refund split settlement details created successfully with totalAmount: " <> show totalAmount <> " totalVendorAmount: " <> show totalVendorAmount <> " and " <> show (length vendorSplits) <> " vendor refund splits"

    return $
      Just $
        RefundSplitSettlementDetails
          { marketplace = RefundMarketplace marketplaceAmount,
            mdrBorneBy = ALL,
            vendor = RefundVendor vendorSplits
          }

groupSumVendorSplits :: [VendorSplitDetails] -> [VendorSplitDetails]
groupSumVendorSplits vendorFees = map (\groups -> (head groups) {splitAmount = roundToTwoDecimalPlaces $ sum (map splitAmount groups)}) (groupBy ((==) `on` vendorId) (sortOn vendorId vendorFees))

getIsSplitEnabled ::
  (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id TicketPlace) ->
  PaymentServiceType ->
  m Bool
getIsSplitEnabled merchantId merchantOperatingCityId mbPlaceId paymentServiceType = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName id (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (getPaymentServiceByType paymentServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  return $ case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> Payment.isSplitEnabled vsc
    Just (DMSC.MetroPaymentServiceConfig vsc) -> Payment.isSplitEnabled vsc
    Just (DMSC.BusPaymentServiceConfig vsc) -> Payment.isSplitEnabled vsc
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> Payment.isSplitEnabled vsc
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> Payment.isSplitEnabled vsc
    Just (DMSC.PassPaymentServiceConfig vsc) -> Payment.isSplitEnabled vsc
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> Payment.isSplitEnabled vsc
    _ -> False
  where
    getPaymentServiceByType = \case
      Normal -> DMSC.PaymentService Payment.Juspay
      Wallet -> DMSC.JuspayWalletService Payment.Juspay
      BBPS -> DMSC.BbpsPaymentService Payment.Juspay
      FRFSBooking -> DMSC.MetroPaymentService Payment.Juspay
      FRFSBusBooking -> DMSC.BusPaymentService Payment.Juspay
      FRFSMultiModalBooking -> DMSC.MultiModalPaymentService Payment.Juspay
      FRFSPassPurchase -> DMSC.PassPaymentService Payment.Juspay
      ParkingBooking -> DMSC.ParkingPaymentService Payment.Juspay

getIsPercentageSplit ::
  (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id TicketPlace) ->
  PaymentServiceType ->
  m Bool
getIsPercentageSplit merchantId merchantOperatingCityId mbPlaceId paymentServiceType = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName id (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (getPaymentServiceByType paymentServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  return $ case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> Payment.isPercentageSplit vsc
    Just (DMSC.MetroPaymentServiceConfig vsc) -> Payment.isPercentageSplit vsc
    Just (DMSC.BusPaymentServiceConfig vsc) -> Payment.isPercentageSplit vsc
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> Payment.isPercentageSplit vsc
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> Payment.isPercentageSplit vsc
    Just (DMSC.PassPaymentServiceConfig vsc) -> Payment.isPercentageSplit vsc
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> Payment.isPercentageSplit vsc
    _ -> False
  where
    getPaymentServiceByType = \case
      Normal -> DMSC.PaymentService Payment.Juspay
      Wallet -> DMSC.JuspayWalletService Payment.Juspay
      BBPS -> DMSC.BbpsPaymentService Payment.Juspay
      FRFSBooking -> DMSC.MetroPaymentService Payment.Juspay
      FRFSBusBooking -> DMSC.BusPaymentService Payment.Juspay
      FRFSMultiModalBooking -> DMSC.MultiModalPaymentService Payment.Juspay
      FRFSPassPurchase -> DMSC.PassPaymentService Payment.Juspay
      ParkingBooking -> DMSC.ParkingPaymentService Payment.Juspay

getIsRefundSplitEnabled ::
  (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id TicketPlace) ->
  PaymentServiceType ->
  m Bool
getIsRefundSplitEnabled merchantId merchantOperatingCityId mbPlaceId paymentServiceType = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName id (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (getPaymentServiceByType paymentServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  return $ case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> Payment.isRefundSplitEnabled vsc
    Just (DMSC.MetroPaymentServiceConfig vsc) -> Payment.isRefundSplitEnabled vsc
    Just (DMSC.BusPaymentServiceConfig vsc) -> Payment.isRefundSplitEnabled vsc
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> Payment.isRefundSplitEnabled vsc
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> Payment.isRefundSplitEnabled vsc
    Just (DMSC.PassPaymentServiceConfig vsc) -> Payment.isRefundSplitEnabled vsc
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> Payment.isRefundSplitEnabled vsc
    _ -> False
  where
    getPaymentServiceByType = \case
      Normal -> DMSC.PaymentService Payment.Juspay
      Wallet -> DMSC.JuspayWalletService Payment.Juspay
      BBPS -> DMSC.BbpsPaymentService Payment.Juspay
      FRFSBooking -> DMSC.MetroPaymentService Payment.Juspay
      FRFSBusBooking -> DMSC.BusPaymentService Payment.Juspay
      FRFSMultiModalBooking -> DMSC.MultiModalPaymentService Payment.Juspay
      FRFSPassPurchase -> DMSC.PassPaymentService Payment.Juspay
      ParkingBooking -> DMSC.ParkingPaymentService Payment.Juspay

getPaymentOrderValidity ::
  (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id TicketPlace) ->
  PaymentServiceType ->
  m (Maybe Seconds)
getPaymentOrderValidity merchantId merchantOperatingCityId mbPlaceId paymentServiceType = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName id (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (getPaymentServiceByType paymentServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  return $ case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> extractPaymentOrderValidity vsc
    Just (DMSC.MetroPaymentServiceConfig vsc) -> extractPaymentOrderValidity vsc
    Just (DMSC.BusPaymentServiceConfig vsc) -> extractPaymentOrderValidity vsc
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> extractPaymentOrderValidity vsc
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> extractPaymentOrderValidity vsc
    Just (DMSC.PassPaymentServiceConfig vsc) -> extractPaymentOrderValidity vsc
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> extractPaymentOrderValidity vsc
    _ -> Nothing
  where
    extractPaymentOrderValidity = \case
      JuspayConfig cfg -> cfg.paymentOrderValidity
      _ -> Nothing

    getPaymentServiceByType = \case
      Normal -> DMSC.PaymentService Payment.Juspay
      Wallet -> DMSC.JuspayWalletService Payment.Juspay
      BBPS -> DMSC.BbpsPaymentService Payment.Juspay
      FRFSBooking -> DMSC.MetroPaymentService Payment.Juspay
      FRFSBusBooking -> DMSC.BusPaymentService Payment.Juspay
      FRFSMultiModalBooking -> DMSC.MultiModalPaymentService Payment.Juspay
      FRFSPassPurchase -> DMSC.PassPaymentService Payment.Juspay
      ParkingBooking -> DMSC.ParkingPaymentService Payment.Juspay

extractSplitSettlementDetailsAmount :: Maybe SplitSettlementDetails -> Maybe SplitSettlementDetailsAmount
extractSplitSettlementDetailsAmount Nothing = Nothing
extractSplitSettlementDetailsAmount (Just (AmountBased details)) = Just details
extractSplitSettlementDetailsAmount (Just (PercentageBased _)) = Nothing

fetchGatewayReferenceId ::
  (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id TicketPlace) ->
  PaymentServiceType ->
  m (Maybe Text)
fetchGatewayReferenceId merchantId merchantOperatingCityId mbPlaceId paymentServiceType = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName id (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (getPaymentServiceByType paymentServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  return $ case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> Payment.getGatewayReferenceId vsc
    Just (DMSC.MetroPaymentServiceConfig vsc) -> Payment.getGatewayReferenceId vsc
    Just (DMSC.BusPaymentServiceConfig vsc) -> Payment.getGatewayReferenceId vsc
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> Payment.getGatewayReferenceId vsc
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> Payment.getGatewayReferenceId vsc
    Just (DMSC.PassPaymentServiceConfig vsc) -> Payment.getGatewayReferenceId vsc
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> Payment.getGatewayReferenceId vsc
    _ -> Nothing
  where
    getPaymentServiceByType = \case
      Normal -> DMSC.PaymentService Payment.Juspay
      Wallet -> DMSC.JuspayWalletService Payment.Juspay
      BBPS -> DMSC.BbpsPaymentService Payment.Juspay
      FRFSBooking -> DMSC.MetroPaymentService Payment.Juspay
      FRFSBusBooking -> DMSC.BusPaymentService Payment.Juspay
      FRFSMultiModalBooking -> DMSC.MultiModalPaymentService Payment.Juspay
      FRFSPassPurchase -> DMSC.PassPaymentService Payment.Juspay
      ParkingBooking -> DMSC.ParkingPaymentService Payment.Juspay

fetchOfferSKUConfig ::
  (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id TicketPlace) ->
  PaymentServiceType ->
  m (Maybe Text)
fetchOfferSKUConfig merchantId merchantOperatingCityId mbPlaceId paymentServiceType = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName id (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (getPaymentServiceByType paymentServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  return $ case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> Payment.offerSKUConfig vsc
    Just (DMSC.MetroPaymentServiceConfig vsc) -> Payment.offerSKUConfig vsc
    Just (DMSC.BusPaymentServiceConfig vsc) -> Payment.offerSKUConfig vsc
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> Payment.offerSKUConfig vsc
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> Payment.offerSKUConfig vsc
    Just (DMSC.PassPaymentServiceConfig vsc) -> Payment.offerSKUConfig vsc
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> Payment.offerSKUConfig vsc
    _ -> Nothing
  where
    getPaymentServiceByType = \case
      Normal -> DMSC.PaymentService Payment.Juspay
      Wallet -> DMSC.JuspayWalletService Payment.Juspay
      BBPS -> DMSC.BbpsPaymentService Payment.Juspay
      FRFSBooking -> DMSC.MetroPaymentService Payment.Juspay
      FRFSBusBooking -> DMSC.BusPaymentService Payment.Juspay
      FRFSMultiModalBooking -> DMSC.MultiModalPaymentService Payment.Juspay
      FRFSPassPurchase -> DMSC.PassPaymentService Payment.Juspay
      ParkingBooking -> DMSC.ParkingPaymentService Payment.Juspay
