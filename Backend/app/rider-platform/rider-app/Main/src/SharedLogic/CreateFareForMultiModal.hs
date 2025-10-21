{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CreateFareForMultiModal where

import qualified Domain.Types.Extra.VendorSplitDetails as VendorSplitDetails
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VendorSplitDetails as VendorSplitDetails
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.Payment ()
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.VendorSplitDetails as QVendorSplitDetails
import qualified Tools.Payment as Payment

fareProcessingLockKey :: Text -> Text
fareProcessingLockKey journeyId = "Fare:Processing:JourneyId" <> journeyId

createFares :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => Text -> Text -> m Bool
createFares searchId pricingId = do
  QJourneyLeg.updateLegPricingIdByLegSearchId (Just pricingId) (Just searchId)
  mbShouldConfirmFare <- getConfirmOnceGetFare searchId
  when (mbShouldConfirmFare == Just True) $ resetConfirmOnceGetFare searchId
  return (mbShouldConfirmFare == Just True)

confirmOnceGetFare :: Text -> Text
confirmOnceGetFare searchId = "COGF:SRID-" <> searchId

setConfirmOnceGetFare :: CacheFlow m r => Text -> m ()
setConfirmOnceGetFare searchId = do
  Hedis.withCrossAppRedis $ Hedis.setExp (confirmOnceGetFare searchId) True 600

resetConfirmOnceGetFare :: CacheFlow m r => Text -> m ()
resetConfirmOnceGetFare searchId = do
  Hedis.withCrossAppRedis $ Hedis.setExp (confirmOnceGetFare searchId) False 600

getConfirmOnceGetFare :: CacheFlow m r => Text -> m (Maybe Bool)
getConfirmOnceGetFare searchId = Hedis.withCrossAppRedis (Hedis.safeGet (confirmOnceGetFare searchId))

createVendorSplitFromBookings ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.PaymentServiceType ->
  Bool ->
  m ([Payment.VendorSplitDetails], HighPrecMoney)
createVendorSplitFromBookings allJourneyBookings merchantId merchantOperatingCityId paymentType isFRFSTestingEnabled = do
  let amount =
        if isFRFSTestingEnabled
          then 1.0 * (HighPrecMoney $ toRational $ length allJourneyBookings)
          else
            foldl
              (\accAmt item -> (accAmt + item.price.amount))
              0.0
              allJourneyBookings
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId merchantOperatingCityId Nothing paymentType
  case allJourneyBookings of
    [] -> return ([], 0.0)
    _ -> do
      if isSplitEnabled
        then do
          splitDetailsZippedByBooking <- do
            mapM
              ( \item -> do
                  integBppConfig <- SIBC.findIntegratedBPPConfigById item.integratedBppConfigId
                  vendorSplitDetailsList <- QVendorSplitDetails.findAllByIntegratedBPPConfigId integBppConfig.id
                  let amountPerBooking = if isFRFSTestingEnabled then 1.0 else item.price.amount
                  return (item.id, (amountPerBooking, vendorSplitDetailsList))
              )
              allJourneyBookings
          vendorSplitDetailsListToIncludeInSplit <- QVendorSplitDetails.findAllByMerchantOperatingCityIdAndIncludeInSplit (Just merchantOperatingCityId) (Just True)
          vendorSplitDetails <- convertVendorDetails splitDetailsZippedByBooking vendorSplitDetailsListToIncludeInSplit isFRFSTestingEnabled
          return (vendorSplitDetails, amount)
        else return ([], amount)

convertVendorDetails ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  [(Id FTBooking.FRFSTicketBooking, (HighPrecMoney, [VendorSplitDetails.VendorSplitDetails]))] ->
  [VendorSplitDetails.VendorSplitDetails] ->
  Bool ->
  m [Payment.VendorSplitDetails]
convertVendorDetails splitDetailsZippedByBooking vendorDetailsToIncludeByDefault isFRFSTestingEnabled = do
  let validVendorSplitDetails = concat $ map (\ele -> createVendorSplitForBooking ele) splitDetailsZippedByBooking
  finalSplits <- ensureAllRequiredVendorsExist validVendorSplitDetails
  logInfo $ "validVendorSplitDetails" <> show validVendorSplitDetails
  logInfo $ "finalSplits" <> show finalSplits
  return finalSplits
  where
    createVendorSplitForBooking (bookingId, (amount, vd)) = map (\splitDetails -> toPaymentVendorDetails bookingId.getId amount splitDetails) vd
    toPaymentVendorDetails bookingId amount vd =
      let totalAmount = if isFRFSTestingEnabled then (1 :: HighPrecMoney) else amount
          splitAmount =
            if vd.splitType == VendorSplitDetails.FLEXIBLE
              then calculateSplitAmount vd.splitShare totalAmount
              else totalAmount
       in Payment.VendorSplitDetails
            { splitAmount = splitAmount,
              splitType = vendorSplitDetailSplitTypeToPaymentSplitType vd.splitType,
              vendorId = vd.vendorId,
              ticketId = Just $ bookingId
            }

    calculateSplitAmount :: Maybe VendorSplitDetails.SplitShare -> HighPrecMoney -> HighPrecMoney
    calculateSplitAmount mbSplitPercentage totalAmount =
      case mbSplitPercentage of
        Just (VendorSplitDetails.Percentage percentage) ->
          totalAmount * (fromRational (toRational percentage) / 100.0)
        Just (VendorSplitDetails.FixedValue fixedValue) ->
          fromIntegral fixedValue
        Nothing ->
          totalAmount

    ensureAllRequiredVendorsExist ::
      ( EsqDBReplicaFlow m r,
        BeamFlow m r,
        EncFlow m r,
        ServiceFlow m r
      ) =>
      [Payment.VendorSplitDetails] ->
      m [Payment.VendorSplitDetails]
    ensureAllRequiredVendorsExist existingVendorSplits = do
      let existingVendorIds = map (.vendorId) existingVendorSplits
          missingVendors = filter (\vd -> vd.vendorId `notElem` existingVendorIds) vendorDetailsToIncludeByDefault
      missingVendorSplits <- mapM createDefaultVendorSplit missingVendors
      return $ existingVendorSplits ++ missingVendorSplits

    createDefaultVendorSplit ::
      ( EsqDBReplicaFlow m r,
        BeamFlow m r,
        EncFlow m r,
        ServiceFlow m r
      ) =>
      VendorSplitDetails.VendorSplitDetails ->
      m Payment.VendorSplitDetails
    createDefaultVendorSplit vd = do
      ticketId <- generateGUID
      return $
        Payment.VendorSplitDetails
          { splitAmount = 0,
            splitType = vendorSplitDetailSplitTypeToPaymentSplitType vd.splitType,
            vendorId = vd.vendorId,
            ticketId = Just ticketId
          }

vendorSplitDetailSplitTypeToPaymentSplitType :: VendorSplitDetails.SplitType -> Payment.SplitType
vendorSplitDetailSplitTypeToPaymentSplitType = \case
  VendorSplitDetails.FIXED -> Payment.FIXED
  VendorSplitDetails.FLEXIBLE -> Payment.FLEXIBLE

createBasketFromBookings ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.PaymentServiceType ->
  Maybe Bool ->
  m [Payment.Basket]
createBasketFromBookings allJourneyBookings merchantId merchantOperatingCityId paymentServiceType mbEnableOffer = do
  logDebug $ "mbEnableOffer: " <> show mbEnableOffer
  let dummyBasket =
        [ Payment.Basket
            { Payment.id = "no_basket",
              Payment.unitPrice = 0,
              Payment.quantity = 1
            }
        ]
  if mbEnableOffer /= Just True
    then do
      return dummyBasket
    else do
      case allJourneyBookings of
        [booking] -> do
          -- offer valid only for single mode booking (not handled for multimodal right now)
          quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)
          mbOfferSKUProductId <- Payment.fetchOfferSKUConfig merchantId merchantOperatingCityId Nothing paymentServiceType
          case (mbOfferSKUProductId, quote.quantity, fromMaybe 0 quote.childTicketQuantity) of
            (Just offerSKUProductId, 1, 0) -> do
              let unitPrice = quote.price.amount
              return $
                [ Payment.Basket
                    { Payment.id = offerSKUProductId,
                      Payment.unitPrice = unitPrice,
                      Payment.quantity = quote.quantity
                    }
                ]
            _ -> return dummyBasket
        _ -> return dummyBasket
