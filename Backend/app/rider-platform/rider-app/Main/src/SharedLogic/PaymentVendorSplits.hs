module SharedLogic.PaymentVendorSplits where

import qualified Domain.Types.Extra.VendorSplitDetails as VendorSplitDetails
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VendorSplitDetails as VendorSplitDetails
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.VendorSplitDetails as QVendorSplitDetails
import qualified Tools.Payment as Payment

data ItemDetail = ItemDetail
  { itemId :: Text,
    itemTransactionId :: Text,
    amount :: HighPrecMoney
  }

createVendorSplit ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.PaymentServiceType ->
  [ItemDetail] ->
  m ([Payment.VendorSplitDetails])
createVendorSplit merchantId merchantOperatingCityId paymentServiceType itemDetails = do
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId merchantOperatingCityId Nothing paymentServiceType
  if isSplitEnabled
    then do
      itemDetailsWithVendorSplitDetails <- mapM createVendorSplitListForItemDetail itemDetails
      vendorSplitDetailsListToIncludeInSplit <- QVendorSplitDetails.findAllByMerchantOperatingCityIdAndIncludeInSplit (Just merchantOperatingCityId) (Just True)
      vendorSplitDetails <- convertVendorDetails itemDetailsWithVendorSplitDetails vendorSplitDetailsListToIncludeInSplit
      return vendorSplitDetails
    else return []
  where
    createVendorSplitListForItemDetail itemDetail = do
      -- Note :: Column name is InteratedBPPConfig but it need not be that always, it can be any itemId for which fulfillment is made
      vendorSplitDetailsList <- QVendorSplitDetails.findAllByIntegratedBPPConfigId (Id itemDetail.itemId)
      return (itemDetail, vendorSplitDetailsList)

convertVendorDetails ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  [(ItemDetail, [VendorSplitDetails.VendorSplitDetails])] ->
  [VendorSplitDetails.VendorSplitDetails] ->
  m [Payment.VendorSplitDetails]
convertVendorDetails itemDetailsWithVendorSplitDetails vendorDetailsToIncludeByDefault = do
  let validVendorSplitDetails = concatMap createVendorSplitForBooking itemDetailsWithVendorSplitDetails
  finalSplits <- ensureAllRequiredVendorsExist validVendorSplitDetails
  return finalSplits
  where
    createVendorSplitForBooking (itemDetail, vendorSplitDetails) =
      map
        ( \splitDetail ->
            toPaymentVendorDetails itemDetail.itemTransactionId itemDetail.amount splitDetail
        )
        vendorSplitDetails
    toPaymentVendorDetails transactionId amount splitDetail =
      let splitAmount =
            if splitDetail.splitType == VendorSplitDetails.FLEXIBLE
              then calculateSplitAmount splitDetail.splitShare amount
              else amount
       in Payment.VendorSplitDetails
            { splitAmount = splitAmount,
              splitType = vendorSplitDetailSplitTypeToPaymentSplitType splitDetail.splitType,
              vendorId = splitDetail.vendorId,
              ticketId = Just transactionId
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
      transactionId <- generateGUID
      return $
        Payment.VendorSplitDetails
          { splitAmount = 0,
            splitType = vendorSplitDetailSplitTypeToPaymentSplitType vd.splitType,
            vendorId = vd.vendorId,
            ticketId = Just transactionId
          }

vendorSplitDetailSplitTypeToPaymentSplitType :: VendorSplitDetails.SplitType -> Payment.SplitType
vendorSplitDetailSplitTypeToPaymentSplitType = \case
  VendorSplitDetails.FIXED -> Payment.FIXED
  VendorSplitDetails.FLEXIBLE -> Payment.FLEXIBLE
