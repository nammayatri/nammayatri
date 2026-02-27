{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.Beckn.Select
  ( DSelectReq (..),
    validateRequest,
    handler,
  )
where

import Data.Either.Extra (eitherToMaybe)
import Data.Text as Text hiding (find)
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Yudhishthira as Y
import Environment
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.AppMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Yudhishthira.Types as Yudhishthira
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.DriverPool
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import qualified SharedLogic.Type as SLT
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    estimateIds :: [Id DEst.Estimate],
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupTime :: UTCTime,
    autoAssignEnabled :: Bool,
    customerExtraFee :: Maybe HighPrecMoney,
    isPetRide :: Bool,
    customerPhoneNum :: Maybe Text,
    isAdvancedBookingEnabled :: Bool,
    isMultipleOrNoDeviceIdExist :: Maybe Bool,
    toUpdateDeviceIdInfo :: Bool,
    disabilityDisable :: Maybe Bool,
    parcelDetails :: (Maybe Text, Maybe Int),
    preferSafetyPlus :: Bool,
    billingCategory :: SLT.BillingCategory,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }

-- user can select array of estimate because of book any option, in most of the cases it will be a single estimate
handler :: DM.Merchant -> DSelectReq -> DSR.SearchRequest -> [DEst.Estimate] -> Flow ()
handler merchant sReq searchReq estimates = do
  logDebug $ "DSelectReq: select request billingCategory: " <> show sReq.billingCategory <> "transactionId: " <> sReq.transactionId
  now <- getCurrentTime
  riderId <- case sReq.customerPhoneNum of
    Just number -> do
      let mbMerchantOperatingCityId = Just searchReq.merchantOperatingCityId
      (riderDetails, isNewRider) <- SRD.getRiderDetails searchReq.currency merchant.id mbMerchantOperatingCityId (fromMaybe "+91" merchant.mobileCountryCode) number searchReq.bapId False
      when isNewRider $ QRD.create riderDetails
      when sReq.toUpdateDeviceIdInfo do
        let mbFlag = mbGetPayoutFlag sReq.isMultipleOrNoDeviceIdExist
        when (riderDetails.payoutFlagReason /= mbFlag) $ QRD.updateFlagReasonAndIsDeviceIdExists mbFlag (Just $ isJust sReq.isMultipleOrNoDeviceIdExist) riderDetails.id
      return (Just riderDetails.id)
    Nothing -> do
      logWarning "Failed to get rider details as BAP Phone Number is NULL"
      return Nothing
  when sReq.isPetRide $ do
    let tagData =
          Y.SelectTagData
            { isPetRide = sReq.isPetRide
            -- ,estimates = estimates uncomment this line if you want to use estimates in select tag data
            }
    addNammaTags tagData searchReq
  tripQuoteDetails <-
    estimates `forM` \estimate -> do
      QDQ.setInactiveAllDQByEstId estimate.id now
      let mbDriverExtraFeeBounds = ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
          driverAdditionalCharges = filterChargesByApplicability $ fromMaybe [] ((.conditionalCharges) <$> estimate.farePolicy)
          petCharges' = if sReq.isPetRide then (.petCharges) =<< estimate.farePolicy else Nothing
          businessDiscount = if sReq.billingCategory == SLT.BUSINESS then fromMaybe 0.0 estimate.businessDiscount else 0.0
          personalDiscount = if sReq.billingCategory == SLT.PERSONAL then fromMaybe 0.0 estimate.personalDiscount else 0.0
      buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 sReq.customerExtraFee + fromMaybe 0 petCharges' - businessDiscount - personalDiscount) Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId driverAdditionalCharges False ((.congestionCharge) =<< estimate.fareParams) petCharges' (estimate.fareParams >>= (.priorityCharges)) estimate.commissionCharges (estimate.fareParams >>= (.tollCharges))
  let parcelType = (fst sReq.parcelDetails) >>= \rpt -> readMaybe @DParcel.ParcelType $ unpack rpt
      updatedSearchRequest =
        searchReq
          { DSR.disabilityTag = if sReq.disabilityDisable == Just True then Nothing else searchReq.disabilityTag,
            DSR.isAdvanceBookingEnabled = sReq.isAdvancedBookingEnabled || searchReq.isAdvanceBookingEnabled,
            DSR.autoAssignEnabled = if sReq.autoAssignEnabled then Just sReq.autoAssignEnabled else searchReq.autoAssignEnabled,
            DSR.riderId = riderId,
            DSR.parcelType = if isJust parcelType then parcelType else searchReq.parcelType,
            DSR.parcelQuantity = if isJust parcelType then snd sReq.parcelDetails else searchReq.parcelQuantity,
            DSR.preferSafetyPlus = sReq.preferSafetyPlus
          }
  QSR.updateMultipleByRequestId updatedSearchRequest searchReq.isScheduled
  QSR.updateByPrimaryKey updatedSearchRequest
  let driverSearchBatchInput =
        DriverSearchBatchInput
          { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
            merchant,
            searchReq = updatedSearchRequest,
            tripQuoteDetails,
            customerExtraFee = sReq.customerExtraFee,
            messageId = sReq.messageId,
            isRepeatSearch = False,
            billingCategory = sReq.billingCategory,
            isAllocatorBatch = False,
            paymentMethodInfo = sReq.paymentMethodInfo
          }
  void $ initiateDriverSearchBatch driverSearchBatchInput
  Metrics.finishGenericLatencyMetrics Metrics.SELECT_TO_SEND_REQUEST searchReq.transactionId
  where
    mbGetPayoutFlag isMultipleOrNoDeviceIdExist = maybe Nothing (\val -> if val then Just DRD.MultipleDeviceIdExists else Nothing) isMultipleOrNoDeviceIdExist
    filterChargesByApplicability conditionalCharges = do
      let safetyCharges = if sReq.preferSafetyPlus then find (\ac -> (ac.chargeCategory) == DAC.SAFETY_PLUS_CHARGES) conditionalCharges else Nothing
          nyregularCharges = if fromMaybe False searchReq.isReserveRide then find (\ac -> (ac.chargeCategory) == DAC.NYREGULAR_SUBSCRIPTION_CHARGE) conditionalCharges else Nothing
      catMaybes $ [safetyCharges, nyregularCharges]

validateRequest :: Id DM.Merchant -> DSelectReq -> Flow (DM.Merchant, DSR.SearchRequest, [DEst.Estimate])
validateRequest merchantId sReq = do
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mbEstimates <- mapM QEst.findById sReq.estimateIds
  let estimates = catMaybes mbEstimates
  case estimates of
    [] -> throwError $ InvalidRequest "User need to select at least one estimate"
    (estimate : xs) -> do
      searchReq <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
      return (merchant, searchReq, [estimate] <> xs)

addNammaTags :: Y.SelectTagData -> DSR.SearchRequest -> Flow ()
addNammaTags tagData sReq = do
  newSearchTags <- withTryCatch "computeNammaTags:Select" (Yudhishthira.computeNammaTags Yudhishthira.Select tagData)
  let tags = sReq.searchTags <> eitherToMaybe newSearchTags
  QSR.updateSearchTags tags sReq.id
