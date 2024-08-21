{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Domain.Action.Beckn.Select
  ( DSelectReq (..),
    validateRequest,
    handler,
    DeliveryDetails (..),
    PersonDetails (..),
    DSelectReqDetails (..),
  )
where

import BecknV2.OnDemand.Enums (DeliveryInitiation)
import Data.Text as Text
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types.DeliveryPersonDetails as DPD
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.Location (LocationAddress)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestDeliveryDetails as DSRDD
import qualified Domain.Types.Trip as DTrip
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.AppMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.DriverPool
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestDeliveryDetails as QSRDD
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
    customerPhoneNum :: Maybe Text,
    isAdvancedBookingEnabled :: Bool,
    isMultipleOrNoDeviceIdExist :: Maybe Bool,
    toUpdateDeviceIdInfo :: Bool,
    selectReqDetails :: Maybe DSelectReqDetails
  }

data DSelectReqDetails = DSelectReqDeliveryDetails DeliveryDetails

data DeliveryDetails = DeliveryDetails
  { senderDetails :: PersonDetails,
    receiverDetails :: PersonDetails,
    initiatedAs :: DeliveryInitiation
  }

data PersonDetails = PersonDetails
  { name :: Text,
    phone :: Text,
    address :: LocationAddress
  }

-- user can select array of estimate because of book any option, in most of the cases it will be a single estimate
handler :: DM.Merchant -> DSelectReq -> DSR.SearchRequest -> [DEst.Estimate] -> Flow ()
handler merchant sReq searchReq estimates = do
  now <- getCurrentTime
  case sReq.customerPhoneNum of
    Just number -> do
      (riderDetails, isNewRider) <- SRD.getRiderDetails searchReq.currency merchant.id (fromMaybe "+91" merchant.mobileCountryCode) number now False
      when isNewRider $ QRD.create riderDetails
      QSR.updateRiderId searchReq.id riderDetails.id
      when sReq.toUpdateDeviceIdInfo do
        let mbFlag = mbGetPayoutFlag sReq.isMultipleOrNoDeviceIdExist
        when (riderDetails.payoutFlagReason /= mbFlag) $ QRD.updateFlagReasonAndIsDeviceIdExists mbFlag (Just $ isJust sReq.isMultipleOrNoDeviceIdExist) riderDetails.id
    Nothing -> do
      logWarning "Failed to get rider details as BAP Phone Number is NULL"
  when sReq.autoAssignEnabled $ QSR.updateAutoAssign searchReq.id sReq.autoAssignEnabled
  when sReq.isAdvancedBookingEnabled $ QSR.updateIsAdvancedBookingEnabled sReq.isAdvancedBookingEnabled searchReq.id
  searchRequestDetails <- case sReq.selectReqDetails of
    Just (DSelectReqDeliveryDetails deliveryDetails) -> do
      -- update search Request location
      let senderLocationId = searchReq.fromLocation.id
      when (isNothing searchReq.toLocation) $ throwError $ InvalidRequest "BPP: Receiver location not found for trip category Delivery"
      let receiverLocationId = fromJust (searchReq.toLocation <&> (.id))
      QLoc.updateInstructionsAndExtrasById deliveryDetails.senderDetails.address.instructions deliveryDetails.senderDetails.address.extras senderLocationId
      QLoc.updateInstructionsAndExtrasById deliveryDetails.receiverDetails.address.instructions deliveryDetails.receiverDetails.address.extras receiverLocationId
      -- update Rider details
      (senderRiderDetails, isNewSender) <- SRD.getRiderDetails searchReq.currency merchant.id (fromMaybe "+91" merchant.mobileCountryCode) deliveryDetails.senderDetails.phone now False
      (receiverRiderDetails, isNewReceiver) <- SRD.getRiderDetails searchReq.currency merchant.id (fromMaybe "+91" merchant.mobileCountryCode) deliveryDetails.receiverDetails.phone now False
      when isNewSender $ QRD.create senderRiderDetails
      when isNewReceiver $ QRD.create receiverRiderDetails
      -- create search request delivery details
      encSenderPhoneNumber <- encrypt deliveryDetails.senderDetails.phone
      encReceiverPhoneNumber <- encrypt deliveryDetails.receiverDetails.phone
      let searchRequestDeliveryDetails =
            DSRDD.SearchRequestDeliveryDetails
              { DSRDD.initiatedAs = deliveryDetails.initiatedAs,
                DSRDD.receiverDetails =
                  DPD.DeliveryPersonDetails
                    { DPD.name = deliveryDetails.receiverDetails.name,
                      DPD.phone = encReceiverPhoneNumber,
                      DPD.id = receiverRiderDetails.id
                    },
                DSRDD.senderDetails =
                  DPD.DeliveryPersonDetails
                    { DPD.name = deliveryDetails.senderDetails.name,
                      DPD.phone = encSenderPhoneNumber,
                      DPD.id = senderRiderDetails.id
                    },
                DSRDD.searchRequestId = searchReq.id.getId,
                DSRDD.createdAt = now,
                DSRDD.updatedAt = now
              }
      QSRDD.create searchRequestDeliveryDetails
      QSR.updateTripCategory (Just (DTrip.Delivery DTrip.OneWayOnDemandDynamicOffer)) searchReq.id
      return $ Just (DSR.DeliveryDetails searchRequestDeliveryDetails)
    Nothing -> return Nothing
  tripQuoteDetails <-
    estimates `forM` \estimate -> do
      QDQ.setInactiveAllDQByEstId estimate.id now
      let mbDriverExtraFeeBounds = ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
      buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 sReq.customerExtraFee) Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId
  let searchReq' = searchReq {DSR.isAdvanceBookingEnabled = sReq.isAdvancedBookingEnabled, DSR.searchRequestDetails = searchRequestDetails}
  let driverSearchBatchInput =
        DriverSearchBatchInput
          { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
            merchant,
            searchReq = searchReq',
            tripQuoteDetails,
            customerExtraFee = sReq.customerExtraFee,
            messageId = sReq.messageId,
            isRepeatSearch = False
          }
  initiateDriverSearchBatch driverSearchBatchInput
  Metrics.finishGenericLatencyMetrics Metrics.SELECT_TO_SEND_REQUEST searchReq.transactionId
  where
    mbGetPayoutFlag isMultipleOrNoDeviceIdExist = maybe Nothing (\val -> if val then (Just DRD.MultipleDeviceIdExists) else Nothing) isMultipleOrNoDeviceIdExist

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
