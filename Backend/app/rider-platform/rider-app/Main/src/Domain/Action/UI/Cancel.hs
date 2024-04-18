{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Cancel
  ( softCancel,
    disputeCancellationDues,
    CancelReq (..),
    CancelRes (..),
    CancelSearch (..),
    CancellationDuesDetailsRes (..),
    mkDomainCancelSearch,
    cancelSearch,
    getCancellationDuesDetails,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.DriverOffer as DDO
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonFlowStatus as DPFS
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DVeh
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverOffer as QDOffer
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QP
import Tools.Error

data CancelReq = CancelReq
  { reasonCode :: SCR.CancellationReasonCode,
    reasonStage :: SCR.CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancelRes = CancelRes
  { bppBookingId :: Maybe (Id SRB.BPPBooking),
    bppId :: Text,
    bppUrl :: BaseUrl,
    cancellationSource :: SBCR.CancellationSource,
    transactionId :: Text,
    merchant :: DM.Merchant,
    cancelStatus :: Text,
    city :: Context.City,
    vehicleVariant :: DVeh.VehicleVariant
  }

data CancelSearch = CancelSearch
  { estimateId :: Id DEstimate.Estimate,
    providerUrl :: BaseUrl,
    providerId :: Text,
    estimateStatus :: DEstimate.EstimateStatus,
    searchReqId :: Id SearchRequest,
    sendToBpp :: Bool,
    merchant :: DM.Merchant,
    city :: Context.City,
    vehicleVariant :: DVeh.VehicleVariant
  }

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { cancellationDues :: HighPrecMoney,
    disputeChancesUsed :: Int,
    canBlockCustomer :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

softCancel :: (EncFlow m r, Esq.EsqDBReplicaFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> m CancelRes
softCancel bookingId _ = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  bppBookingId <- fromMaybeM (BookingFieldNotPresent "bppBookingId") booking.bppBookingId
  city <-
    CQMOC.findById booking.merchantOperatingCityId
      >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  return $
    CancelRes
      { bppBookingId = Just bppBookingId,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        cancellationSource = SBCR.ByUser,
        transactionId = booking.transactionId,
        merchant = merchant,
        cancelStatus = show Enums.SOFT_CANCEL,
        vehicleVariant = DVST.castServiceTierToVariant booking.vehicleServiceTierType, -- TODO: fix it
        ..
      }

mkDomainCancelSearch ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) =>
  Id Person.Person ->
  Id DEstimate.Estimate ->
  m CancelSearch
mkDomainCancelSearch personId estimateId = do
  estStatus <- QEstimate.getStatus estimateId >>= fromMaybeM (EstimateStatusDoesNotExist estimateId.getId)
  let isEstimateNotNew = estStatus /= DEstimate.NEW
  buildCancelReq estimateId isEstimateNotNew estStatus
  where
    buildCancelReq estId isEstimateNotNew estStatus = do
      estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
      person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      isValueAddNP <- CQVAN.isValueAddNP estimate.providerId
      let searchRequestId = estimate.requestId
      city <- case estimate.merchantOperatingCityId of
        Nothing -> pure merchant.defaultCity
        Just mOCId ->
          CQMOC.findById mOCId
            >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound mOCId.getId)
      pure
        CancelSearch
          { estimateId = estId,
            providerUrl = estimate.providerUrl,
            providerId = estimate.providerId,
            searchReqId = searchRequestId,
            estimateStatus = estStatus,
            sendToBpp = isEstimateNotNew && isValueAddNP,
            merchant = merchant,
            vehicleVariant = DVST.castServiceTierToVariant estimate.vehicleServiceTierType, -- TODO: fix it
            ..
          }

cancelSearch ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Person.Person ->
  CancelSearch ->
  m ()
cancelSearch personId dcr = do
  _ <-
    if dcr.estimateStatus == DEstimate.GOT_DRIVER_QUOTE
      then -- then Esq.runTransaction $ do
      do
        _ <- QPFS.updateStatus personId DPFS.IDLE
        void $ QEstimate.updateStatus DEstimate.DRIVER_QUOTE_CANCELLED dcr.estimateId
        QDOffer.updateStatus DDO.INACTIVE dcr.estimateId
      else do
        _ <- QPFS.updateStatus personId DPFS.IDLE
        void $ QEstimate.updateStatus DEstimate.CANCELLED dcr.estimateId
        QDOffer.updateStatus DDO.INACTIVE dcr.estimateId
  QPFS.clearCache personId

disputeCancellationDues :: (Id Person.Person, Id Merchant.Merchant) -> Flow APISuccess
disputeCancellationDues (personId, merchantId) = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) -> do
      CallBPPInternal.disputeCancellationDues merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mobileNumber countryCode person.currentCity
    _ -> throwError (PersonMobileNumberIsNULL person.id.getId)

getCancellationDuesDetails :: (Id Person.Person, Id Merchant.Merchant) -> Flow CancellationDuesDetailsRes
getCancellationDuesDetails (personId, merchantId) = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) -> do
      res <- CallBPPInternal.getCancellationDuesDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mobileNumber countryCode person.currentCity
      return $ CancellationDuesDetailsRes {cancellationDues = res.customerCancellationDues, disputeChancesUsed = res.disputeChancesUsed, canBlockCustomer = res.canBlockCustomer}
    _ -> throwError (PersonMobileNumberIsNULL person.id.getId)
