{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.CustomerCancellationDues where

import qualified Domain.Action.UI.Ride.CancelRide.Internal as RideCancelInternal
import qualified Domain.Types.CancellationDuesDetails as DCDD
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (DbHash, unDbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified SharedLogic.UserCancellationDues as UserCancellationDues
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Text.Hex
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error
import Tools.OpenAPIInstances ()

showDbHashHex :: DbHash -> Text
showDbHashHex = Text.Hex.encodeHex . unDbHash

data CancellationDuesReq = CancellationDuesReq
  { customerMobileNumberHash :: DbHash
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { cancellationDues :: PriceAPIEntity,
    cancellationDuesPaid :: HighPrecMoney,
    noOfTimesCancellationDuesPaid :: Int,
    waivedOffAmount :: HighPrecMoney,
    noOfTimesWaiveOffUsed :: Int,
    duesBreakup :: Maybe [CancellationDueBreakup]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CancellationDueBreakup = CancellationDueBreakup
  { rideId :: Id DRide.Ride,
    dueAmount :: PriceAPIEntity,
    dueStatus :: DCDD.CancellationDuesPaymentStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CustomerCancellationDuesSyncReq = CustomerCancellationDuesSyncReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    cancellationLedgerAction :: UserCancellationDues.CancellationLedgerAction,
    bppRideId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getCancellationDuesDetails ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Bool ->
  CancellationDuesReq ->
  m CancellationDuesDetailsRes
getCancellationDuesDetails merchantId _merchantCity apiKey mbIncludeBreakup CancellationDuesReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  riderDetails <- QRD.findByMobileNumberHashAndMerchant customerMobileNumberHash merchant.id >>= fromMaybeM (RiderDetailsDoNotExist "Mobile Number Hash" (showDbHashHex customerMobileNumberHash))
  mbDuesBreakup <-
    if mbIncludeBreakup == Just True
      then do
        pendingDues <- QCDD.findAllPendingByRiderId riderDetails.id
        let sorted = sortOn (Down . (.createdAt)) pendingDues
        pure $ Just $ fmap mkBreakup sorted
      else pure Nothing
  return $
    CancellationDuesDetailsRes
      { cancellationDues = PriceAPIEntity riderDetails.cancellationDues riderDetails.currency,
        cancellationDuesPaid = riderDetails.cancellationDuesPaid,
        noOfTimesCancellationDuesPaid = riderDetails.noOfTimesCanellationDuesPaid,
        waivedOffAmount = riderDetails.waivedOffAmount,
        noOfTimesWaiveOffUsed = riderDetails.noOfTimesWaiveOffUsed,
        duesBreakup = mbDuesBreakup
      }
  where
    mkBreakup row =
      CancellationDueBreakup
        { rideId = row.rideId,
          dueAmount = PriceAPIEntity row.cancellationAmount row.currency,
          dueStatus = row.paymentStatus
        }

customerCancellationDuesSync ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Id Merchant ->
  Context.City ->
  Maybe Text ->
  CustomerCancellationDuesSyncReq ->
  m APISuccess
customerCancellationDuesSync merchantId merchantCity apiKey req = ActorInfo.withRequestIdActorInfo $ do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  merchantOperatingCity <- CQMM.findByMerchantIdAndCity merchantId merchantCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchantCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCity.id Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCity.id.getId)
  mbRide <- QRide.findById (Id req.bppRideId)
  Kernel.Prelude.whenJust mbRide $ \ride -> do
    mbBooking <- QBooking.findById ride.bookingId
    Kernel.Prelude.whenJust mbBooking $ \booking ->
      RideCancelInternal.applyCancellationLedgerAction booking ride req.cancellationLedgerAction transporterConfig
  return Success
