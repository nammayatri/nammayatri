{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.Ride where

import qualified AWS.S3 as S3
import Data.List (sortOn)
import qualified Data.Text as T
import Domain.Types.Ride
import qualified Domain.Types.RideEvidence as DRE
import Domain.Utils
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.RideEvidence as QRideEvidence
import Tools.Error

getDeliveryImage :: Id Ride -> Maybe Text -> Flow Text
getDeliveryImage rideId apiKey = do
  ride <- runInReplica $ QRideLite.findByIdLite rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBookingLite.findByIdLite ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  latestDeliveryFileId <- ride.deliveryFileIds >>= safeLast & fromMaybeM DeliveryImageNotFound
  mediaFile <- QMF.findById latestDeliveryFileId >>= fromMaybeM (FileDoNotExist latestDeliveryFileId.getId)
  mediaFilePath <- mediaFile.s3FilePath & fromMaybeM (FileDoNotExist latestDeliveryFileId.getId)
  S3.get $ T.unpack mediaFilePath

-- What the customer's app is allowed to know about a proof. The stored file path stays on this side.
data RideEvidenceInternalItem = RideEvidenceInternalItem
  { evidenceId :: Text,
    evidenceDescription :: Maybe Text,
    evidenceRideStatus :: Text,
    evidenceCreatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- Proofs that ops has hidden never reach the customer.
isVisibleToCustomer :: DRE.RideEvidence -> Bool
isVisibleToCustomer evidence = isNothing evidence.hiddenAt

authoriseInternalRideCall :: Id Ride -> Maybe Text -> Flow ()
authoriseInternalRideCall rideId apiKey = do
  ride <- runInReplica $ QRideLite.findByIdLite rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBookingLite.findByIdLite ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

getRideEvidence :: Id Ride -> Maybe Text -> Flow [RideEvidenceInternalItem]
getRideEvidence rideId apiKey = do
  authoriseInternalRideCall rideId apiKey
  evidences <- QRideEvidence.findAllByRideId rideId
  pure $
    map
      ( \evidence ->
          RideEvidenceInternalItem
            { evidenceId = evidence.id.getId,
              evidenceDescription = evidence.description,
              evidenceRideStatus = show evidence.rideStatus,
              evidenceCreatedAt = evidence.createdAt
            }
      )
      (sortOn (.createdAt) (filter isVisibleToCustomer evidences))

getRideEvidenceFile :: Id Ride -> Text -> Maybe Text -> Flow Text
getRideEvidenceFile rideId evidenceId apiKey = do
  authoriseInternalRideCall rideId apiKey
  evidence <- QRideEvidence.findById (Id evidenceId) >>= fromMaybeM (InvalidRequest $ "Evidence does not exist: " <> evidenceId)
  unless (evidence.rideId == rideId && isVisibleToCustomer evidence) $
    throwError (InvalidRequest $ "Evidence does not exist: " <> evidenceId)
  mediaFile <- QMF.findById evidence.mediaFileId >>= fromMaybeM (FileDoNotExist evidence.mediaFileId.getId)
  mediaFilePath <- mediaFile.s3FilePath & fromMaybeM (FileDoNotExist evidence.mediaFileId.getId)
  S3.get $ T.unpack mediaFilePath
