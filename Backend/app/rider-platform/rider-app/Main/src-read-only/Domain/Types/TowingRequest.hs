{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TowingRequest where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import qualified Domain.Types.TowingVehicle
import qualified Domain.Types.VehicleViolation
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data TowingRequestE e = TowingRequest
  { agencyEntityId :: Kernel.Prelude.Text,
    agentId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancelledReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    dropAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropFacilityName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    dropLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    evidence :: [Domain.Types.TowingRequest.TowingEvidence],
    id :: Kernel.Types.Id.Id Domain.Types.TowingRequest.TowingRequest,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notes :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    parkingFine :: Kernel.Types.Common.HighPrecMoney,
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentOrderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder),
    paymentReceivedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    paymentReceivedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentStatus :: Domain.Types.TowingRequest.TowingPaymentStatus,
    pickupAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    priority :: Domain.Types.TowingRequest.TowingPriority,
    receiptNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    releasedToIdNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    releasedToIdType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    releasedToName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    releasedToPhone :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    releasedToRelation :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestNumber :: Kernel.Prelude.Text,
    route :: Kernel.Prelude.Maybe Data.Aeson.Value,
    status :: Domain.Types.TowingRequest.TowingStatus,
    storageFeePerDay :: Kernel.Types.Common.HighPrecMoney,
    ticketPlaceId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace),
    totalDaysInYard :: Kernel.Prelude.Int,
    totalFee :: Kernel.Types.Common.HighPrecMoney,
    towingFee :: Kernel.Types.Common.HighPrecMoney,
    towingVehicleId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TowingVehicle.TowingVehicle),
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    violationId :: Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type TowingRequest = TowingRequestE ('AsEncrypted)

type DecryptedTowingRequest = TowingRequestE ('AsUnencrypted)

instance EncryptedItem TowingRequest where
  type Unencrypted TowingRequest = (DecryptedTowingRequest, HashSalt)
  encryptItem (entity, salt) = do
    releasedToIdNumber_ <- encryptItem $ (,salt) <$> releasedToIdNumber entity
    releasedToPhone_ <- encryptItem $ (,salt) <$> releasedToPhone entity
    pure
      TowingRequest
        { agencyEntityId = agencyEntityId entity,
          agentId = agentId entity,
          cancelledReason = cancelledReason entity,
          currency = currency entity,
          dropAddress = dropAddress entity,
          dropFacilityName = dropFacilityName entity,
          dropLat = dropLat entity,
          dropLon = dropLon entity,
          evidence = evidence entity,
          id = id entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          notes = notes entity,
          parkingFine = parkingFine entity,
          paymentMethod = paymentMethod entity,
          paymentOrderId = paymentOrderId entity,
          paymentReceivedAt = paymentReceivedAt entity,
          paymentReceivedBy = paymentReceivedBy entity,
          paymentStatus = paymentStatus entity,
          pickupAddress = pickupAddress entity,
          pickupLat = pickupLat entity,
          pickupLon = pickupLon entity,
          priority = priority entity,
          receiptNumber = receiptNumber entity,
          releasedToIdNumber = releasedToIdNumber_,
          releasedToIdType = releasedToIdType entity,
          releasedToName = releasedToName entity,
          releasedToPhone = releasedToPhone_,
          releasedToRelation = releasedToRelation entity,
          requestNumber = requestNumber entity,
          route = route entity,
          status = status entity,
          storageFeePerDay = storageFeePerDay entity,
          ticketPlaceId = ticketPlaceId entity,
          totalDaysInYard = totalDaysInYard entity,
          totalFee = totalFee entity,
          towingFee = towingFee entity,
          towingVehicleId = towingVehicleId entity,
          vehicleNumber = vehicleNumber entity,
          vehicleType = vehicleType entity,
          violationId = violationId entity,
          merchantId = merchantId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    releasedToIdNumber_ <- fmap fst <$> decryptItem (releasedToIdNumber entity)
    releasedToPhone_ <- fmap fst <$> decryptItem (releasedToPhone entity)
    pure
      ( TowingRequest
          { agencyEntityId = agencyEntityId entity,
            agentId = agentId entity,
            cancelledReason = cancelledReason entity,
            currency = currency entity,
            dropAddress = dropAddress entity,
            dropFacilityName = dropFacilityName entity,
            dropLat = dropLat entity,
            dropLon = dropLon entity,
            evidence = evidence entity,
            id = id entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            notes = notes entity,
            parkingFine = parkingFine entity,
            paymentMethod = paymentMethod entity,
            paymentOrderId = paymentOrderId entity,
            paymentReceivedAt = paymentReceivedAt entity,
            paymentReceivedBy = paymentReceivedBy entity,
            paymentStatus = paymentStatus entity,
            pickupAddress = pickupAddress entity,
            pickupLat = pickupLat entity,
            pickupLon = pickupLon entity,
            priority = priority entity,
            receiptNumber = receiptNumber entity,
            releasedToIdNumber = releasedToIdNumber_,
            releasedToIdType = releasedToIdType entity,
            releasedToName = releasedToName entity,
            releasedToPhone = releasedToPhone_,
            releasedToRelation = releasedToRelation entity,
            requestNumber = requestNumber entity,
            route = route entity,
            status = status entity,
            storageFeePerDay = storageFeePerDay entity,
            ticketPlaceId = ticketPlaceId entity,
            totalDaysInYard = totalDaysInYard entity,
            totalFee = totalFee entity,
            towingFee = towingFee entity,
            towingVehicleId = towingVehicleId entity,
            vehicleNumber = vehicleNumber entity,
            vehicleType = vehicleType entity,
            violationId = violationId entity,
            merchantId = merchantId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' TowingRequest where
  type UnencryptedItem TowingRequest = DecryptedTowingRequest
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data TowingEvidence = TowingEvidence
  { accuracyMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    caption :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    capturedAt :: Kernel.Prelude.UTCTime,
    capturedBy :: Kernel.Prelude.Text,
    capturedByRole :: Kernel.Prelude.Text,
    deviceInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    mediaFileId :: Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile,
    stage :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data TowingPaymentStatus
  = Unpaid
  | Partial
  | Paid
  | Waived
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data TowingPriority
  = Normal
  | Urgent
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data TowingStatus
  = Pending
  | Accepted
  | AgentAssigned
  | EnRoutePickup
  | ArrivedPickup
  | VehicleLoaded
  | EnRouteDrop
  | ArrivedDrop
  | VehicleUnloaded
  | InYard
  | ReleaseRequested
  | ReleasePaymentPending
  | ReleasePaymentReceived
  | VehicleReleased
  | Completed
  | Cancelled
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TowingPaymentStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TowingPriority))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TowingStatus))
