{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.VehicleRegistrationCertificate where

import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data VehicleRegistrationCertificateT f = VehicleRegistrationCertificateT
  { id :: C f (Id DRC.VehicleRegistrationCertificate),
    unencryptedCertificateNumber :: C f (Maybe Text),
    vehicleModel :: C f (Maybe Text),
    vehicleManufacturer :: C f (Maybe Text),
    verified :: C f (Maybe Bool),
    approved :: C f (Maybe Bool),
    fleetOwnerId :: C f (Maybe Text)
  }
  deriving (Generic)

deriving instance Show VehicleRegistrationCertificate

vehicleRegistrationCertificateTTable :: VehicleRegistrationCertificateT (FieldModification VehicleRegistrationCertificateT)
vehicleRegistrationCertificateTTable =
  VehicleRegistrationCertificateT
    { id = "id",
      unencryptedCertificateNumber = "unencrypted_certificate_number",
      vehicleModel = "vehicle_model",
      vehicleManufacturer = "vehicle_manufacturer",
      verified = "verified",
      approved = "approved",
      fleetOwnerId = "fleet_owner_id"
    }

type VehicleRegistrationCertificate = VehicleRegistrationCertificateT Identity

$(TH.mkClickhouseInstances ''VehicleRegistrationCertificateT 'SELECT_FINAL_MODIFIER)

findByIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRC.VehicleRegistrationCertificate] ->
  m [VehicleRegistrationCertificate]
findByIds ids = do
  CH.findAll $
    CH.select $
      CH.filter_ (\rc -> rc.id `CH.in_` ids) (CH.all_ @CH.APP_SERVICE_CLICKHOUSE vehicleRegistrationCertificateTTable)

-- | Vehicle counts per (verified, approved) for the legacy statusSummary; the caller buckets them
--   with the same rule as the Redis onboarding counters.
getStatusCountsByFleetOwnerIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  m [(Maybe Bool, Maybe Bool, Int)]
getStatusCountsByFleetOwnerIds fleetOwnerIds =
  CH.findAll $
    CH.select_
      ( \info -> do
          let countVehicles = CH.count_ info.id
          CH.groupBy (info.verified, info.approved) $ \(verified, approved) -> (verified, approved, countVehicles)
      )
      $ CH.filter_
        (\info -> info.fleetOwnerId `CH.in_` (map Just fleetOwnerIds))
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE vehicleRegistrationCertificateTTable)
