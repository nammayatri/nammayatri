{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.VehicleRegistrationCertificate where

import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import Storage.Clickhouse.DriverInformation (flagCountRow, flagFromText)

data VehicleRegistrationCertificateT f = VehicleRegistrationCertificateT
  { id :: C f (Id DRC.VehicleRegistrationCertificate),
    unencryptedCertificateNumber :: C f (Maybe Text),
    vehicleModel :: C f (Maybe Text),
    vehicleManufacturer :: C f (Maybe Text),
    verified :: C f (Maybe Text),
    approved :: C f (Maybe Text),
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
--   with the same rule as the Redis onboarding counters. Flags are read as text, see `flagFromText`.
getStatusCountsByFleetOwnerIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  m [(Maybe Bool, Maybe Bool, Int)]
getStatusCountsByFleetOwnerIds fleetOwnerIds =
  map flagCountRow
    <$> CH.findAll
      ( CH.select_
          ( \info -> do
              let countVehicles = CH.count_ info.id
              CH.groupBy (info.verified, info.approved) $ \(verified, approved) -> (verified, approved, countVehicles)
          )
          $ CH.filter_
            (\info -> info.fleetOwnerId `CH.in_` (map Just fleetOwnerIds))
            (CH.all_ @CH.APP_SERVICE_CLICKHOUSE vehicleRegistrationCertificateTTable)
      )

-- | Verified RC ids owned by the given fleet owners (no driver link required).
findVerifiedIdsByFleetOwnerIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  m [Id DRC.VehicleRegistrationCertificate]
findVerifiedIdsByFleetOwnerIds fleetOwnerIds =
  if null fleetOwnerIds
    then pure []
    else do
      rows <-
        CH.findAll $
          CH.select_ (\rc -> CH.notGrouped (rc.id, rc.verified)) $
            CH.filter_
              (\rc -> rc.fleetOwnerId `CH.in_` (map Just fleetOwnerIds))
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE vehicleRegistrationCertificateTTable)
      pure [rcId | (rcId, verified) <- rows, flagFromText verified == Just True]
