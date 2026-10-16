{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.FleetOwnerInformation where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data FleetOwnerInformationT f = FleetOwnerInformationT
  { fleetOwnerPersonId :: C f (Id DP.Person),
    verified :: C f (Maybe Bool),
    approved :: C f (Maybe Bool),
    merchantOperatingCityId :: C f (Maybe Text)
  }
  deriving (Generic)

deriving instance Show FleetOwnerInformation

fleetOwnerInformationTTable :: FleetOwnerInformationT (FieldModification FleetOwnerInformationT)
fleetOwnerInformationTTable =
  FleetOwnerInformationT
    { fleetOwnerPersonId = "fleet_owner_person_id",
      verified = "verified",
      approved = "approved",
      merchantOperatingCityId = "merchant_operating_city_id"
    }

type FleetOwnerInformation = FleetOwnerInformationT Identity

$(TH.mkClickhouseInstances ''FleetOwnerInformationT 'SELECT_FINAL_MODIFIER)

-- | Fleet owner counts per (verified, approved) for the legacy statusSummary; the caller buckets
--   them with the same rule as the Redis onboarding counters. The mirror columns are Nullable.
getStatusCountsByCityId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m [(Maybe Bool, Maybe Bool, Int)]
getStatusCountsByCityId cityId =
  CH.findAll $
    CH.select_
      ( \info -> do
          let countOwners = CH.count_ info.fleetOwnerPersonId
          CH.groupBy (info.verified, info.approved) $ \(verified, approved) -> (verified, approved, countOwners)
      )
      $ CH.filter_
        (\info -> info.merchantOperatingCityId CH.==. Just cityId)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOwnerInformationTTable)

getFleetOwnerIdsByCityId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m [Id DP.Person]
getFleetOwnerIdsByCityId cityId =
  CH.findAll $
    CH.select_ (\info -> CH.notGrouped info.fleetOwnerPersonId) $
      CH.filter_
        (\info -> info.merchantOperatingCityId CH.==. Just cityId)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOwnerInformationTTable)
