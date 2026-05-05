{-# OPTIONS_GHC -Wno-orphans #-}
module Storage.Clickhouse.FleetOwnerInformation where

import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import Storage.Clickhouse.DriverInformation ()

data FleetOwnerInformationT f = FleetOwnerInformationT
  { fleetOwnerPersonId :: C f (Id DP.Person),
    docsVerificationStatus :: C f (Maybe DDVS.DocsVerificationStatus),
    merchantOperatingCityId :: C f (Maybe Text)
  }
  deriving (Generic)

deriving instance Show FleetOwnerInformation

fleetOwnerInformationTTable :: FleetOwnerInformationT (FieldModification FleetOwnerInformationT)
fleetOwnerInformationTTable =
  FleetOwnerInformationT
    { fleetOwnerPersonId = "fleet_owner_person_id",
      docsVerificationStatus = "docs_verification_status",
      merchantOperatingCityId = "merchant_operating_city_id"
    }

type FleetOwnerInformation = FleetOwnerInformationT Identity

$(TH.mkClickhouseInstances ''FleetOwnerInformationT 'SELECT_FINAL_MODIFIER)

getStatusCountsByCityId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m [(Maybe DDVS.DocsVerificationStatus, Int)]
getStatusCountsByCityId cityId =
  CH.findAll $
    CH.select_
      ( \info -> do
          let status = info.docsVerificationStatus
          let countOwners = CH.count_ info.fleetOwnerPersonId
          CH.groupBy status $ \s -> (s, countOwners)
      )
      $ CH.filter_
        (\info -> info.merchantOperatingCityId CH.==. Just cityId)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOwnerInformationTTable)
