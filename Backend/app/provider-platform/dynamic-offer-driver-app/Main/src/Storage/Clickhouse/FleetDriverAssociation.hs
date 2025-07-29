module Storage.Clickhouse.FleetDriverAssociation where

import qualified Domain.Types.FleetDriverAssociation as FDA
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data FleetDriverAssociationT f = FleetDriverAssociationT
  { id :: C f (Id FDA.FleetDriverAssociation),
    driverId :: C f (Id DP.Person),
    fleetOwnerId :: C f Text,
    isActive :: C f Bool
  }
  deriving (Generic)

deriving instance Show FleetDriverAssociation

fleetDriverAssociationTTable :: FleetDriverAssociationT (FieldModification FleetDriverAssociationT)
fleetDriverAssociationTTable =
  FleetDriverAssociationT
    { id = "id",
      driverId = "driver_id",
      fleetOwnerId = "fleet_owner_id",
      isActive = "is_active"
    }

type FleetDriverAssociation = FleetDriverAssociationT Identity

$(TH.mkClickhouseInstances ''FleetDriverAssociationT 'SELECT_FINAL_MODIFIER)

getDriverIdsByFleetOwnerId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m [Id DP.Person]
getDriverIdsByFleetOwnerId fleetOwnerId =
  CH.findAll $
    CH.select_ (\assoc -> CH.notGrouped (assoc.driverId)) $
      CH.filter_
        (\assoc _ -> assoc.fleetOwnerId CH.==. fleetOwnerId CH.&&. assoc.isActive CH.==. True)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetDriverAssociationTTable)

getDriverIdsByFleetOwnerIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  m [Id DP.Person]
getDriverIdsByFleetOwnerIds fleetOwnerIds =
  CH.findAll $
    CH.select_ (\assoc -> CH.notGrouped (assoc.driverId)) $
      CH.filter_
        (\assoc _ -> (assoc.fleetOwnerId `CH.in_` fleetOwnerIds) CH.&&. (assoc.isActive CH.==. True))
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetDriverAssociationTTable)
