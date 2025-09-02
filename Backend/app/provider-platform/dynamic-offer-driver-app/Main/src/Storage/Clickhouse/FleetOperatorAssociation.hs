module Storage.Clickhouse.FleetOperatorAssociation where

import qualified Domain.Types.FleetOperatorAssociation as FDA
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data FleetOperatorAssociationT f = FleetOperatorAssociationT
  { id :: C f (Id FDA.FleetOperatorAssociation),
    fleetOwnerId :: C f Text,
    operatorId :: C f Text,
    isActive :: C f Bool
  }
  deriving (Generic)

deriving instance Show FleetOperatorAssociation

fleetOperatorAssociationTTable :: FleetOperatorAssociationT (FieldModification FleetOperatorAssociationT)
fleetOperatorAssociationTTable =
  FleetOperatorAssociationT
    { id = "id",
      fleetOwnerId = "fleet_owner_id",
      operatorId = "operator_id",
      isActive = "is_active"
    }

type FleetOperatorAssociation = FleetOperatorAssociationT Identity

$(TH.mkClickhouseInstances ''FleetOperatorAssociationT 'SELECT_FINAL_MODIFIER)

getFleetOwnerIdsByOperatorId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m [Text]
getFleetOwnerIdsByOperatorId operatorId =
  CH.findAll $
    CH.select_ (\assoc -> CH.notGrouped (assoc.fleetOwnerId)) $
      CH.filter_
        (\assoc -> assoc.operatorId CH.==. operatorId CH.&&. assoc.isActive CH.==. True)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOperatorAssociationTTable)
