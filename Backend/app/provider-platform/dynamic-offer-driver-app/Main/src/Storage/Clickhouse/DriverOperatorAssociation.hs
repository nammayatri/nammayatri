module Storage.Clickhouse.DriverOperatorAssociation where

import qualified Domain.Types.DriverOperatorAssociation as DOA
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data DriverOperatorAssociationT f = DriverOperatorAssociationT
  { id :: C f (Id DOA.DriverOperatorAssociation),
    driverId :: C f (Id DP.Person),
    operatorId :: C f Text,
    isActive :: C f Bool
  }
  deriving (Generic)

deriving instance Show DriverOperatorAssociation

driverOperatorAssociationTTable :: DriverOperatorAssociationT (FieldModification DriverOperatorAssociationT)
driverOperatorAssociationTTable =
  DriverOperatorAssociationT
    { id = "id",
      driverId = "driver_id",
      operatorId = "operator_id",
      isActive = "is_active"
    }

type DriverOperatorAssociation = DriverOperatorAssociationT Identity

$(TH.mkClickhouseInstances ''DriverOperatorAssociationT 'NO_SELECT_MODIFIER)

getDriverIdsByOperatorId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m [Id DP.Person]
getDriverIdsByOperatorId operatorId =
  CH.findAll $
    CH.select_ (\assoc -> CH.notGrouped (assoc.driverId)) $
      CH.filter_
        (\assoc _ -> assoc.operatorId CH.==. operatorId CH.&&. assoc.isActive CH.==. True)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverOperatorAssociationTTable)
