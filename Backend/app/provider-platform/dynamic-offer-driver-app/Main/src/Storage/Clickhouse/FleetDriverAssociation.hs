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
    isActive :: C f Bool,
    associatedOn :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show FleetDriverAssociation

fleetDriverAssociationTTable :: FleetDriverAssociationT (FieldModification FleetDriverAssociationT)
fleetDriverAssociationTTable =
  FleetDriverAssociationT
    { id = "id",
      driverId = "driver_id",
      fleetOwnerId = "fleet_owner_id",
      isActive = "is_active",
      associatedOn = "associated_on"
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
        (\assoc -> assoc.fleetOwnerId CH.==. fleetOwnerId CH.&&. assoc.isActive CH.==. True)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetDriverAssociationTTable)

getTotalDriverCountByFleetOwnerIdsInDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  UTCTime ->
  UTCTime ->
  m Int
getTotalDriverCountByFleetOwnerIdsInDateRange fleetOwnerIds from to = do
  res <-
    CH.findAll $
      CH.select_ (\assoc -> CH.aggregate $ CH.count_ (assoc.driverId)) $
        CH.filter_
          (\assoc -> assoc.fleetOwnerId `CH.in_` fleetOwnerIds CH.&&. assoc.isActive CH.==. True CH.&&. assoc.associatedOn CH.>=. from CH.&&. assoc.associatedOn CH.<=. to)
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetDriverAssociationTTable)
  pure $ fromMaybe 0 (listToMaybe res)

getDriverIdsByFleetOwnerIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  m [Id DP.Person]
getDriverIdsByFleetOwnerIds fleetOwnerIds =
  CH.findAll $
    CH.select_ (\assoc -> CH.notGrouped (assoc.driverId)) $
      CH.filter_
        (\assoc -> (assoc.fleetOwnerId `CH.in_` fleetOwnerIds) CH.&&. (assoc.isActive CH.==. True))
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetDriverAssociationTTable)
