module Storage.Queries.FarePolicy where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Beckn.Types.Schema
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Domain.FarePolicy as D
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy as Storage
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.FarePolicyT))
getDbTable =
  DB.farePolicy . DB.transporterDb <$> getSchemaName

create :: DBFlow m r => Storage.FarePolicy -> m ()
create Storage.FarePolicy {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue Storage.FarePolicy {..})

findFarePolicyByOrgAndVehicleVariant ::
  DBFlow m r =>
  Id Organization.Organization ->
  Vehicle.Variant ->
  m (Maybe Storage.FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.FarePolicy {..} =
      organizationId ==. B.val_ orgId
        &&. vehicleVariant ==. B.val_ vehicleVariant_

findFarePoliciesByOrgId :: DBFlow m r => Id Organization.Organization -> m [Storage.FarePolicy]
findFarePoliciesByOrgId orgId = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.orderBy_ orderByAsc) predicate
  where
    orderByAsc Storage.FarePolicy {..} = B.asc_ vehicleVariant
    predicate Storage.FarePolicy {..} = organizationId ==. B.val_ orgId

findFarePolicyById :: DBFlow m r => Id D.FarePolicy -> m (Maybe Storage.FarePolicy)
findFarePolicyById fpId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.FarePolicy {..} = id ==. B.val_ fpId

updateFarePolicy :: DBFlow m r => Storage.FarePolicy -> m ()
updateFarePolicy farePolicy = do
  dbTable <- getDbTable
  now <- getCurrentTime
  let farePolicyId = farePolicy.id
  DB.update dbTable (setClause farePolicy now) (predicate farePolicyId)
  where
    setClause fp now Storage.FarePolicy {..} =
      mconcat
        [ baseFare <-. B.val_ (fp.baseFare),
          baseDistance <-. B.val_ (fp.baseDistance),
          perExtraKmRate <-. B.val_ (fp.perExtraKmRate),
          nightShiftStart <-. B.val_ (fp.nightShiftStart),
          nightShiftEnd <-. B.val_ (fp.nightShiftEnd),
          nightShiftRate <-. B.val_ (fp.nightShiftRate),
          updatedAt <-. B.val_ now
        ]
    predicate fpId Storage.FarePolicy {..} = id ==. B.val_ fpId
