module Storage.Queries.FarePolicy where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id (Id, cast)
import Beckn.Types.Schema
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKMRate
import qualified Types.Domain.FarePolicy as D
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy as Storage
import qualified Types.Storage.FarePolicy.PerExtraKmRate as SExtraKmRate
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
  m (Maybe D.FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ = do
  dbTable <- getDbTable
  DB.runSqlDBTransaction $ do
    sFarePolicy <- DB.findOne' dbTable predicate
    traverse buildDomainFarePolicy sFarePolicy
  where
    predicate Storage.FarePolicy {..} =
      organizationId ==. B.val_ orgId
        &&. vehicleVariant ==. B.val_ vehicleVariant_

findFarePoliciesByOrgId :: DBFlow m r => Id Organization.Organization -> m [D.FarePolicy]
findFarePoliciesByOrgId orgId = do
  dbTable <- getDbTable
  DB.runSqlDBTransaction $ do
    sFarePolicyList <- DB.findAll' dbTable (B.orderBy_ orderByAsc) predicate
    traverse buildDomainFarePolicy sFarePolicyList
  where
    orderByAsc Storage.FarePolicy {..} = B.asc_ vehicleVariant
    predicate Storage.FarePolicy {..} = organizationId ==. B.val_ orgId

findFarePolicyById :: DBFlow m r => Id D.FarePolicy -> m (Maybe D.FarePolicy)
findFarePolicyById fpId = do
  dbTable <- getDbTable
  DB.runSqlDBTransaction $ do
    sFarePolicy <- DB.findOne' dbTable (predicate $ cast fpId)
    traverse buildDomainFarePolicy sFarePolicy
  where
    predicate fpId_ Storage.FarePolicy {..} = id ==. B.val_ fpId_

updateFarePolicy :: DBFlow m r => D.FarePolicy -> m ()
updateFarePolicy farePolicy = do
  dbTable <- getDbTable
  now <- getCurrentTime
  sExtraKmRateList <- traverse buildStoragePerExtraKmRate farePolicy.perExtraKmRateList
  DB.runSqlDBTransaction $ do
    QExtraKMRate.deleteAll farePolicy.organizationId farePolicy.vehicleVariant
    traverse_ QExtraKMRate.create sExtraKmRateList
    DB.update' dbTable (setClause farePolicy now) (predicate $ cast farePolicy.id)
  where
    setClause fp now Storage.FarePolicy {..} =
      mconcat
        [ baseFare <-. B.val_ (fromRational <$> fp.baseFare),
          baseDistance <-. B.val_ (fromRational <$> fp.baseDistance),
          nightShiftStart <-. B.val_ (fp.nightShiftStart),
          nightShiftEnd <-. B.val_ (fp.nightShiftEnd),
          nightShiftRate <-. B.val_ (fromRational <$> fp.nightShiftRate),
          updatedAt <-. B.val_ now
        ]
    predicate fpId Storage.FarePolicy {..} = id ==. B.val_ fpId
    buildStoragePerExtraKmRate :: MonadFlow m => D.PerExtraKmRate -> m SExtraKmRate.FarePolicyPerExtraKmRate
    buildStoragePerExtraKmRate dExtraKmRate = do
      uuid <- generateGUID
      return $
        SExtraKmRate.FarePolicyPerExtraKmRate
          { id = uuid,
            organizationId = farePolicy.organizationId,
            vehicleVariant = farePolicy.vehicleVariant,
            extraDistanceRangeStart = fromRational dExtraKmRate.extraDistanceRangeStart,
            extraFare = fromRational dExtraKmRate.extraFare
          }

buildDomainFarePolicy :: Storage.FarePolicy -> DB.SqlDB D.FarePolicy
buildDomainFarePolicy sFarePolicy = do
  sExtraKmRate <- QExtraKMRate.findAll sFarePolicy.organizationId sFarePolicy.vehicleVariant
  return $ fromTable sFarePolicy sExtraKmRate

fromTable :: Storage.FarePolicy -> [SExtraKmRate.FarePolicyPerExtraKmRate] -> D.FarePolicy
fromTable sFarePolicy extraKmRateList =
  D.FarePolicy
    { id = cast sFarePolicy.id,
      vehicleVariant = sFarePolicy.vehicleVariant,
      organizationId = sFarePolicy.organizationId,
      baseFare = toRational <$> sFarePolicy.baseFare,
      baseDistance = toRational <$> sFarePolicy.baseDistance,
      perExtraKmRateList = extraKmRateFromTable <$> extraKmRateList,
      nightShiftStart = sFarePolicy.nightShiftStart,
      nightShiftEnd = sFarePolicy.nightShiftEnd,
      nightShiftRate = toRational <$> sFarePolicy.nightShiftRate
    }
  where
    extraKmRateFromTable sExtraKmRate =
      D.PerExtraKmRate
        { extraDistanceRangeStart = toRational $ sExtraKmRate.extraDistanceRangeStart,
          extraFare = toRational $ sExtraKmRate.extraFare
        }