module Storage.Queries.FarePolicy where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id (Id, cast)
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.FarePolicy.Discount as Discount
import qualified Storage.Queries.FarePolicy.Discount as QDiscount
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKMRate
import qualified Types.Domain.FarePolicy as D
import Types.Error
import qualified Types.Storage.DB as DB
import Types.Storage.FarePolicy
import qualified Types.Storage.FarePolicy as Storage
import qualified Types.Storage.FarePolicy.PerExtraKmRate as SExtraKmRate
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.FarePolicyT))
getDbTable =
  DB.farePolicy . DB.transporterDb <$> getSchemaName

findFarePolicyByOrgAndVehicleVariant ::
  DBFlow m r =>
  Id Organization.Organization ->
  Vehicle.Variant ->
  m (Maybe D.FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ = do
  dbTable <- getDbTable
  farePolicyData <-
    DB.runSqlDB $
      DB.findOne' dbTable predicate
        >>= mapM loadFarePolicyData
  mapM fromTable farePolicyData
  where
    predicate Storage.FarePolicy {..} =
      organizationId ==. B.val_ orgId
        &&. vehicleVariant ==. B.val_ vehicleVariant_

findFarePoliciesByOrgId :: DBFlow m r => Id Organization.Organization -> m [D.FarePolicy]
findFarePoliciesByOrgId orgId = do
  dbTable <- getDbTable
  farePolicyDataList <- DB.runSqlDB $ do
    sFarePolicyList <- DB.findAll' dbTable (B.orderBy_ orderByAsc) predicate
    traverse loadFarePolicyData sFarePolicyList
  mapM fromTable farePolicyDataList
  where
    orderByAsc Storage.FarePolicy {..} = B.asc_ vehicleVariant
    predicate Storage.FarePolicy {..} = organizationId ==. B.val_ orgId

findFarePolicyById :: DBFlow m r => Id D.FarePolicy -> m (Maybe D.FarePolicy)
findFarePolicyById fpId = do
  dbTable <- getDbTable
  farePolicyData <- DB.runSqlDB $ do
    sFarePolicy <- DB.findOne' dbTable (predicate $ cast fpId)
    mapM loadFarePolicyData sFarePolicy
  mapM fromTable farePolicyData
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
            distanceRangeStart = fromRational dExtraKmRate.distanceRangeStart,
            fare = fromRational dExtraKmRate.fare
          }

loadFarePolicyData :: Storage.FarePolicy -> DB.SqlDB FarePolicyData
loadFarePolicyData sFarePolicy = do
  sExtraKmRate <- QExtraKMRate.findAll sFarePolicy.organizationId sFarePolicy.vehicleVariant
  discountList <- QDiscount.findAll sFarePolicy.organizationId sFarePolicy.vehicleVariant
  pure $ FarePolicyData sFarePolicy sExtraKmRate discountList

fromTable :: (Monad m, MonadThrow m, Log m) => FarePolicyData -> m D.FarePolicy
fromTable FarePolicyData {..} = do
  extraKmRates <-
    case extraKmRateList of
      e : es -> pure $ e :| es
      [] -> throwError NoPerExtraKmRate
  pure $
    D.FarePolicy
      { id = cast farePolicy.id,
        vehicleVariant = farePolicy.vehicleVariant,
        organizationId = farePolicy.organizationId,
        baseFare = toRational <$> farePolicy.baseFare,
        perExtraKmRateList = extraKmRateFromTable <$> extraKmRates,
        discountList = map Discount.fromTable discountList,
        nightShiftStart = farePolicy.nightShiftStart,
        nightShiftEnd = farePolicy.nightShiftEnd,
        nightShiftRate = toRational <$> farePolicy.nightShiftRate
      }
  where
    extraKmRateFromTable sExtraKmRate =
      D.PerExtraKmRate
        { distanceRangeStart = toRational $ sExtraKmRate.distanceRangeStart,
          fare = toRational $ sExtraKmRate.fare
        }
