{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.FarePolicy where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Domain.FarePolicy as D
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy as Storage
import Utils.Common

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.FarePolicyT))
getDbTable =
  DB._farePolicy . DB.transporterDb <$> getSchemaName

create :: Storage.FarePolicy -> Flow ()
create Storage.FarePolicy {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.FarePolicy {..})
    >>= checkDBError

findFarePolicyByOrgAndVehicleVariant ::
  Id Organization.Organization -> Vehicle.Variant -> Flow (Maybe Storage.FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Storage.FarePolicy {..} =
      _organizationId ==. B.val_ orgId
        &&. _vehicleVariant ==. B.val_ vehicleVariant

findFarePoliciesByOrgId :: Id Organization.Organization -> Flow [Storage.FarePolicy]
findFarePoliciesByOrgId orgId = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= checkDBError
  where
    predicate Storage.FarePolicy {..} = _organizationId ==. B.val_ orgId

findFarePolicyById :: Id D.FarePolicy -> Flow (Maybe Storage.FarePolicy)
findFarePolicyById fpId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Storage.FarePolicy {..} = _id ==. B.val_ fpId

updateFarePolicy :: Storage.FarePolicy -> Flow ()
updateFarePolicy farePolicy = do
  dbTable <- getDbTable
  now <- getCurrentTime
  let farePolicyId = farePolicy ^. #_id
  DB.update dbTable (setClause farePolicy now) (predicate farePolicyId)
    >>= checkDBError
  where
    setClause fp now Storage.FarePolicy {..} =
      mconcat
        [ _baseFare <-. B.val_ (fp ^. #_baseFare),
          _baseDistance <-. B.val_ (fp ^. #_baseDistance),
          _perExtraKmRate <-. B.val_ (fp ^. #_perExtraKmRate),
          _nightShiftStart <-. B.val_ (fp ^. #_nightShiftStart),
          _nightShiftEnd <-. B.val_ (fp ^. #_nightShiftEnd),
          _nightShiftRate <-. B.val_ (fp ^. #_nightShiftRate),
          _updatedAt <-. B.val_ now
        ]
    predicate id Storage.FarePolicy {..} = _id ==. B.val_ id
