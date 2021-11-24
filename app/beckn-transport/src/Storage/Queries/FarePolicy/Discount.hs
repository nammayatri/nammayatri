module Storage.Queries.FarePolicy.Discount where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id (Id, cast)
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Domain.FarePolicy.Discount as D
import qualified Types.Storage.DB as DB
import qualified Types.Storage.FarePolicy.Discount as Storage
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.FarePolicyDiscountT))
getDbTable =
  DB.farePolicyDiscount . DB.transporterDb <$> getSchemaName

create :: D.Discount -> DB.SqlDB ()
create disc = do
  dbTable <- getDbTable
  let storageDisc = toTable disc
  DB.createOne' dbTable (Storage.insertValue storageDisc)

findById ::
  DBFlow m r =>
  Id D.Discount ->
  m (Maybe D.Discount)
findById discId = do
  dbTable <- getDbTable
  map fromTable <$> DB.findOne dbTable (predicate (cast discId))
  where
    predicate discId_ Storage.FarePolicyDiscount {..} =
      id ==. B.val_ discId_

findAllFlow ::
  DBFlow m r =>
  Id Organization.Organization ->
  Vehicle.Variant ->
  m [D.Discount]
findAllFlow orgId vehicleVariant_ = do
  discountList <- DB.runSqlDB $ findAll orgId vehicleVariant_
  pure $ map fromTable discountList

findAll ::
  Id Organization.Organization ->
  Vehicle.Variant ->
  DB.SqlDB [Storage.FarePolicyDiscount]
findAll orgId vehicleVariant_ = do
  dbTable <- getDbTable
  DB.findAll' dbTable identity predicate
  where
    predicate Storage.FarePolicyDiscount {..} =
      organizationId ==. B.val_ orgId
        &&. vehicleVariant ==. B.val_ vehicleVariant_

update :: Id D.Discount -> D.Discount -> DB.SqlDB ()
update discId disc = do
  dbTable <- getDbTable
  let storageDisc = toTable disc
  currTime <- getCurrentTime
  DB.update'
    dbTable
    (setClause storageDisc currTime)
    (predicate $ cast discId)
  where
    predicate discId_ Storage.FarePolicyDiscount {..} = id ==. B.val_ discId_
    setClause storageDisc currTime Storage.FarePolicyDiscount {..} =
      mconcat
        [ fromDate <-. B.val_ storageDisc.fromDate,
          toDate <-. B.val_ storageDisc.toDate,
          enabled <-. B.val_ storageDisc.enabled,
          discount <-. B.val_ storageDisc.discount,
          updatedAt <-. B.val_ currTime
        ]

deleteById :: Id D.Discount -> DB.SqlDB ()
deleteById discId = do
  dbTable <- getDbTable
  DB.delete' dbTable $ predicate (cast discId)
  where
    predicate discId_ Storage.FarePolicyDiscount {..} =
      id ==. B.val_ discId_

fromTable :: Storage.FarePolicyDiscount -> D.Discount
fromTable Storage.FarePolicyDiscount {..} =
  D.Discount
    { id = cast id,
      discount = toRational discount,
      ..
    }

toTable :: D.Discount -> Storage.FarePolicyDiscount
toTable D.Discount {..} =
  Storage.FarePolicyDiscount
    { id = cast id,
      discount = fromRational discount,
      ..
    }