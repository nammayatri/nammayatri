{-# LANGUAGE TypeApplications #-}

module Storage.Queries.ProductInstance where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.API.ProductInstance
import qualified Types.Storage.Case as Case
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org
import Types.Storage.Person (Person)
import qualified Types.Storage.ProductInstance as Storage
import Types.Storage.Products
import qualified Types.Storage.Products as Product
import Types.Storage.Vehicle (Vehicle)

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductInstanceT))
getDbTable = DB.productInstance . DB.transporterDb <$> getSchemaName

getCsTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Case.CaseT))
getCsTable =
  DB._case . DB.transporterDb <$> getSchemaName

getProdTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Product.ProductsT))
getProdTable =
  DB.products . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.ProductInstance -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.ProductInstance -> DB.SqlDB ()
create productInstance = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression productInstance)

findAllByIds :: DBFlow m r => Integer -> Integer -> [Id Product.Products] -> m [Storage.ProductInstance]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ createdAt
    predicate Storage.ProductInstance {..} =
      B.in_ productId (B.val_ <$> ids)

findAllByCaseId :: DBFlow m r => Id Case.Case -> m [Storage.ProductInstance]
findAllByCaseId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = caseId ==. B.val_ piId

findByCaseId :: DBFlow m r => Id Case.Case -> m (Maybe Storage.ProductInstance)
findByCaseId piId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} = caseId ==. B.val_ piId

findById' :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findById' productInstanceId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      id ==. B.val_ productInstanceId

findAllByCaseId' :: DBFlow m r => Id Case.Case -> m [Storage.ProductInstance]
findAllByCaseId' caseId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      caseId ==. B.val_ caseId_

findAllByIds' :: DBFlow m r => [Id Storage.ProductInstance] -> m [Storage.ProductInstance]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ id (B.val_ <$> ids)

updateStatusForProducts :: DBFlow m r => Id Product.Products -> Storage.ProductInstanceStatus -> m ()
updateStatusForProducts productId_ status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate productId_)
  where
    predicate pId Storage.ProductInstance {..} = productId ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  m ()
updateStatusFlow prodInstId status = DB.runSqlDB (updateStatus prodInstId status)

updateStatus ::
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatus prodInstId status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.ProductInstance {..} =
      id ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

findAllByCaseIds :: DBFlow m r => [Id Case.Case] -> m [Storage.ProductInstance]
findAllByCaseIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ caseId (B.val_ <$> ids)

updateStatusByIdsFlow ::
  DBFlow m r =>
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  m ()
updateStatusByIdsFlow ids status =
  DB.runSqlDB (updateStatusByIds ids status)

updateStatusByIds ::
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatusByIds ids status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = B.in_ id (B.val_ <$> pids)
    setClause scStatus currTime' Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime',
          status <-. B.val_ scStatus
        ]

updateCaseId ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Id Case.Case ->
  m ()
updateCaseId prodInstId caseId_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause caseId_ currTime)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause scCaseId currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          caseId <-. B.val_ scCaseId
        ]

updateDistance ::
  Id Person ->
  Double ->
  DB.SqlDB ()
updateDistance driverId distance' = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause distance')
    (predicate driverId)
  where
    predicate driverId' Storage.ProductInstance {..} =
      personId ==. B.val_ (Just driverId')
        &&. status ==. B.val_ Storage.INPROGRESS
    setClause distance'' Storage.ProductInstance {..} = distance <-. B.current_ distance + B.val_ distance''

findAllByProdId :: DBFlow m r => Id Product.Products -> m [Storage.ProductInstance]
findAllByProdId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = productId ==. B.val_ piId

findAllByStatusParentId :: DBFlow m r => [Storage.ProductInstanceStatus] -> Id Storage.ProductInstance -> m [Storage.ProductInstance]
findAllByStatusParentId status_ piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      status `B.in_` (B.val_ <$> status_)
        &&. parentId ==. B.val_ (Just piId)

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

productInstancejoinQuery ::
  ( B.Database be db,
    B.HasSqlEqualityCheck be (Id Case.Case),
    B.HasSqlEqualityCheck be (Id Product)
  ) =>
  B.DatabaseEntity be db (B.TableEntity Case.CaseT) ->
  B.DatabaseEntity be db (B.TableEntity ProductsT) ->
  B.DatabaseEntity be db (B.TableEntity Storage.ProductInstanceT) ->
  (Case.CaseT (B.QExpr be s) -> B.QExpr be s Bool) ->
  (ProductsT (B.QExpr be s) -> B.QExpr be s Bool) ->
  (Storage.ProductInstanceT (B.QExpr be s) -> B.QExpr be s Bool) ->
  B.Q
    be
    db
    s
    ( Case.CaseT (B.QExpr be s),
      ProductsT (B.QExpr be s),
      Storage.ProductInstanceT (B.QExpr be s)
    )
productInstancejoinQuery tbl1 tbl2 tbl3 pred1 pred2 pred3 = do
  i <- B.filter_ pred1 $ B.all_ tbl1
  j <- B.filter_ pred2 $ B.all_ tbl2
  k <- B.filter_ pred3 $
    B.join_ tbl3 $
      \line ->
        Case.CasePrimaryKey (Storage.caseId line) B.==. B.primaryKey i
          B.&&. ProductsPrimaryKey (Storage.productId line) B.==. B.primaryKey j
  pure (i, j, k)

productInstanceJoin :: DBFlow m r => Int -> Int -> [Case.CaseType] -> Id Org.Organization -> [Storage.ProductInstanceStatus] -> m ProductInstanceList
productInstanceJoin limit_ offset_ csTypes orgId status_ = do
  dbTable <- getDbTable
  prodTable <- getProdTable
  csTable <- getCsTable
  joinedValues <-
    DB.findAllByJoin
      (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
      (productInstancejoinQuery csTable prodTable dbTable csPred prodPred piPred)
  return $ mkJoinRes <$> joinedValues
  where
    limit = toInteger limit_
    offset = toInteger offset_
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ createdAt
    csPred Case.Case {..} =
      _type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes
    prodPred Product.Products {..} = B.val_ True
    piPred Storage.ProductInstance {..} =
      organizationId ==. B.val_ orgId
        &&. (status `B.in_` (B.val_ <$> status_) ||. complementVal status_)
    mkJoinRes (cs, pr, cpr) =
      ProductInstanceRes
        { _case = cs,
          product = pr,
          productInstance = cpr,
          fromLocation = Nothing,
          toLocation = Nothing
        }

productInstanceJoinWithoutLimits :: DBFlow m r => Case.CaseType -> Id Org.Organization -> [Storage.ProductInstanceStatus] -> m ProductInstanceList
productInstanceJoinWithoutLimits csType orgId status_ = do
  dbTable <- getDbTable
  csTable <- getCsTable
  prodTable <- getProdTable
  joinedValues <-
    DB.findAllByJoin
      (B.orderBy_ orderByDesc)
      (productInstancejoinQuery csTable prodTable dbTable csPred prodPred cprPred)
  return $ mkJoinRes <$> joinedValues
  where
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ createdAt
    csPred Case.Case {..} =
      _type ==. B.val_ csType
    prodPred Product.Products {..} = B.val_ True
    cprPred Storage.ProductInstance {..} =
      organizationId ==. B.val_ orgId
        &&. (status `B.in_` (B.val_ <$> status_) ||. complementVal status_)
    mkJoinRes (cs, pr, cpr) =
      ProductInstanceRes
        { _case = cs,
          product = pr,
          productInstance = cpr,
          fromLocation = Nothing,
          toLocation = Nothing
        }

findById :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} = id ==. B.val_ pid

updateDriverFlow :: DBFlow m r => [Id Storage.ProductInstance] -> Maybe (Id Person) -> m ()
updateDriverFlow ids driverId =
  DB.runSqlDB (updateDriver ids driverId)

updateDriver ::
  [Id Storage.ProductInstance] ->
  Maybe (Id Person) ->
  DB.SqlDB ()
updateDriver ids driverId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause driverId now)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = id `B.in_` (B.val_ <$> pids)
    setClause sDriverId currTime Storage.ProductInstance {..} =
      mconcat
        [ personId <-. B.val_ sDriverId,
          personUpdatedAt <-. B.val_ (Just currTime),
          updatedAt <-. B.val_ currTime
        ]

updateVehicleFlow :: DBFlow m r => [Id Storage.ProductInstance] -> Maybe Text -> m ()
updateVehicleFlow ids vehId = do
  DB.runSqlDB (updateVehicle ids (Id <$> vehId))

updateVehicle ::
  [Id Storage.ProductInstance] ->
  Maybe (Id Vehicle) ->
  DB.SqlDB ()
updateVehicle ids vehId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause (getId <$> vehId) now)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = id `B.in_` (B.val_ <$> pids)
    setClause vehicleId currTime' Storage.ProductInstance {..} =
      mconcat
        [ entityId <-. B.val_ vehicleId,
          updatedAt <-. B.val_ currTime'
        ]

updateInfo :: Id Storage.ProductInstance -> Text -> DB.SqlDB ()
updateInfo prodInstId info_ = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info_)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause pInfo Storage.ProductInstance {..} =
      mconcat
        [info <-. B.val_ (Just pInfo)]

updateActualPrice :: Amount -> Id Storage.ProductInstance -> DB.SqlDB ()
updateActualPrice price' prodInstId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause price' now)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause price'' currTime Storage.ProductInstance {..} =
      mconcat
        [ actualPrice <-. B.val_ (Just price''),
          updatedAt <-. B.val_ currTime
        ]

findAllByVehicleId :: DBFlow m r => Maybe (Id Vehicle) -> m [Storage.ProductInstance]
findAllByVehicleId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = B.val_ (isJust piId) &&. entityId ==. B.val_ (getId <$> piId)

findAllByPersonId :: DBFlow m r => Id Person -> m [Storage.ProductInstance]
findAllByPersonId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = personId ==. B.val_ (Just piId)

findAllByParentId :: DBFlow m r => Id Storage.ProductInstance -> m [Storage.ProductInstance]
findAllByParentId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = parentId ==. B.val_ (Just piId)

findOrderPIByParentId :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findOrderPIByParentId pid = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate pid)
  where
    predicate piid Storage.ProductInstance {..} =
      parentId ==. B.val_ (Just piid)
        &&. _type ==. B.val_ Case.RIDEORDER

findByIdType :: DBFlow m r => [Id Storage.ProductInstance] -> Case.CaseType -> m (Maybe Storage.ProductInstance)
findByIdType ids csType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      id `B.in_` (B.val_ <$> ids)
        &&. _type ==. B.val_ csType

findByParentIdType :: DBFlow m r => Id Storage.ProductInstance -> Case.CaseType -> m (Maybe Storage.ProductInstance)
findByParentIdType mparentId csType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      parentId ==. B.val_ (Just mparentId)
        &&. _type ==. B.val_ csType

findAllExpiredByStatus :: DBFlow m r => [Storage.ProductInstanceStatus] -> UTCTime -> m [Storage.ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. startTime B.<=. B.val_ expiryTime

getCountByStatus :: DBFlow m r => Id Org.Organization -> Storage.ProductInstanceType -> m [(Storage.ProductInstanceStatus, Int)]
getCountByStatus orgId piType = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.aggregate_ aggregator) predicate
  where
    aggregator Storage.ProductInstance {..} = (B.group_ status, B.as_ @Int B.countAll_)
    predicate Storage.ProductInstance {..} =
      organizationId ==. B.val_ orgId
        &&. _type ==. B.val_ piType

findByStartTimeBuffer ::
  DBFlow m r =>
  Storage.ProductInstanceType ->
  UTCTime ->
  NominalDiffTime ->
  [Storage.ProductInstanceStatus] ->
  m [Storage.ProductInstance]
findByStartTimeBuffer piType startTime_ buffer statuses = do
  dbTable <- getDbTable
  let fromTime = addUTCTime (- buffer * 60 * 60) startTime_
  let toTime = addUTCTime (buffer * 60 * 60) startTime_
  DB.findAll dbTable identity (predicate fromTime toTime)
  where
    predicate fromTime toTime Storage.ProductInstance {..} =
      let inStatus = fmap B.val_ statuses
       in _type ==. B.val_ piType
            &&. startTime B.<=. B.val_ toTime
            &&. startTime B.>=. B.val_ fromTime
            &&. status `B.in_` inStatus

getDriverCompletedRides :: DBFlow m r => Id Person -> UTCTime -> UTCTime -> m [Storage.ProductInstance]
getDriverCompletedRides driverId fromTime toTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      _type ==. B.val_ Case.RIDEORDER
        &&. personId ==. B.val_ (Just driverId)
        &&. status ==. B.val_ Storage.COMPLETED
        &&. startTime B.>=. B.val_ fromTime
        &&. startTime B.<=. B.val_ toTime
