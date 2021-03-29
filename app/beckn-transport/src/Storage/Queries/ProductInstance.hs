{-# LANGUAGE TypeApplications #-}

module Storage.Queries.ProductInstance where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person (Person)
import qualified Beckn.Types.Storage.ProductInstance as Storage
import Beckn.Types.Storage.Products
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Types.Storage.Vehicle (Vehicle)
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.API.ProductInstance
import qualified Types.Storage.DB as DB

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductInstanceT))
getDbTable = DB._productInstance . DB.transporterDb <$> getSchemaName

getCsTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Case.CaseT))
getCsTable =
  DB._case . DB.transporterDb <$> getSchemaName

getProdTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Product.ProductsT))
getProdTable =
  DB._products . DB.transporterDb <$> getSchemaName

create :: Storage.ProductInstance -> Flow ()
create Storage.ProductInstance {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.ProductInstance {..})
    >>= either throwDBError pure

findAllByIds :: Integer -> Integer -> [Id Product.Products] -> Flow [Storage.ProductInstance]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= either throwDBError pure
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ _createdAt
    predicate Storage.ProductInstance {..} =
      B.in_ _productId (B.val_ <$> ids)

findAllByCaseId :: Id Case.Case -> Flow [Storage.ProductInstance]
findAllByCaseId id = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _caseId ==. B.val_ id

findByCaseId :: Id Case.Case -> Flow Storage.ProductInstance
findByCaseId id = do
  dbTable <- getDbTable
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _caseId ==. B.val_ id

findById' :: Id Storage.ProductInstance -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findById' productInstanceId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _id ==. B.val_ productInstanceId

findAllByCaseId' :: Id Case.Case -> Flow (T.DBResult [Storage.ProductInstance])
findAllByCaseId' caseId = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _caseId ==. B.val_ caseId

findAllByIds' :: [Id Storage.ProductInstance] -> Flow (T.DBResult [Storage.ProductInstance])
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ _id (B.val_ <$> ids)

updateStatusForProducts :: Id Product.Products -> Storage.ProductInstanceStatus -> Flow (T.DBResult ())
updateStatusForProducts productId status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate productId)
  where
    predicate pId Storage.ProductInstance {..} = _productId ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]

updateStatusFlow ::
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatusFlow prodInstId status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.ProductInstance {..} =
      _id ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]

updateStatus ::
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatus prodInstId status = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.ProductInstance {..} =
      _id ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]

findAllByCaseIds :: [Id Case.Case] -> Flow [Storage.ProductInstance]
findAllByCaseIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either throwDBError pure
  where
    predicate Storage.ProductInstance {..} =
      B.in_ _caseId (B.val_ <$> ids)

updateStatusByIdsFlow ::
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatusByIdsFlow ids status =
  DB.runSqlDB (updateStatusByIds ids status)

updateStatusByIds ::
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatusByIds ids status = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status currTime)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = B.in_ _id (B.val_ <$> pids)
    setClause scStatus currTime' Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime',
          _status <-. B.val_ scStatus
        ]

updateCaseId ::
  Id Storage.ProductInstance ->
  Id Case.Case ->
  Flow (T.DBResult ())
updateCaseId prodInstId caseId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause caseId currTime)
    (predicate prodInstId)
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause scCaseId currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _caseId <-. B.val_ scCaseId
        ]

findAllByProdId :: Id Product.Products -> Flow [Storage.ProductInstance]
findAllByProdId id = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _productId ==. B.val_ id

findAllByStatusParentId :: [Storage.ProductInstanceStatus] -> Maybe (Id Storage.ProductInstance) -> Flow [Storage.ProductInstance]
findAllByStatusParentId status id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either throwDBError pure
  where
    predicate Storage.ProductInstance {..} =
      _status `B.in_` (B.val_ <$> status)
        &&. B.val_ (isJust id)
        &&. _parentId ==. B.val_ id

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
        Case.CasePrimaryKey (Storage._caseId line) B.==. B.primaryKey i
          B.&&. ProductsPrimaryKey (Storage._productId line) B.==. B.primaryKey j
  pure (i, j, k)

productInstanceJoin :: Int -> Int -> [Case.CaseType] -> Text -> [Storage.ProductInstanceStatus] -> Flow ProductInstanceList
productInstanceJoin _limit _offset csTypes orgId status = do
  dbTable <- getDbTable
  prodTable <- getProdTable
  csTable <- getCsTable
  joinedValues <-
    DB.findAllByJoin
      limit
      offset
      orderByDesc
      (productInstancejoinQuery csTable prodTable dbTable csPred prodPred piPred)
      >>= either throwDBError pure
  return $ mkJoinRes <$> joinedValues
  where
    limit = toInteger _limit
    offset = toInteger _offset
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ _createdAt
    csPred Case.Case {..} =
      _type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes
    prodPred Product.Products {..} = B.val_ True
    piPred Storage.ProductInstance {..} =
      _organizationId ==. B.val_ orgId
        &&. (_status `B.in_` (B.val_ <$> status) ||. complementVal status)
    mkJoinRes (cs, pr, cpr) =
      ProductInstanceRes
        { _case = cs,
          _product = pr,
          _productInstance = cpr,
          _fromLocation = Nothing,
          _toLocation = Nothing
        }

productInstanceJoinWithoutLimits :: Case.CaseType -> Text -> [Storage.ProductInstanceStatus] -> Flow ProductInstanceList
productInstanceJoinWithoutLimits csType orgId status = do
  dbTable <- getDbTable
  csTable <- getCsTable
  prodTable <- getProdTable
  joinedValues <-
    DB.findAllByJoinWithoutLimits
      orderByDesc
      (productInstancejoinQuery csTable prodTable dbTable csPred prodPred cprPred)
      >>= either throwDBError pure
  return $ mkJoinRes <$> joinedValues
  where
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ _createdAt
    csPred Case.Case {..} =
      _type ==. B.val_ csType
    prodPred Product.Products {..} = B.val_ True
    cprPred Storage.ProductInstance {..} =
      _organizationId ==. B.val_ orgId
        &&. (_status `B.in_` (B.val_ <$> status) ||. complementVal status)
    mkJoinRes (cs, pr, cpr) =
      ProductInstanceRes
        { _case = cs,
          _product = pr,
          _productInstance = cpr,
          _fromLocation = Nothing,
          _toLocation = Nothing
        }

findById :: Id Storage.ProductInstance -> Flow Storage.ProductInstance
findById pid = do
  dbTable <- getDbTable
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _id ==. B.val_ pid

updateDriverFlow :: [Id Storage.ProductInstance] -> Maybe (Id Person) -> Flow ()
updateDriverFlow ids driverId =
  DB.runSqlDB (updateDriver ids driverId)
    >>= either throwDBError pure

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
    predicate pids Storage.ProductInstance {..} = _id `B.in_` (B.val_ <$> pids)
    setClause sDriverId currTime Storage.ProductInstance {..} =
      mconcat
        [ _personId <-. B.val_ sDriverId,
          _personUpdatedAt <-. B.val_ (Just currTime),
          _updatedAt <-. B.val_ currTime
        ]

updateVehicleFlow :: [Id Storage.ProductInstance] -> Maybe Text -> Flow ()
updateVehicleFlow ids vehId = do
  DB.runSqlDB (updateVehicle ids (Id <$> vehId))
    >>= either throwDBError pure

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
    predicate pids Storage.ProductInstance {..} = _id `B.in_` (B.val_ <$> pids)
    setClause vehicleId currTime' Storage.ProductInstance {..} =
      mconcat
        [ _entityId <-. B.val_ vehicleId,
          _updatedAt <-. B.val_ currTime'
        ]

updateInfo :: Id Storage.ProductInstance -> Text -> DB.SqlDB ()
updateInfo prodInstId info = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info)
    (predicate prodInstId)
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause pInfo Storage.ProductInstance {..} =
      mconcat
        [_info <-. B.val_ (Just pInfo)]

findAllByVehicleId :: Maybe Text -> Flow [Storage.ProductInstance]
findAllByVehicleId id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either throwDBError pure
  where
    predicate Storage.ProductInstance {..} = B.val_ (isJust id) &&. _entityId ==. B.val_ id

findAllByPersonId :: Id Person -> Flow [Storage.ProductInstance]
findAllByPersonId id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either throwDBError pure
  where
    predicate Storage.ProductInstance {..} = _personId ==. B.val_ (Just id)

findAllByParentId :: Maybe (Id Storage.ProductInstance) -> Flow [Storage.ProductInstance]
findAllByParentId id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either throwDBError pure
  where
    predicate Storage.ProductInstance {..} = B.val_ (isJust id) &&. _parentId ==. B.val_ id

findByIdType :: [Id Storage.ProductInstance] -> Case.CaseType -> Flow Storage.ProductInstance
findByIdType ids csType = do
  dbTable <- getDbTable
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _id `B.in_` (B.val_ <$> ids)
        &&. _type ==. B.val_ csType

findByParentIdType :: Maybe (Id Storage.ProductInstance) -> Case.CaseType -> Flow Storage.ProductInstance
findByParentIdType mparentId csType = do
  dbTable <- getDbTable
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      B.val_ (isJust mparentId) &&. _parentId ==. B.val_ mparentId
        &&. _type ==. B.val_ csType

findAllExpiredByStatus :: [Storage.ProductInstanceStatus] -> UTCTime -> Flow (T.DBResult [Storage.ProductInstance])
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ _status (B.val_ <$> statuses)
        &&. _startTime B.<=. B.val_ expiryTime

getCountByStatus' :: Text -> Storage.ProductInstanceType -> Flow (T.DBResult [(Storage.ProductInstanceStatus, Int)])
getCountByStatus' orgId piType = do
  dbTable <- getDbTable
  DB.aggregate dbTable aggregator predicate
  where
    aggregator Storage.ProductInstance {..} = (B.group_ _status, B.as_ @Int B.countAll_)
    predicate Storage.ProductInstance {..} =
      _organizationId ==. B.val_ orgId
        &&. _type ==. B.val_ piType

findByStartTimeBuffer :: Storage.ProductInstanceType -> UTCTime -> NominalDiffTime -> [Storage.ProductInstanceStatus] -> Flow (T.DBResult [Storage.ProductInstance])
findByStartTimeBuffer piType startTime buffer statuses = do
  dbTable <- getDbTable
  let fromTime = addUTCTime (- buffer * 60 * 60) startTime
  let toTime = addUTCTime (buffer * 60 * 60) startTime
  DB.findAll dbTable (predicate fromTime toTime)
  where
    predicate fromTime toTime Storage.ProductInstance {..} =
      let inStatus = fmap B.val_ statuses
       in _type ==. B.val_ piType
            &&. _startTime B.<=. B.val_ toTime
            &&. _startTime B.>=. B.val_ fromTime
            &&. _status `B.in_` inStatus

getDriverCompletedRides :: Id Person -> UTCTime -> UTCTime -> Flow [Storage.ProductInstance]
getDriverCompletedRides driverId fromTime toTime = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate >>= checkDBError
  where
    predicate Storage.ProductInstance {..} =
      _type ==. B.val_ Case.RIDEORDER
        &&. _personId ==. B.val_ (Just driverId)
        &&. _status ==. B.val_ Storage.COMPLETED
        &&. _startTime B.>=. B.val_ fromTime
        &&. _startTime B.<=. B.val_ toTime
