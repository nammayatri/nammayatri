{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Storage.Queries.ProductInstance where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as Storage
import Beckn.Types.Storage.Products
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.API.ProductInstance
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductInstanceT))
getDbTable =
  DB._productInstance . DB.transporterDb <$> getSchemaName

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
    >>= either DB.throwDBError pure

findAllByIds :: Integer -> Integer -> [ProductsId] -> Flow [Storage.ProductInstance]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ _createdAt
    predicate Storage.ProductInstance {..} =
      B.in_ _productId (B.val_ <$> ids)

findAllByCaseId :: CaseId -> Flow [Storage.ProductInstance]
findAllByCaseId id = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _caseId ==. B.val_ id

findByCaseId :: CaseId -> Flow Storage.ProductInstance
findByCaseId id = do
  dbTable <- getDbTable
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _caseId ==. B.val_ id

findById' :: ProductInstanceId -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findById' productInstanceId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _id ==. B.val_ productInstanceId

findAllByCaseId' :: CaseId -> Flow (T.DBResult [Storage.ProductInstance])
findAllByCaseId' caseId = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _caseId ==. B.val_ caseId

findAllByIds' :: [ProductInstanceId] -> Flow (T.DBResult [Storage.ProductInstance])
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ _id (B.val_ <$> ids)

updateStatusForProducts :: ProductsId -> Storage.ProductInstanceStatus -> Flow (T.DBResult ())
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

updateStatus ::
  ProductInstanceId ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatus prodInstId status = do
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

findAllByCaseIds :: [CaseId] -> Flow [Storage.ProductInstance]
findAllByCaseIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.ProductInstance {..} =
      B.in_ _caseId (B.val_ <$> ids)

updateStatusByIds ::
  [ProductInstanceId] ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatusByIds ids status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = B.in_ _id (B.val_ <$> pids)
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]

updateCaseId ::
  ProductInstanceId ->
  CaseId ->
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

findAllByProdId :: ProductsId -> Flow [Storage.ProductInstance]
findAllByProdId id = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _productId ==. B.val_ id

findAllByStatusParentId :: [Storage.ProductInstanceStatus] -> Maybe ProductInstanceId -> Flow [Storage.ProductInstance]
findAllByStatusParentId status id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.ProductInstance {..} =
      _status `B.in_` (B.val_ <$> status)
        &&. B.val_ (isJust id)
        &&. _parentId ==. B.val_ id

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

productInstancejoinQuery tbl1 tbl2 tbl3 pred1 pred2 pred3 = do
  i <- B.filter_ pred1 $ B.all_ tbl1
  j <- B.filter_ pred2 $ B.all_ tbl2
  k <- B.filter_ pred3 $
    B.join_ tbl3 $
      \line ->
        CasePrimaryKey (Storage._caseId line) B.==. B.primaryKey i
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
      >>= either DB.throwDBError pure
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
      >>= either DB.throwDBError pure
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

findById :: ProductInstanceId -> Flow Storage.ProductInstance
findById pid = do
  dbTable <- getDbTable
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _id ==. B.val_ pid

updateDriver :: [ProductInstanceId] -> Maybe PersonId -> Flow ()
updateDriver ids driverId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause driverId currTime)
    (predicate ids)
    >>= either DB.throwDBError pure
  where
    predicate pids Storage.ProductInstance {..} = _id `B.in_` (B.val_ <$> pids)
    setClause sDriverId currTime Storage.ProductInstance {..} =
      mconcat
        [ _personId <-. B.val_ sDriverId,
          _personUpdatedAt <-. B.val_ (Just currTime),
          _updatedAt <-. B.val_ currTime
        ]

updateVehicle :: [ProductInstanceId] -> Maybe Text -> Flow ()
updateVehicle ids vehId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause vehId currTime)
    (predicate ids)
    >>= either DB.throwDBError pure
  where
    predicate pids Storage.ProductInstance {..} = _id `B.in_` (B.val_ <$> pids)
    setClause vehicleId currTime Storage.ProductInstance {..} =
      mconcat
        [ _entityId <-. B.val_ vehicleId,
          _updatedAt <-. B.val_ currTime
        ]

updateInfo :: ProductInstanceId -> Maybe Text -> Flow ()
updateInfo prodInstId info = do
  dbTable <- getDbTable
  DB.update
    dbTable
    (setClause info)
    (predicate prodInstId)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause pInfo Storage.ProductInstance {..} =
      mconcat
        [_info <-. B.val_ pInfo]

findAllByVehicleId :: Maybe Text -> Flow [Storage.ProductInstance]
findAllByVehicleId id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.ProductInstance {..} = B.val_ (isJust id) &&. _entityId ==. B.val_ id

findAllByPersonId :: PersonId -> Flow [Storage.ProductInstance]
findAllByPersonId id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.ProductInstance {..} = _personId ==. B.val_ (Just id)

findAllByParentId :: Maybe ProductInstanceId -> Flow [Storage.ProductInstance]
findAllByParentId id = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.ProductInstance {..} = B.val_ (isJust id) &&. _parentId ==. B.val_ id

findByIdType :: [ProductInstanceId] -> Case.CaseType -> Flow Storage.ProductInstance
findByIdType ids csType = do
  dbTable <- getDbTable
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _id `B.in_` (B.val_ <$> ids)
        &&. _type ==. B.val_ csType

findByParentIdType :: Maybe ProductInstanceId -> Case.CaseType -> Flow Storage.ProductInstance
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

findByTypeAndStatuses :: Storage.ProductInstanceType -> [Storage.ProductInstanceStatus] -> Flow [Storage.ProductInstance]
findByTypeAndStatuses piType statuses = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate >>= checkDBError
  where
    predicate Storage.ProductInstance {..} =
      _type ==. B.val_ piType
        &&. (_status `B.in_` (B.val_ <$> statuses) ||. complementVal statuses)

getDriverRides :: PersonId -> UTCTime -> UTCTime -> Flow [Storage.ProductInstance]
getDriverRides driverId fromTime toTime = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate >>= checkDBError
  where
    predicate Storage.ProductInstance {..} =
      _type ==. B.val_ Case.RIDEORDER
        &&. _personId ==. B.val_ (Just driverId)
        &&. _status ==. B.val_ Storage.COMPLETED
        &&. _startTime B.>=. B.val_ fromTime
        &&. _startTime B.<=. B.val_ toTime
