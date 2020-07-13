module Storage.Queries.ProductInstance where

import App.Types
import Beckn.Types.App
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as Storage
import Beckn.Types.Storage.Products
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.API.ProductInstance
import Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductInstanceT)
dbTable = DB._productInstance DB.transporterDb

csTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Case.CaseT)
csTable = DB._case DB.transporterDb

prodTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Product.ProductsT)
prodTable = DB._products DB.transporterDb

create :: Storage.ProductInstance -> Flow ()
create Storage.ProductInstance {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.ProductInstance {..})
    >>= either DB.throwDBError pure

findAllByIds :: Integer -> Integer -> [ProductsId] -> Flow [Storage.ProductInstance]
findAllByIds limit offset ids =
  DB.findAllWithLimitOffsetWhere dbTable (pred ids) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ _createdAt
    pred ids Storage.ProductInstance {..} =
      B.in_ _productId (B.val_ <$> ids)

findAllByCaseId :: CaseId -> Flow [Storage.ProductInstance]
findAllByCaseId id =
  DB.findAllOrErr dbTable (pred id)
  where
    pred id Storage.ProductInstance {..} = _caseId ==. B.val_ id

findByCaseId :: CaseId -> Flow Storage.ProductInstance
findByCaseId id =
  DB.findOneWithErr dbTable (pred id)
  where
    pred id Storage.ProductInstance {..} = _caseId ==. B.val_ id

findById' :: ProductInstanceId -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findById' productInstanceId =
  DB.findOne dbTable (predicate productInstanceId)
  where
    predicate productInstanceId Storage.ProductInstance {..} =
      _id ==. B.val_ productInstanceId

findAllByCaseId' :: CaseId -> Flow (T.DBResult [Storage.ProductInstance])
findAllByCaseId' caseId =
  DB.findAll dbTable (predicate caseId)
  where
    predicate caseId Storage.ProductInstance {..} =
      _caseId ==. B.val_ caseId

findAllByIds' :: [ProductInstanceId] -> Flow (T.DBResult [Storage.ProductInstance])
findAllByIds' ids =
  DB.findAll dbTable (predicate ids)
  where
    predicate ids Storage.ProductInstance {..} =
      B.in_ _id (B.val_ <$> ids)

updateStatusForProducts :: ProductsId -> Storage.ProductInstanceStatus -> Flow (T.DBResult ())
updateStatusForProducts productId status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate productId)
  where
    predicate pId Storage.ProductInstance {..} = _productId ==. B.val_ pId
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateStatus ::
  ProductInstanceId ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatus prodInstId status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.ProductInstance {..} =
      _id ==. B.val_ pId
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

findAllByCaseIds :: [CaseId] -> Flow [Storage.ProductInstance]
findAllByCaseIds ids =
  DB.findAll dbTable (pred ids)
    >>= either DB.throwDBError pure
  where
    pred ids Storage.ProductInstance {..} =
      B.in_ _caseId (B.val_ <$> ids)

updateStatusByIds ::
  [ProductInstanceId] ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatusByIds ids status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate ids)
  where
    predicate ids Storage.ProductInstance {..} = B.in_ _id (B.val_ <$> ids)
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateCaseId ::
  ProductInstanceId ->
  CaseId ->
  Flow (T.DBResult ())
updateCaseId prodInstId caseId = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause caseId currTime)
    (predicate prodInstId)
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause caseId currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _caseId <-. B.val_ caseId
        ]

findAllByProdId :: ProductsId -> Flow [Storage.ProductInstance]
findAllByProdId id =
  DB.findAllOrErr dbTable (pred id)
  where
    pred id Storage.ProductInstance {..} = _productId ==. B.val_ id

findAllByStatusParentId :: [Storage.ProductInstanceStatus] -> Maybe ProductInstanceId -> Flow [Storage.ProductInstance]
findAllByStatusParentId status id =
  DB.findAll dbTable (pred id status)
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ _createdAt
    pred id status Storage.ProductInstance {..} =
      _status `B.in_` (B.val_ <$> status)
        &&. B.val_ (isJust id)
        &&. _parentId ==. B.val_ id

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

productInstanceJoin :: Int -> Int -> [Case.CaseType] -> Text -> [Storage.ProductInstanceStatus] -> Flow ProductInstanceList
productInstanceJoin _limit _offset csTypes orgId status = do
  joinedValues <-
    DB.findAllByJoin
      limit
      offset
      orderByDesc
      (joinQuery csTable prodTable dbTable (csPred csTypes) prodPred (piPred orgId status))
      >>= either DB.throwDBError pure
  return $ mkJoinRes <$> joinedValues
  where
    limit = toInteger _limit
    offset = toInteger _offset
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ _createdAt
    csPred csType Case.Case {..} =
      _type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes
    prodPred Product.Products {..} = B.val_ True
    piPred orgId status Storage.ProductInstance {..} =
      _organizationId ==. B.val_ orgId
        &&. _status `B.in_` (B.val_ <$> status) ||. complementVal status
    mkJoinRes (cs, pr, cpr) =
      ProductInstanceRes
        { _case = cs,
          _product = pr,
          _productInstance = cpr,
          _fromLocation = Nothing,
          _toLocation = Nothing
        }
    joinQuery tbl1 tbl2 tbl3 pred1 pred2 pred3 = do
      i <- B.filter_ pred1 $ B.all_ tbl1
      j <- B.filter_ pred2 $ B.all_ tbl2
      k <- B.filter_ pred3 $
        B.join_ tbl3 $
          \line ->
            CasePrimaryKey (Storage._caseId line) B.==. B.primaryKey i
              B.&&. ProductsPrimaryKey (Storage._productId line) B.==. B.primaryKey j
      pure (i, j, k)

productInstanceJoinWithoutLimits :: Case.CaseType -> Text -> [Storage.ProductInstanceStatus] -> Flow ProductInstanceList
productInstanceJoinWithoutLimits csType orgId status = do
  joinedValues <-
    DB.findAllByJoinWithoutLimits
      orderByDesc
      (joinQuery csTable prodTable dbTable (csPred csType) (prodPred orgId) (cprPred status))
      >>= either DB.throwDBError pure
  return $ mkJoinRes <$> joinedValues
  where
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ _createdAt
    csPred csType Case.Case {..} =
      _type ==. B.val_ csType
    prodPred orgId Product.Products {..} = B.val_ True
    cprPred status Storage.ProductInstance {..} =
      _organizationId ==. B.val_ orgId
        &&. _status `B.in_` (B.val_ <$> status) ||. complementVal status
    mkJoinRes (cs, pr, cpr) =
      ProductInstanceRes
        { _case = cs,
          _product = pr,
          _productInstance = cpr,
          _fromLocation = Nothing,
          _toLocation = Nothing
        }
    joinQuery tbl1 tbl2 tbl3 pred1 pred2 pred3 = do
      i <- B.filter_ pred1 $ B.all_ tbl1
      j <- B.filter_ pred2 $ B.all_ tbl2
      k <- B.filter_ pred3 $
        B.join_ tbl3 $
          \line ->
            CasePrimaryKey (Storage._caseId line) B.==. B.primaryKey i
              B.&&. ProductsPrimaryKey (Storage._productId line) B.==. B.primaryKey j
      pure (i, j, k)

findById :: ProductInstanceId -> Flow Storage.ProductInstance
findById pid =
  DB.findOneWithErr dbTable (predicate pid)
  where
    predicate pid Storage.ProductInstance {..} = _id ==. B.val_ pid

updateDvr :: [ProductInstanceId] -> Maybe PersonId -> Flow ()
updateDvr ids driverId = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause driverId currTime)
    (predicate ids)
    >>= either DB.throwDBError pure
  where
    predicate ids Storage.ProductInstance {..} = _id `B.in_` (B.val_ <$> ids)
    setClause driverId currTime Storage.ProductInstance {..} =
      mconcat
        [ _personId <-. B.val_ driverId,
          _updatedAt <-. B.val_ currTime
        ]

updateVeh :: [ProductInstanceId] -> Maybe Text -> Flow ()
updateVeh ids vehId = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause vehId currTime)
    (predicate ids)
    >>= either DB.throwDBError pure
  where
    predicate ids Storage.ProductInstance {..} = _id `B.in_` (B.val_ <$> ids)
    setClause vehId currTime Storage.ProductInstance {..} =
      mconcat
        [ _entityId <-. B.val_ vehId,
          _updatedAt <-. B.val_ currTime
        ]

updateInfo :: ProductInstanceId -> Maybe Text -> Flow ()
updateInfo prodInstId info =
  DB.update
    dbTable
    (setClause info)
    (predicate prodInstId)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause info Storage.ProductInstance {..} =
      mconcat
        [_info <-. B.val_ info]

findAllByVehicleId :: Maybe Text -> Flow [Storage.ProductInstance]
findAllByVehicleId id =
  DB.findAll dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.ProductInstance {..} = B.val_ (isJust id) &&. _entityId ==. B.val_ id

findAllByPersonId :: PersonId -> Flow [Storage.ProductInstance]
findAllByPersonId id =
  DB.findAll dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.ProductInstance {..} = _personId ==. B.val_ (Just id)

findAllByParentId :: Maybe ProductInstanceId -> Flow [Storage.ProductInstance]
findAllByParentId id =
  DB.findAll dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.ProductInstance {..} = B.val_ (isJust id) &&. _parentId ==. B.val_ id
