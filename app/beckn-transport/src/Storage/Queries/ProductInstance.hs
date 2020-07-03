module Storage.Queries.ProductInstance where

import Beckn.Types.App
import Beckn.Types.Common
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

create :: Storage.ProductInstance -> L.Flow ()
create Storage.ProductInstance {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.ProductInstance {..})
    >>= either DB.throwDBError pure

findAllByIds :: Integer -> Integer -> [ProductsId] -> L.Flow [Storage.ProductInstance]
findAllByIds limit offset ids =
  DB.findAllWithLimitOffsetWhere dbTable (pred ids) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ _createdAt
    pred ids Storage.ProductInstance {..} =
      B.in_ _productId (B.val_ <$> ids)

findAllByCaseId :: CaseId -> L.Flow [Storage.ProductInstance]
findAllByCaseId id =
  DB.findAllOrErr dbTable (pred id)
  where
    pred id Storage.ProductInstance {..} = _caseId ==. B.val_ id

findByCaseId :: CaseId -> L.Flow Storage.ProductInstance
findByCaseId id =
  DB.findOneWithErr dbTable (pred id)
  where
    pred id Storage.ProductInstance {..} = _caseId ==. B.val_ id

updateStatusForProducts :: ProductsId -> Storage.ProductInstanceStatus -> L.Flow (T.DBResult ())
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
  CaseId ->
  ProductsId ->
  Storage.ProductInstanceStatus ->
  L.Flow (T.DBResult ())
updateStatus caseId productId status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate caseId productId)
  where
    predicate cId pId Storage.ProductInstance {..} =
      (_caseId ==. B.val_ cId) &&. (_productId ==. B.val_ pId)
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

findAllByCaseIds :: [CaseId] -> L.Flow [Storage.ProductInstance]
findAllByCaseIds ids =
  DB.findAll dbTable (pred ids)
    >>= either DB.throwDBError pure
  where
    pred ids Storage.ProductInstance {..} =
      B.in_ _caseId (B.val_ <$> ids)

updateStatusByIds ::
  [ProductInstanceId] ->
  Storage.ProductInstanceStatus ->
  L.Flow (T.DBResult ())
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

findAllByProdId :: ProductsId -> L.Flow [Storage.ProductInstance]
findAllByProdId id =
  DB.findAllOrErr dbTable (pred id)
  where
    pred id Storage.ProductInstance {..} = _productId ==. B.val_ id

findAllByStatusIds :: [Storage.ProductInstanceStatus] -> [ProductsId] -> L.Flow [Storage.ProductInstance]
findAllByStatusIds status ids =
  DB.findAll dbTable (pred ids status)
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ _createdAt
    pred ids status Storage.ProductInstance {..} =
      _status `B.in_` (B.val_ <$> status) ||. complementVal status
        &&. B.in_ _productId (B.val_ <$> ids)

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

productInstanceJoin :: Int -> Int -> Case.CaseType -> Text -> [Storage.ProductInstanceStatus] -> L.Flow ProductInstanceList
productInstanceJoin _limit _offset csType orgId status = do
  joinedValues <-
    DB.findAllByJoin
      limit
      offset
      orderByDesc
      (joinQuery csTable prodTable dbTable (pred1 csType) (pred2 orgId) (pred3 status))
      >>= either DB.throwDBError pure
  return $ mkJoinRes <$> joinedValues
  where
    limit = toInteger _limit
    offset = toInteger _offset
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ _createdAt
    pred1 csType Case.Case {..} =
      _type ==. B.val_ csType
    pred2 orgId Product.Products {..} =
      _organizationId ==. B.val_ orgId
    pred3 status Storage.ProductInstance {..} =
      _status `B.in_` (B.val_ <$> status) ||. complementVal status
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

productInstanceJoinWithoutLimits :: Case.CaseType -> Text -> [Storage.ProductInstanceStatus] -> L.Flow ProductInstanceList
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
    prodPred orgId Product.Products {..} =
      _organizationId ==. B.val_ orgId
    cprPred status Storage.ProductInstance {..} =
      _status `B.in_` (B.val_ <$> status) ||. complementVal status
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
