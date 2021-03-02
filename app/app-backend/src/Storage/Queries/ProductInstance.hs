module Storage.Queries.ProductInstance where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as Storage
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Models.Case as Case
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductInstanceT))
getDbTable =
  DB._productInstance . DB.appDb <$> getSchemaName

create :: Storage.ProductInstance -> Flow (T.DBResult ())
create Storage.ProductInstance {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.ProductInstance {..})

findById :: ID Storage.ProductInstance -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate pid)
  where
    predicate piid Storage.ProductInstance {..} = _id ==. B.val_ piid

findAllByCaseId :: ID Case.Case -> Flow (T.DBResult [Storage.ProductInstance])
findAllByCaseId caseId = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _caseId ==. B.val_ caseId

findByProductId :: ProductsId -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findByProductId pId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _productId ==. B.val_ pId

findAllByPerson :: ID Person.Person -> Flow (T.DBResult [Storage.ProductInstance])
findAllByPerson perId = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _personId ==. B.val_ (Just perId)

updateCaseId ::
  ID Storage.ProductInstance ->
  ID Case.Case ->
  Flow (T.DBResult ())
updateCaseId id caseId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause caseId currTime)
    (predicate id)
  where
    predicate piid Storage.ProductInstance {..} = _id ==. B.val_ piid
    setClause cid currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _caseId <-. B.val_ cid
        ]

updateStatus ::
  ID Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatus id status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate piid Storage.ProductInstance {..} = _id ==. B.val_ piid
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]

updateAllProductInstancesByCaseId :: ID Case.Case -> Storage.ProductInstanceStatus -> Flow (T.DBResult ())
updateAllProductInstancesByCaseId caseId status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate caseId)
  where
    predicate cid Storage.ProductInstance {..} = _caseId ==. B.val_ cid
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]

listAllProductInstanceWithOffset :: Integer -> Integer -> ListById -> [Storage.ProductInstanceStatus] -> [Case.CaseType] -> Flow (T.DBResult [Storage.ProductInstance])
listAllProductInstanceWithOffset limit offset id stats csTypes = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable (predicate id stats) limit offset orderBy
  where
    predicate (ByApplicationId i) s Storage.ProductInstance {..} =
      _caseId ==. B.val_ i
        &&. (_status `B.in_` (B.val_ <$> s) ||. complementVal s)
        &&. (_type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes)
    predicate (ByCustomerId i) s Storage.ProductInstance {..} =
      _personId ==. B.val_ (Just i)
        &&. (_status `B.in_` (B.val_ <$> s) ||. complementVal s)
        &&. (_type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes)
    predicate (ById i) s Storage.ProductInstance {..} =
      _productId ==. B.val_ i
        &&. (_status `B.in_` (B.val_ <$> s) ||. complementVal s)
        &&. (_type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes)
    orderBy Storage.ProductInstance {..} = B.desc_ _updatedAt

listAllProductInstance :: ListById -> [Storage.ProductInstanceStatus] -> Flow (T.DBResult [Storage.ProductInstance])
listAllProductInstance id status = do
  dbTable <- getDbTable
  DB.findAll dbTable (predicate id status)
  where
    predicate (ByApplicationId i) [] Storage.ProductInstance {..} = _caseId ==. B.val_ i
    predicate (ByApplicationId i) s Storage.ProductInstance {..} = _caseId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)
    predicate (ByCustomerId i) [] Storage.ProductInstance {..} = _personId ==. B.val_ (Just i)
    predicate (ByCustomerId i) s Storage.ProductInstance {..} = _personId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s)
    predicate (ById i) [] Storage.ProductInstance {..} = _productId ==. B.val_ i
    predicate (ById i) s Storage.ProductInstance {..} = _productId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)

listAllProductInstanceByPerson :: Person.Person -> ListById -> [Storage.ProductInstanceStatus] -> Flow (T.DBResult [Storage.ProductInstance])
listAllProductInstanceByPerson person id status =
  case id of
    ByApplicationId caseId ->
      Case.findIdByPerson person caseId >> listAllProductInstance id status
    _ -> listAllProductInstance id status

updateMultiple :: ID Storage.ProductInstance -> Storage.ProductInstance -> Flow (T.DBResult ())
updateMultiple id prdInst@Storage.ProductInstance {..} = do
  dbTable <- getDbTable
  currTime <- getCurrTime
  DB.update dbTable (setClause currTime prdInst) (predicate id)
  where
    predicate piid Storage.ProductInstance {..} = _id ==. B.val_ piid
    setClause now prodInst Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _status <-. B.val_ (Storage._status prodInst),
          --_personId <-. B.val_ (Storage._personId prd),
          _fromLocation <-. B.val_ (Storage._fromLocation prodInst),
          _toLocation <-. B.val_ (Storage._toLocation prodInst),
          _info <-. B.val_ (Storage._info prodInst),
          _udf4 <-. B.val_ (Storage._udf4 prodInst)
        ]

findByParentIdType :: Maybe (ID Storage.ProductInstance) -> Case.CaseType -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findByParentIdType mparentId csType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      B.val_ (isJust mparentId) &&. _parentId ==. B.val_ mparentId
        &&. _type ==. B.val_ csType

findAllByParentId :: Maybe (ID Storage.ProductInstance) -> Flow (T.DBResult [Storage.ProductInstance])
findAllByParentId id = do
  dbTable <- getDbTable
  DB.findAll dbTable (predicate id)
  where
    predicate piid Storage.ProductInstance {..} = B.val_ (isJust id) &&. _parentId ==. B.val_ piid

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findAllExpiredByStatus :: [Storage.ProductInstanceStatus] -> UTCTime -> Flow (T.DBResult [Storage.ProductInstance])
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ _status (B.val_ <$> statuses)
        &&. _startTime B.<=. B.val_ expiryTime
