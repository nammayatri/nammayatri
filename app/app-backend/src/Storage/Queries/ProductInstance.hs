module Storage.Queries.ProductInstance where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as Storage
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Models.Case as Case
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductInstanceT)
dbTable = DB._productInstance DB.appDb

create :: Storage.ProductInstance -> Flow (T.DBResult ())
create Storage.ProductInstance {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.ProductInstance {..})

findById :: ProductInstanceId -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findById pid =
  DB.findOne dbTable (predicate pid)
  where
    predicate pid Storage.ProductInstance {..} = _id ==. B.val_ pid

findAllByCaseId :: CaseId -> Flow (T.DBResult [Storage.ProductInstance])
findAllByCaseId caseId =
  DB.findAll dbTable (predicate caseId)
  where
    predicate caseId Storage.ProductInstance {..} =
      _caseId ==. B.val_ caseId

findByProductId :: ProductsId -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findByProductId pId =
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _productId ==. B.val_ pId

findAllByPerson :: PersonId -> Flow (T.DBResult [Storage.ProductInstance])
findAllByPerson perId =
  DB.findAll dbTable predicate
  where
    predicate Storage.ProductInstance {..} = _personId ==. B.val_ (Just perId)

updateCaseId ::
  ProductInstanceId ->
  CaseId ->
  Flow (T.DBResult ())
updateCaseId id caseId = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause caseId currTime)
    (predicate id)
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause caseId currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _caseId <-. B.val_ caseId
        ]

updateStatus ::
  ProductInstanceId ->
  Storage.ProductInstanceStatus ->
  Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateAllProductInstancesByCaseId :: CaseId -> Storage.ProductInstanceStatus -> Flow (T.DBResult ())
updateAllProductInstancesByCaseId caseId status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate caseId)
  where
    predicate caseId Storage.ProductInstance {..} = _caseId ==. B.val_ caseId
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

listAllProductInstanceWithOffset :: Integer -> Integer -> ListById -> [Storage.ProductInstanceStatus] -> [Case.CaseType] -> Flow (T.DBResult [Storage.ProductInstance])
listAllProductInstanceWithOffset limit offset id stats csTypes =
  DB.findAllWithLimitOffsetWhere dbTable (predicate id stats csTypes) limit offset orderBy
  where
    predicate (ByApplicationId i) s csTypes Storage.ProductInstance {..} =
      _caseId ==. B.val_ i
        &&. (_status `B.in_` (B.val_ <$> s) ||. complementVal s)
        &&. (_type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes)
    predicate (ByCustomerId i) s csTypes Storage.ProductInstance {..} =
      _personId ==. B.val_ (Just i)
        &&. (_status `B.in_` (B.val_ <$> s) ||. complementVal s)
        &&. (_type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes)
    predicate (ById i) s csTypes Storage.ProductInstance {..} =
      _productId ==. B.val_ i
        &&. (_status `B.in_` (B.val_ <$> s) ||. complementVal s)
        &&. (_type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes)
    orderBy Storage.ProductInstance {..} = B.desc_ _updatedAt

listAllProductInstance :: ListById -> [Storage.ProductInstanceStatus] -> Flow (T.DBResult [Storage.ProductInstance])
listAllProductInstance id status =
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

updateMultiple :: ProductInstanceId -> Storage.ProductInstance -> Flow (T.DBResult ())
updateMultiple id prdInst@Storage.ProductInstance {..} = do
  currTime <- getCurrentTimeUTC
  DB.update dbTable (setClause currTime prdInst) (predicate id)
  where
    predicate id Storage.ProductInstance {..} = _id ==. B.val_ id
    setClause now prdInst Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _status <-. B.val_ (Storage._status prdInst),
          --_personId <-. B.val_ (Storage._personId prd),
          _fromLocation <-. B.val_ (Storage._fromLocation prdInst),
          _toLocation <-. B.val_ (Storage._toLocation prdInst),
          _info <-. B.val_ (Storage._info prdInst)
        ]

findByParentIdType :: Maybe ProductInstanceId -> Case.CaseType -> Flow (T.DBResult (Maybe Storage.ProductInstance))
findByParentIdType mparentId csType =
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      B.val_ (isJust mparentId) &&. _parentId ==. B.val_ mparentId
        &&. _type ==. B.val_ csType

findAllByParentId :: Maybe ProductInstanceId -> Flow (T.DBResult [Storage.ProductInstance])
findAllByParentId id =
  DB.findAll dbTable (predicate id)
  where
    predicate id Storage.ProductInstance {..} = B.val_ (isJust id) &&. _parentId ==. B.val_ id

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False
