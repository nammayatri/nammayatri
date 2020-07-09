module Storage.Queries.ProductInstance where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as Storage
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Products as Products
import qualified Storage.Queries.Products as QP
import Types.App
import qualified Types.Storage.DB as DB

-- TODO: Add this later if required

-- | ByOrganizationId OrganizationId
data ListById
  = ByApplicationId CaseId
  | ById ProductsId
  | ByCustomerId PersonId

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductInstanceT)
dbTable = DB._productInstance DB.appDb

create :: Storage.ProductInstance -> L.Flow ()
create Storage.ProductInstance {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.ProductInstance {..})
    >>= either DB.throwDBError pure

findAllByCaseId :: CaseId -> L.Flow [Storage.ProductInstance]
findAllByCaseId caseId =
  DB.findAll dbTable (predicate caseId)
    >>= either DB.throwDBError pure
  where
    predicate caseId Storage.ProductInstance {..} =
      _caseId ==. B.val_ caseId

findByProductId :: ProductsId -> L.Flow Storage.ProductInstance
findByProductId pId =
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _productId ==. B.val_ pId

findByCaseAndProductId :: CaseId -> ProductsId -> L.Flow Storage.ProductInstance
findByCaseAndProductId caseId pId =
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      _productId ==. B.val_ pId &&. _caseId ==. B.val_ caseId

findAllByPerson :: PersonId -> L.Flow [Storage.ProductInstance]
findAllByPerson perId =
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.ProductInstance {..} = _personId ==. B.val_ (Just perId)

updateStatus ::
  ProductsId ->
  Storage.ProductInstanceStatus ->
  L.Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.ProductInstance {..} = _productId ==. B.val_ id
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateAllProductInstancesByCaseId :: CaseId -> Storage.ProductInstanceStatus -> L.Flow (T.DBResult ())
updateAllProductInstancesByCaseId caseId status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  productInstances <- findAllByCaseId caseId
  DB.update
    dbTable
    (setClause status currTime)
    (predicate (Storage._id <$> productInstances))
  where
    predicate ids Storage.ProductInstance {..} = _id `B.in_` (B.val_ <$> ids)
    setClause status currTime Storage.ProductInstance {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateAllProductsByCaseId :: CaseId -> Products.ProductsStatus -> L.Flow (T.DBResult ())
updateAllProductsByCaseId caseId status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  productInstances <- findAllByCaseId caseId
  let productIds = Storage._productId <$> productInstances
  DB.update
    table
    (setClause status currTime)
    (predicate productIds)
  where
    setClause status currTime Products.Products {..} =
      mconcat
        [ _status <-. B.val_ status,
          _updatedAt <-. B.val_ currTime
        ]
    predicate ids Products.Products {..} = _id `B.in_` (B.val_ <$> ids)
    table :: B.DatabaseEntity be DB.AppDb (B.TableEntity Products.ProductsT)
    table = DB._products DB.appDb

listAllProductInstanceWithOffset :: Integer -> Integer -> ListById -> [Storage.ProductInstanceStatus] -> L.Flow [Storage.ProductInstance]
listAllProductInstanceWithOffset limit offset id stats =
  DB.findAllWithLimitOffsetWhere dbTable (predicate id stats) limit offset orderBy
    >>= either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.ProductInstance {..} = _caseId ==. B.val_ i
    predicate (ByApplicationId i) s Storage.ProductInstance {..} = _caseId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)
    predicate (ByCustomerId i) [] Storage.ProductInstance {..} = _personId ==. B.val_ (Just i)
    predicate (ByCustomerId i) s Storage.ProductInstance {..} = _personId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s)
    predicate (ById i) [] Storage.ProductInstance {..} = _productId ==. B.val_ i
    predicate (ById i) s Storage.ProductInstance {..} = _productId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)
    orderBy Storage.ProductInstance {..} = B.desc_ _updatedAt

listAllProductInstance :: ListById -> [Storage.ProductInstanceStatus] -> L.Flow [Storage.ProductInstance]
listAllProductInstance id status =
  DB.findAll dbTable (predicate id status)
    >>= either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.ProductInstance {..} = _caseId ==. B.val_ i
    predicate (ByApplicationId i) s Storage.ProductInstance {..} = _caseId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)
    predicate (ByCustomerId i) [] Storage.ProductInstance {..} = _personId ==. B.val_ (Just i)
    predicate (ByCustomerId i) s Storage.ProductInstance {..} = _personId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s)
    predicate (ById i) [] Storage.ProductInstance {..} = _productId ==. B.val_ i
    predicate (ById i) s Storage.ProductInstance {..} = _productId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)

listAllProductInstanceByPerson :: Person.Person -> ListById -> [Storage.ProductInstanceStatus] -> L.Flow [Storage.ProductInstance]
listAllProductInstanceByPerson person id status =
  case id of
    ByApplicationId caseId ->
      Case.findIdByPerson person caseId >> listAllProductInstance id status
    _ -> listAllProductInstance id status

findAllProductsByCaseId :: CaseId -> L.Flow [Products.Products]
findAllProductsByCaseId caseId =
  findAllByCaseId caseId
    >>= Products.findAllByIds . map Storage._productId
