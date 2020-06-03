module Storage.Queries.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as Storage
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
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

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseProductT)
dbTable = DB._caseProduct DB.appDb

create :: Storage.CaseProduct -> L.Flow ()
create Storage.CaseProduct {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.CaseProduct {..})
    >>= either DB.throwDBError pure

findAllByCaseId :: CaseId -> L.Flow [Storage.CaseProduct]
findAllByCaseId caseId =
  DB.findAll dbTable (predicate caseId)
    >>= either DB.throwDBError pure
  where
    predicate caseId Storage.CaseProduct {..} =
      _caseId ==. B.val_ caseId

findByProductId :: ProductsId -> L.Flow Storage.CaseProduct
findByProductId pId =
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.CaseProduct {..} =
      _productId ==. B.val_ pId

findByCaseAndProductId :: CaseId -> ProductsId -> L.Flow Storage.CaseProduct
findByCaseAndProductId caseId pId =
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.CaseProduct {..} =
      _productId ==. B.val_ pId &&. _caseId ==. B.val_ caseId

findAllByPerson :: PersonId -> L.Flow [Storage.CaseProduct]
findAllByPerson perId =
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.CaseProduct {..} = _personId ==. B.val_ (Just perId)

updateStatus ::
  ProductsId ->
  Storage.CaseProductStatus ->
  L.Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.CaseProduct {..} = _productId ==. B.val_ id
    setClause status currTime Storage.CaseProduct {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateAllProductsByCaseId :: CaseId -> Products.ProductsStatus -> L.Flow (T.DBResult ())
updateAllProductsByCaseId caseId status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  caseProducts <- findAllByCaseId caseId
  let productIds = Storage._productId <$> caseProducts
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

listAllCaseProductWithOffset :: Integer -> Integer -> ListById -> [Storage.CaseProductStatus] -> L.Flow [Storage.CaseProduct]
listAllCaseProductWithOffset limit offset id stats = do
  DB.findAllWithLimitOffsetWhere dbTable (predicate id stats) limit offset orderBy
    >>= either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.CaseProduct {..} = (_caseId ==. B.val_ i)
    predicate (ByApplicationId i) s Storage.CaseProduct {..} = (_caseId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    predicate (ByCustomerId i) [] Storage.CaseProduct {..} = (_personId ==. B.val_ (Just i))
    predicate (ByCustomerId i) s Storage.CaseProduct {..} = (_personId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s))
    predicate (ById i) [] Storage.CaseProduct {..} = (_productId ==. B.val_ i)
    predicate (ById i) s Storage.CaseProduct {..} = (_productId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    orderBy Storage.CaseProduct {..} = B.desc_ _updatedAt

listAllCaseProduct :: ListById -> [Storage.CaseProductStatus] -> L.Flow [Storage.CaseProduct]
listAllCaseProduct id status = do
  DB.findAll dbTable (predicate id status)
    >>= either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.CaseProduct {..} = (_caseId ==. B.val_ i)
    predicate (ByApplicationId i) s Storage.CaseProduct {..} = (_caseId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    predicate (ByCustomerId i) [] Storage.CaseProduct {..} = (_personId ==. B.val_ (Just i))
    predicate (ByCustomerId i) s Storage.CaseProduct {..} = (_personId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s))
    predicate (ById i) [] Storage.CaseProduct {..} = (_productId ==. B.val_ i)
    predicate (ById i) s Storage.CaseProduct {..} = (_productId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))

findAllProductsByCaseId :: CaseId -> L.Flow [Products.Products]
findAllProductsByCaseId caseId =
  findAllByCaseId caseId
    >>= Products.findAllByIds . map Storage._productId
