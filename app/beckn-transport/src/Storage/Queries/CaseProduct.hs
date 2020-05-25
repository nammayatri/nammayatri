module Storage.Queries.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.CaseProduct as Storage
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CaseProductT)
dbTable = DB._caseProduct DB.transporterDb

create :: Storage.CaseProduct -> L.Flow ()
create Storage.CaseProduct {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.CaseProduct {..})
    >>= either DB.throwDBError pure

findAllByIds :: Integer -> Integer -> [ProductsId] -> L.Flow [Storage.CaseProduct]
findAllByIds limit offset ids =
  DB.findAllWithLimitOffsetWhere dbTable (pred ids) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.CaseProduct {..} = B.desc_ _createdAt
    pred ids Storage.CaseProduct {..} =
      B.in_ _productId (B.val_ <$> ids)

findAllByCaseId :: CaseId -> L.Flow [Storage.CaseProduct]
findAllByCaseId id =
  DB.findAllOrErr dbTable (pred id)
  where
    pred id Storage.CaseProduct {..} = _caseId ==. (B.val_ id)

findByCaseId :: CaseId -> L.Flow Storage.CaseProduct
findByCaseId id =
  DB.findOneWithErr dbTable (pred id)
  where
    pred id Storage.CaseProduct {..} = _caseId ==. (B.val_ id)

updateStatusForProducts :: ProductsId -> Storage.CaseProductStatus -> L.Flow (T.DBResult ())
updateStatusForProducts productId status = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate productId)
  where
    predicate pId Storage.CaseProduct {..} = (_productId ==. B.val_ pId)
    setClause status currTime Storage.CaseProduct {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateStatus ::
  CaseId ->
  ProductsId ->
  Storage.CaseProductStatus ->
  L.Flow (T.DBResult ())
updateStatus caseId productId status = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate caseId productId)
  where
    predicate cId pId Storage.CaseProduct {..} =
      (_caseId ==. B.val_ cId) &&. (_productId ==. B.val_ pId)
    setClause status currTime Storage.CaseProduct {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

findAllByProdId :: ProductsId -> L.Flow [Storage.CaseProduct]
findAllByProdId id =
  DB.findAllOrErr dbTable (pred id)
  where
    pred id Storage.CaseProduct {..} = _productId ==. (B.val_ id)

findAllByStatusIds :: Integer -> Integer -> [Storage.CaseProductStatus] -> [ProductsId] -> L.Flow [Storage.CaseProduct]
findAllByStatusIds limit offset status ids =
  DB.findAllWithLimitOffsetWhere dbTable (pred ids status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.CaseProduct {..} = B.desc_ _createdAt
    pred ids status Storage.CaseProduct {..} =
      ( _status `B.in_` ((B.val_) <$> status) ||. complementVal status
          &&. B.in_ _productId (B.val_ <$> ids)
      )

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False
