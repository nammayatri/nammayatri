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

updateStatus ::
  ProductsId ->
  Storage.CaseProductStatus ->
  L.Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrTime
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
