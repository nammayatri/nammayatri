module Storage.Queries.Products where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Products as Storage
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

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductsT)
dbTable = DB._products DB.appDb

create :: Storage.Products -> L.Flow ()
create Storage.Products {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})
    >>= either DB.throwDBError pure

findAllByTypeOrgId :: Text -> Storage.ProductsStatus -> L.Flow [Storage.Products]
findAllByTypeOrgId orgId status =
  DB.findAll dbTable (predicate orgId status)
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Products {..} = B.desc_ _createdAt
    predicate orgId status Storage.Products {..} =
      ( _status ==. (B.val_ status)
          &&. _organizationId ==. (B.val_ orgId)
      )

findById :: ProductsId -> L.Flow Storage.Products
findById pid =
  DB.findOneWithErr dbTable (predicate pid)
  where
    predicate pid Storage.Products {..} = _id ==. (B.val_ pid)

findAllByIds :: [ProductsId] -> L.Flow [Storage.Products]
findAllByIds pids =
  DB.findAll dbTable (predicate pids)
    >>= either DB.throwDBError pure
  where
    predicate pids Storage.Products {..} =
      _id `B.in_` (B.val_ <$> pids)

updateStatus ::
  ProductsId ->
  Storage.ProductsStatus ->
  L.Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.Products {..} = _id ==. B.val_ id
    setClause status currTime Storage.Products {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateMultiple :: Text -> Storage.Products -> L.Flow ()
updateMultiple id pass@Storage.Products {..} = do
  currTime <- getCurrTime
  DB.update dbTable (setClause currTime pass) (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Products {..} = _id ==. B.val_ (ProductsId id)
    setClause now pass Storage.Products {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _status <-. B.val_ (Storage._status pass),
          --_personId <-. B.val_ (Storage._personId pass),
          _fromLocation <-. B.val_ (Storage._fromLocation pass),
          _toLocation <-. B.val_ (Storage._toLocation pass)
        ]
