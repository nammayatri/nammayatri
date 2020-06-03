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
  L.Flow ()
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Products {..} = _id ==. B.val_ id
    setClause status currTime Storage.Products {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateMultiple :: Text -> Storage.Products -> L.Flow ()
updateMultiple id prd@Storage.Products {..} = do
  currTime <- getCurrentTimeUTC
  DB.update dbTable (setClause currTime prd) (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Products {..} = _id ==. B.val_ (ProductsId id)
    setClause now prd Storage.Products {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _status <-. B.val_ (Storage._status prd),
          --_personId <-. B.val_ (Storage._personId prd),
          _fromLocation <-. B.val_ (Storage._fromLocation prd),
          _toLocation <-. B.val_ (Storage._toLocation prd),
          _info <-. B.val_ (Storage._info prd)
        ]

findAllByIdsAndStatus :: [ProductsId] -> Storage.ProductsStatus -> L.Flow [Storage.Products]
findAllByIdsAndStatus pids status =
  if null pids
    then return []
    else
      DB.findAll dbTable (predicate pids status)
        >>= either DB.throwDBError pure
  where
    predicate pids status Storage.Products {..} =
      _id `B.in_` (B.val_ <$> pids)
        &&. _status ==. (B.val_ status)
