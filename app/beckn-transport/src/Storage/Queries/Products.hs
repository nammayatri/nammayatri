module Storage.Queries.Products where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Products as Storage
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductsT)
dbTable = DB._products DB.transporterDb

create :: Storage.Products -> L.Flow ()
create Storage.Products {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})
    >>= either DB.throwDBError pure

findAllByTypeOrgId :: Text -> [Storage.ProductsStatus] -> L.Flow [Storage.Products]
findAllByTypeOrgId orgId status =
  DB.findAll dbTable (predicate orgId status)
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Products {..} = B.desc_ _createdAt
    predicate orgId status Storage.Products {..} =
      ( _organizationId ==. (B.val_ orgId)
          &&. B.in_ _status (B.val_ <$> status)
      )

findAllById :: [ProductsId] -> L.Flow [Storage.Products]
findAllById ids =
  DB.findAllOrErr dbTable (predicate ids)
  where
    predicate ids Storage.Products {..} =
      B.in_ _id (B.val_ <$> ids)

findById :: ProductsId -> L.Flow Storage.Products
findById pid =
  DB.findOneWithErr dbTable (predicate pid)
  where
    predicate pid Storage.Products {..} = _id ==. (B.val_ pid)

updateStatus ::
  ProductsId ->
  Storage.ProductsStatus ->
  L.Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
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

updateInfo :: ProductsId -> Maybe Text -> L.Flow ()
updateInfo prodId info = do
  DB.update
    dbTable
    (setClause info)
    (predicate prodId)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Products {..} = _id ==. B.val_ id
    setClause info Storage.Products {..} =
      mconcat
        [_info <-. B.val_ info]

updateVeh :: ProductsId -> Maybe Text -> L.Flow ()
updateVeh prodId vehId = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause vehId currTime)
    (predicate prodId)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Products {..} = _id ==. B.val_ id
    setClause vehId currTime Storage.Products {..} =
      mconcat
        [ _udf3 <-. B.val_ vehId,
          _updatedAt <-. B.val_ currTime
        ]

updateDvr :: ProductsId -> Maybe Text -> L.Flow ()
updateDvr prodId driverId = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause driverId currTime)
    (predicate prodId)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Products {..} = _id ==. B.val_ id
    setClause driverId currTime Storage.Products {..} =
      mconcat
        [ _assignedTo <-. B.val_ driverId,
          _updatedAt <-. B.val_ currTime
        ]

findAllByAssignedTo :: Text -> L.Flow [Storage.Products]
findAllByAssignedTo id =
  DB.findAll dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Products {..} = (_assignedTo ==. (B.val_ (Just id)))

findAllByOrgId :: Text -> L.Flow [Storage.Products]
findAllByOrgId orgId =
  DB.findAll dbTable (predicate orgId)
    >>= either DB.throwDBError pure
  where
    predicate orgId Storage.Products {..} =
      ( _organizationId ==. (B.val_ orgId)
      )
