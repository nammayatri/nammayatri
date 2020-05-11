module Storage.Queries.Products where

import qualified Beckn.Storage.Queries as DB
import qualified Storage.Queries.Vehicle as VQ
import qualified Storage.Queries.Driver as VD
import Beckn.Types.Common
import Beckn.Types.App
import qualified Beckn.Types.Storage.Products as Storage
import qualified Types.Storage.Driver as D
import qualified Types.Storage.Vehicle as V
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductsT)
dbTable = DB._products DB.transporterDb

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
          &&. _organizationId ==. (B.val_ orgId))

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

updateInfo :: ProductsId -> DriverId -> VehicleId -> L.Flow (T.DBResult ())
updateInfo prodId driverId vechId = do
  drivInfo <- VD.findDriverById driverId
  vechInfo <- VQ.findVehicleById vechId
  let info = Just $ show $ toJSON (prepareInfo drivInfo vechInfo)
  DB.update
    dbTable
    (setClause info)
    (predicate prodId)
  where
    prepareInfo drivInfo vechInfo = Storage.ProdInfo
      { driverInfo = show (toJSON <$> drivInfo)
      , vehicleInfo = show (toJSON <$>  vechInfo)
      }
    predicate id Storage.Products {..} = _id ==. B.val_ id
    setClause info Storage.Products {..} =
      mconcat
        [ _info <-. B.val_ info]

