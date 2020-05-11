module Product.Products where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Types.API.Products
import qualified Types.Storage.Driver
import qualified Types.Storage.Vehicle
import qualified Storage.Queries.Vehicle as VQ
import qualified Storage.Queries.Driver as VD
import qualified Storage.Queries.Products as DB
import qualified Beckn.Types.Storage.Products as Storage
import           Types.API.CaseProduct
import System.Environment
import Types.App
import Utils.Routes


updateProductInfo :: ProdReq -> FlowHandler ProdInfoRes
updateProductInfo ProdReq {..} = withFlowHandler $ do
  drivInfo <- VD.findDriverById (DriverId _driverId)
  vechInfo <- VQ.findVehicleById (VehicleId _vehicleId)
  let info = Just $ show $ toJSON (prepareInfo drivInfo vechInfo)
  DB.updateInfo (ProductsId _id) info
  return $ fromMaybe "Failure" info
  where
  prepareInfo drivInfo vechInfo = Storage.ProdInfo
        { driverInfo = show (toJSON <$> drivInfo)
        , vehicleInfo = show (toJSON <$>  vechInfo)
        }
