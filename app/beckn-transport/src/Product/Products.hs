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
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Types.Storage.Driver as D
import qualified Beckn.Types.Storage.Vehicle as V
import qualified Storage.Queries.Vehicle as VQ
import qualified Storage.Queries.Driver as VD
import qualified Storage.Queries.Products as DB
import qualified Beckn.Types.Storage.Products as Storage
import qualified Storage.Queries.RegistrationToken as QR
import           Types.API.CaseProduct
import System.Environment
import Types.App
import Beckn.Utils.Common (withFlowHandler)

updateInfo :: Maybe Text -> ProdReq -> FlowHandler ProdInfoRes
updateInfo regToken ProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  let info = Just $ show $ toJSON (prepareInfo _driverInfo _vehicleInfo)
  DB.updateInfo (ProductsId _id) info
  return $ fromMaybe "Failure" info
  where
    prepareInfo drivInfo vechInfo = Storage.ProdInfo
          { driverInfo = show (toJSON  drivInfo)
          , vehicleInfo = show (toJSON  vechInfo)
          }
