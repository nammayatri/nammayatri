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
import qualified Beckn.Types.Storage.Person as SP
import qualified Types.Storage.Driver as D
import qualified Beckn.Types.Storage.Vehicle as V
import qualified Storage.Queries.Vehicle as VQ
import qualified Storage.Queries.Driver as VD
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Products as DB
import qualified Beckn.Types.Storage.Products as Storage
import qualified Storage.Queries.RegistrationToken as QR
import           Types.API.CaseProduct
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Types.App
import Beckn.Utils.Common (withFlowHandler)

updateInfo :: Maybe Text -> ProdReq -> FlowHandler ProdInfoRes
updateInfo regToken ProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  let info = Just $ TE.decodeUtf8 $ BSL.toStrict $ encode (prepareInfo _driverInfo _vehicleInfo)
  DB.updateInfo (ProductsId _id) info
  return $ fromMaybe "Failure" info
  where
    prepareInfo drivInfo vehiInfo = Storage.ProdInfo
          { driverInfo = TE.decodeUtf8 $ BSL.toStrict $ encode drivInfo
          , vehicleInfo = TE.decodeUtf8 $ BSL.toStrict $ encode vehiInfo
          , assignedTo = _getDriverId $ D._id drivInfo
          }

listRides :: Maybe Text -> FlowHandler RideList
listRides regToken = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  person <- QP.findPersonById (PersonId _EntityId)
  case SP._organizationId person of
    Just orgId -> do
      rideList <- DB.findAllByOrgId orgId
      let rides = filter (filterRides person) rideList
      return $ rides
    Nothing ->
      L.throwException $ err400 {errBody = "organisation id is missing"}
  where
    filterRides person ride =
        case Storage._info ride of
          Just k ->
            let rideInfoText = BSL.fromStrict $ TE.encodeUtf8 k
                rideInfo = (decode rideInfoText) :: Maybe Storage.ProdInfo
            in case rideInfo of
              Just c -> _getPersonId (SP._id person) == Storage.assignedTo c
              Nothing -> False
          Nothing -> False
