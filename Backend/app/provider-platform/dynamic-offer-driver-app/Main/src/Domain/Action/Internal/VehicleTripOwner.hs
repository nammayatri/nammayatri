{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.VehicleTripOwner
  ( VehicleTripOwnerReq (..),
    VehicleTripOwner (..),
    resolveVehicleTripOwner,
  )
where

import BecknV2.FRFS.Enums (VehicleCategory (..))
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (throwError)
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import Lib.GtfsDataServer.Types
import SharedLogic.IntegratedBPPConfig (findIbppConfigByCityVehicleAndFeedKey, getGimsBaseUrl)
import Tools.Error (GenericError (InvalidRequest))

data VehicleTripOwnerReq = VehicleTripOwnerReq
  { merchantOperatingCityId :: Text,
    gtfsId :: Text,
    vehicleNumber :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VehicleTripOwner = VehicleTripOwner
  { onlineWaybill :: Bool,
    crewBadges :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

resolveVehicleTripOwner :: Maybe Text -> VehicleTripOwnerReq -> Flow VehicleTripOwner
resolveVehicleTripOwner apiKey req = do
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ throwError $ InvalidRequest "Invalid API key"
  integratedBPPConfig <- findIbppConfigByCityVehicleAndFeedKey (Id req.merchantOperatingCityId) (show BUS) req.gtfsId
  let busNo = T.strip req.vehicleNumber
  toOwner <$> busOnlineWaybill integratedBPPConfig busNo
  where
    toOwner = \case
      Just details ->
        VehicleTripOwner
          { onlineWaybill = True,
            crewBadges = filter (not . T.null) . map T.strip $ catMaybes [details.gimsDriverId, details.gimsConductorId]
          }
      Nothing -> VehicleTripOwner {onlineWaybill = False, crewBadges = []}

busOnlineWaybill :: DIBC.IntegratedBPPConfig -> Text -> Flow (Maybe GimsCurrentTripDetailsResp)
busOnlineWaybill integratedBPPConfig busNo = do
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  mbDetails <-
    NandiFlow.gimsCurrentTripDetailsIfOnline baseUrl (DIBC.feedKey integratedBPPConfig) $
      GimsCurrentTripDetailsReq {previousTripNumber = 0, gimsConductorId = Nothing, gimsDriverId = Nothing, vehicleNumber = Just busNo}
  pure $ do
    details <- mbDetails
    guard (T.strip details.vehicleNumber == busNo)
    pure details
