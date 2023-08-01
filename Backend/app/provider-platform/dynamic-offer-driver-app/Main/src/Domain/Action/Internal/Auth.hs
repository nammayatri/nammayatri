{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.Auth where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Common as Utils
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth (validateToken)

newtype InternalResp = InternalResp
  { driverId :: Id DP.Person
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

internalAuth :: (HasField "locationTrackingServiceKey" AppEnv Text) => Maybe Text -> Maybe Text -> Maybe Text -> Flow InternalResp
internalAuth token apiKey merchantId = do
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ do
    throwError $ InvalidToken "Invalid api key"
  registraionDetails <-
    QR.findByToken (fromMaybe "" token)
      >>= Utils.fromMaybeM (InvalidRequest "Invalid token")
      >>= validateToken
  unless (registraionDetails.merchantId == fromMaybe "" merchantId) $ do
    throwError $ InvalidRequest "Invalid merchant id"
  pure $
    InternalResp
      { driverId = Id registraionDetails.entityId
      }
