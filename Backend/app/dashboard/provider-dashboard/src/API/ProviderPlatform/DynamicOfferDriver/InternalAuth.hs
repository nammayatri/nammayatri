{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module API.ProviderPlatform.DynamicOfferDriver.InternalAuth
  ( API,
    handler,
  )
where

import Data.Aeson as DA
import Domain.Action.ProviderPlatform.Fleet.Driver
import qualified "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Tools.Auth.Common as Auth
import "lib-dashboard" Tools.Error

type API =
  "internal"
    :> "auth"
    :> Header "api-key" Text
    :> Header "token" RegToken
    :> Get '[JSON] InternalAuthResp

handler :: FlowServer API
handler = internalAuthHandler

data InternalAuthResp = InternalAuthResp
  { personId :: Id DP.Person,
    personIds :: [Id DP.Person]
  }
  deriving (Generic, ToSchema)

instance ToJSON InternalAuthResp where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = \case "personId" -> "driverId"; "personIds" -> "driverIds"; other -> other}

instance FromJSON InternalAuthResp where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = \case "personId" -> "driverId"; "personIds" -> "driverIds"; other -> other}

internalAuthHandler ::
  Maybe Text ->
  Maybe RegToken ->
  FlowHandler InternalAuthResp
internalAuthHandler apiKey token = withFlowHandlerAPI' $ do
  internalAuthAPIKey <- asks (.internalAuthAPIKey)
  unless (apiKey == Just internalAuthAPIKey) $ do
    throwError $ InvalidRequest "Invalid API key"
  (personId, _, _) <- Auth.verifyPerson (fromMaybe "" token)
  fleetOwnerIds <- getFleetOwnerIds personId.getId Nothing
  case fleetOwnerIds of
    [] -> throwError $ InternalError "No Fleet Member Association Found"
    fleetOwner@(fleetOwnerId, _) : fleetOwners ->
      pure $
        InternalAuthResp
          { personId = Id fleetOwnerId,
            personIds = (Id . fst) <$> ([fleetOwner] <> fleetOwners)
          }
