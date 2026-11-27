{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Api-key authenticated probe that resolves a dashboard session to its topic
-- and the fleet owners it covers.
--
-- Authenticated by a shared api-key rather than an operator capability, so it
-- does not belong in the login tree and keeps its own mount.
--
-- The session lookup is dashboard data; the fleet-owner lookup is driver-app
-- data, which provider-dashboard had to fetch over HTTP and this server simply
-- reads. Only the dashboard half is wrapped in 'runInDashboardDb'.
module API.DashboardInternalAuth
  ( API,
    handler,
  )
where

import Data.Aeson as DA
import Data.List (nub)
import qualified "lib-dashboard" Domain.Types.FleetScopeMode as DFSM
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified "lib-dashboard" Domain.Types.PersonRoleAssignment as DPRA
import Environment
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Fleet as SFleet
import qualified "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QDashMerchant
import qualified "lib-dashboard" Storage.Queries.Person as QDashPerson
import qualified "lib-dashboard" Storage.Queries.PersonRoleAssignment as QDashAssignment
import qualified "lib-dashboard" Tools.Auth.Common as Auth
import qualified "lib-dashboard" Tools.Auth.FleetScope as LibFleetScope
import qualified "lib-dashboard" Tools.DashboardTopic as DTopic

type API =
  "internal"
    :> "auth"
    :> Header "api-key" Text
    :> Header "token" RegToken
    :> Get '[JSON] InternalAuthResp

handler :: FlowServer API
handler = internalAuthHandler

data InternalAuthResp = InternalAuthResp
  { topic :: Text,
    personIds :: [Id DP.Person]
  }
  deriving (Generic, ToSchema)

-- The wire names differ from the field names and are part of the contract.
instance ToJSON InternalAuthResp where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = \case "topic" -> "driverId"; "personIds" -> "driverIds"; other -> other}

instance FromJSON InternalAuthResp where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = \case "topic" -> "driverId"; "personIds" -> "driverIds"; other -> other}

internalAuthHandler :: Maybe Text -> Maybe RegToken -> FlowHandler InternalAuthResp
internalAuthHandler apiKey token = withFlowHandlerAPI' $ do
  internalAuthAPIKey <- asks (.internalAuthAPIKey)
  unless (apiKey == Just internalAuthAPIKey) $
    throwError $ InvalidRequest "Invalid API key"
  (personId, merchantId, _) <- runInDashboardDb $ Auth.verifyPerson (fromMaybe "" token)
  runInDashboardDb $ LibFleetScope.assertOtpSessionStillAllowedByIds (fromMaybe "" token) personId merchantId
  -- Looked up before entering the dashboard scope: part of it reads this application's own tables. As
  -- 'resolveTopicForPerson' does with its lookup, a failure leaves no fleets.
  resolvedFleetOwnerIds <- try @_ @SomeException (fleetOwnerLookup merchantId (fromMaybe "" token) personId.getId)
  (topic, fleetOwnerIds) <- runInDashboardDb $ DTopic.resolveTopicForPerson (const $ pure (either (const []) identity resolvedFleetOwnerIds)) personId
  pure $ InternalAuthResp {topic = topic.getTopic, personIds = Id <$> fleetOwnerIds}

-- | The fleets of a person. Fleet owners are driver-app rows, so only the dashboard reads here run in the
-- dashboard scope.
--
-- Once fleet scope is enforced a fleet owner's fleets are the ones their session has selected, out of those they
-- hold by role assignment (all of them while nothing is selected); the fleet member association is not read.
-- Otherwise the member association decides, as before.
fleetOwnerLookup :: Id DMerchant.Merchant -> RegToken -> Text -> Flow [Text]
fleetOwnerLookup merchantId token personId = do
  mbEnforced <- runInDashboardDb $ do
    mbMerchant <- QDashMerchant.findById merchantId
    mbPerson <- QDashPerson.findById (Id personId)
    case (mbMerchant, mbPerson) of
      (Just merchant, Just person) | merchant.fleetScopeMode == DFSM.ENFORCE && DP.isFleetOwner person -> do
        held <- QDashAssignment.findAllActiveByPersonIdAndMerchantId (Id personId) merchant.id
        selection <- (.fleetSelection) <$> LibFleetScope.sessionState token
        let fleetIds = nub [a.scopeId | a <- held, a.scopeType == DPRA.FLEET]
        pure . Just $ if null selection then fleetIds else filter (`elem` selection) fleetIds
      _ -> pure Nothing
  maybe (map fst <$> SFleet.getFleetOwnerIds personId Nothing) pure mbEnforced
