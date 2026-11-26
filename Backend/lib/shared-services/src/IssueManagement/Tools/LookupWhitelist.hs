{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Receiving-side whitelist for the in-house chat lookup engine, shared by
-- rider-app and driver-app.
--
-- rider-app's engine calls a /direct-dashboard tree (its own for
-- InternalRider, driver-app's for InternalDriver) with its service token
-- ('issueLookupToken'). Each app runs 'issueLookupGuard' before its dashboard
-- session check. By then the router has already resolved the request path to
-- an endpoint id, so no path matching is needed: for a request carrying the
-- lookup token, that endpoint id must be listed in the receiving app's
-- issue_config.allowed_lookup_endpoints for the merchant/city in the path,
-- else AccessDenied. Requests carrying any other token are untouched.
module IssueManagement.Tools.LookupWhitelist (issueLookupGuard) where

import qualified IssueManagement.Common as Common
import qualified IssueManagement.Domain.Types.Issue.IssueConfig as DIC
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQIssueConfig
import qualified IssueManagement.Tools.LookupGuard as LookupGuard
import Kernel.Prelude
import Kernel.Types.Beckn.City (City)
import Kernel.Types.Error (AuthError (AccessDenied))
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (parseUrlPiece)

issueLookupGuard ::
  (BeamFlow m r, HasFlowEnv m r '["issueLookupToken" ::: Maybe Text]) =>
  -- | the app's merchant/city resolver (findMOCityByMerchantShortIdAndCity)
  (ShortId Common.Merchant -> City -> m (Maybe Common.MerchantOperatingCity)) ->
  -- | which app's issue_config to read (CUSTOMER for rider-app, DRIVER for driver-app)
  Common.Identifier ->
  -- | endpoint id the router resolved, e.g. "RIDER_MANAGEMENT/RIDE/GET_RIDE_INFO"
  Text ->
  -- | request path segments
  [Text] ->
  -- | the request's token header
  Text ->
  m ()
issueLookupGuard findMOCity identifier endpointId pathSegments token = do
  mbLookupToken <- asks (.issueLookupToken)
  when (Just token == mbLookupToken) $ do
    (merchantShortId, city) <- maybe (deny "lookup path is not under /direct-dashboard/{merchant}/{city}/") pure (merchantAndCity pathSegments)
    (moCity :: Common.MerchantOperatingCity) <-
      findMOCity (ShortId merchantShortId) city
        >>= maybe (deny $ "unknown merchant/city " <> merchantShortId <> "/" <> show city) pure
    -- Read straight from issue_config (not through ConfigPilot), so NammaTag
    -- dynamic logic cannot widen the whitelist.
    (mbIssueConfig :: Maybe DIC.IssueConfig) <- CQIssueConfig.findByMerchantOpCityId moCity.id identifier
    case LookupGuard.checkLookupAllowed (mbIssueConfig >>= (.allowedLookupEndpoints)) endpointId of
      Left reason -> deny reason
      Right () -> pure ()
  where
    deny :: (MonadFlow m) => Text -> m a
    deny reason = do
      logTagError "ISSUE_LOOKUP_DENIED" $ "endpointId: " <> endpointId <> ", reason: " <> reason
      throwError AccessDenied

    merchantAndCity = \case
      ("direct-dashboard" : merchantShortId : cityText : _ : _) -> (merchantShortId,) <$> either (const Nothing) Just (parseUrlPiece @City cityText)
      (_ : rest) -> merchantAndCity rest
      [] -> Nothing
