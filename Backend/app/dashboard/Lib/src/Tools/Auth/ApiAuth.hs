{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Dashboard API authentication, agnostic to which package owns the action
-- type.
--
-- Each server owns the enum describing its own endpoints; this module never
-- names any of them. The action stays a real type all the way through -- it is
-- only rendered to text at the single point where it has to be, because
-- @capability_endpoint@ is keyed by a text column in the database.
module Tools.Auth.ApiAuth where

import Data.Singletons
import Data.Singletons.TH (genSingletons)
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth (VerificationMethod (..))
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Servant hiding (throwError)
import qualified Tools.Auth.Common as Common
import qualified Tools.Auth.Verify as Verify
import Tools.Servant.HeaderAuth

-- | How an owning package renders its action type to the endpoint id stored in
-- @capability_endpoint.endpoint_id@. This is the one place the typed value
-- becomes text, and it mirrors what the enum's own 'Show' already produced.
class IsUserActionType uat where
  showUserActionType :: uat -> Text

-- | Which API family an action belongs to. Predates the DSL; every generated
-- endpoint uses 'DSL' and the rest are legacy hand-written trees.
data ApiEntity
  = DSL
  | CUSTOMERS
  | DRIVERS
  | RIDES
  | MONITORING
  | MERCHANT
  | MESSAGE
  | REFERRAL
  | ISSUE
  | VOLUNTEER
  | SPECIAL_ZONES
  | SUBSCRIPTION
  | FLEET
  | OVERLAY
  | NAMMA_TAG
  | MIGRATION
  | FRFS
  | BHARAT_TAXI_USER
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema, Eq, Ord)

genSingletons [''ApiEntity]

-- | What a route demands of its caller, with the action still typed.
data ApiAccessLevel uat = ApiAccessLevel
  { serverName :: DSN.ServerName,
    apiEntity :: ApiEntity,
    userActionType :: uat
  }

-- | What the auth path establishes, handed to the handler. The action is the
-- caller's own type, not a string.
data ApiTokenInfo uat = ApiTokenInfo
  { personId :: Id DP.Person,
    merchant :: DM.Merchant,
    city :: City.City,
    userActionType :: uat,
    person :: DP.Person
  }

data VerifyApi (uat :: Type)

instance VerificationMethod (VerifyApi uat) where
  type VerificationResult (VerifyApi uat) = ApiTokenInfo uat
  verificationDescription =
    "Checks whether token is registered and checks person api access. \
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload (VerifyApi uat) where
  type VerificationPayloadType (VerifyApi uat) = ApiAccessLevel uat

-- | The route's identity, fixed at the type level. @uat@ is poly-kinded: it is
-- a promoted constructor of whichever action enum the owning package defines.
-- @uat'@ is that enum's ordinary (demoted) type, carried explicitly because
-- GHC will not accept the type family @Demote k@ in an instance head.
data ApiPayload (uat' :: Type) (sn :: DSN.ServerName) (ae :: ApiEntity) (uat :: k)

-- | Token verification with api access level checking, for a package that owns
-- the action enum @uat'@. Each owning package defines its own three-argument
-- @ApiAuth@ synonym pinning @uat'@, so route definitions stay unchanged:
--
-- > type ApiAuth sn ae uat = ApiAuthFor UserActionType sn ae uat
type ApiAuthFor (uat' :: Type) (sn :: DSN.ServerName) (ae :: ApiEntity) (uat :: k) =
  HeaderAuthWithPayload "token" (VerifyApi uat') (ApiPayload uat' sn ae uat)

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ApiAuthFor uat' sn ae uat :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  forall uat' (sn :: DSN.ServerName) (ae :: ApiEntity) k (uat :: k).
  (SingI sn, SingI ae, SingI uat, SingKind k, Demote k ~ uat') =>
  VerificationPayload (ApiAccessLevel uat') (ApiPayload uat' sn ae uat)
  where
  toPayloadType _ =
    ApiAccessLevel
      { serverName = fromSing (sing @sn),
        apiEntity = fromSing (sing @ae),
        userActionType = fromSing (sing @uat)
      }

-- | The endpoint id this action is registered under in @capability_endpoint@.
-- DSL actions serialize to MODULE/RESOURCE/ACTION; legacy (non-DSL) actions
-- carry a LEGACY/<entity>/<action> qualified id.
mkEndpointId :: IsUserActionType uat => ApiAccessLevel uat -> Text
mkEndpointId lvl =
  case lvl.apiEntity of
    DSL -> showUserActionType lvl.userActionType
    entity -> "LEGACY/" <> T.pack (show entity) <> "/" <> showUserActionType lvl.userActionType

verifyApiAction ::
  forall uat m r.
  ( Common.AuthFlow m r,
    Redis.HedisFlow m r,
    IsUserActionType uat,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int],
    HasFlowEnv m r '["dashboardApiRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  VerificationActionWithPayload (VerifyApi uat) m
verifyApiAction = VerificationActionWithPayload verifyApi

verifyApi ::
  ( Common.AuthFlow m r,
    Redis.HedisFlow m r,
    IsUserActionType uat,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int],
    HasFlowEnv m r '["dashboardApiRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  ApiAccessLevel uat ->
  [Text] ->
  RegToken ->
  m (ApiTokenInfo uat)
verifyApi requiredAccessLevel pathSegments token = do
  verified <-
    Verify.verifyDashboardUser
      requiredAccessLevel.serverName
      (mkEndpointId requiredAccessLevel)
      pathSegments
      token
  -- Applied once the caller is known, so the limit is per operator rather than
  -- per source address.
  rateLimitOptions <- asks (.dashboardApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (dashboardApiHitsCountKey verified.personId) rateLimitOptions
  pure
    ApiTokenInfo
      { personId = verified.personId,
        merchant = verified.merchant,
        city = verified.city,
        userActionType = requiredAccessLevel.userActionType,
        person = verified.person
      }

dashboardApiHitsCountKey :: Id DP.Person -> Text
dashboardApiHitsCountKey personId = "dashboard:apiRateLimit:" <> personId.getId

type (/) a b = a b

infixr 0 /
