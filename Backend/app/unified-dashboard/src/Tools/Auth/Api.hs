{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.Api (module Tools.Auth.Api, module Reexport) where

import Data.Singletons.TH
import Domain.Types.AccessMatrix as Reexport (ServerName (..), UserActionType (..))
import qualified Domain.Types.AccessMatrix as DMatrix
import Domain.Types.AccessMatrix.Singletons ()
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (throwError)
import qualified Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QAccessMatrix
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Auth.Common as Common
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ApiAuth sn uat :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
-- Simplified version for unified-dashboard: uses ServerName and UserActionType directly
type ApiAuth sn uat = HeaderAuthWithPayload "token" VerifyApi (ApiPayload sn uat)

data VerifyApi

data ApiPayload (sn :: DMatrix.ServerName) (uat :: DMatrix.UserActionType)

data ApiTokenInfo = ApiTokenInfo
  { personId :: Id DP.Person,
    merchant :: DM.Merchant,
    city :: City.City,
    userActionType :: DMatrix.UserActionType,
    person :: DP.Person
  }

instance VerificationMethod VerifyApi where
  type VerificationResult VerifyApi = ApiTokenInfo
  verificationDescription =
    "Checks whether token is registered and checks person api access. \
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyApi where
  type VerificationPayloadType VerifyApi = DMatrix.ApiAccessLevel

verifyApiAction ::
  ( Common.AuthFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  VerificationActionWithPayload VerifyApi m
verifyApiAction = VerificationActionWithPayload verifyApi

verifyApi ::
  ( Common.AuthFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  DMatrix.ApiAccessLevel ->
  RegToken ->
  m ApiTokenInfo
verifyApi requiredAccessLevel token = do
  (personId, merchantId, city) <- Common.verifyPerson token
  verifiedPerson <- verifyAccessLevel requiredAccessLevel personId
  verifiedMerchant <- verifyServer requiredAccessLevel.serverName merchantId
  _ <- verifyCity verifiedMerchant city
  pure
    ApiTokenInfo
      { personId = verifiedPerson.id,
        merchant = verifiedMerchant,
        city = city,
        userActionType = requiredAccessLevel.userActionType,
        person = verifiedPerson
      }

instance
  forall (sn :: DMatrix.ServerName) (uat :: DMatrix.UserActionType).
  (SingI sn, SingI uat) =>
  (VerificationPayload DMatrix.ApiAccessLevel) (ApiPayload sn uat)
  where
  toPayloadType _ =
    DMatrix.ApiAccessLevel
      { serverName = Just (fromSing (sing @sn) :: DMatrix.ServerName),
        userActionType = fromSing (sing @uat) :: DMatrix.UserActionType
      }

verifyAccessLevel ::
  ( Storage.Beam.BeamFlow.BeamFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  DMatrix.ApiAccessLevel ->
  Id DP.Person ->
  m DP.Person
verifyAccessLevel requiredApiAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbAccessMatrixItem <- QAccessMatrix.findByRoleIdAndServerAndActionType person.roleId requiredApiAccessLevel.serverName requiredApiAccessLevel.userActionType
  -- If entry exists, access is granted. If not, access is denied.
  when (isNothing mbAccessMatrixItem) $ throwError AccessDenied
  pure person

verifyServer ::
  Storage.Beam.BeamFlow.BeamFlow m r =>
  Maybe DMatrix.ServerName ->
  Id DM.Merchant ->
  m DM.Merchant
verifyServer mbRequiredServerAccess merchantId = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- If serverName is Nothing, skip server verification (API only uses lib-dashboard)
  whenJust mbRequiredServerAccess $ \requiredServerAccess ->
    unless (requiredServerAccess `elem` merchant.serverNames) $ throwError AccessDenied
  return merchant

verifyCity :: MonadFlow m => DM.Merchant -> City.City -> m ()
verifyCity merchant city = unless (city `elem` merchant.supportedOperatingCities) $ throwError AccessDenied

type (/) a b = a b

infixr 0 /
