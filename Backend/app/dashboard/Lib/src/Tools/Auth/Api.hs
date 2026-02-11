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
import Domain.Types.AccessMatrix as Reexport (ApiEntity (..), UserActionType (..))
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Domain.Types.ServerName as Reexport (ServerName (..))
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (throwError)
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QAccessMatrix
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
-- import qualified Storage.CachedQueries.Role as CQRole
import qualified Tools.Auth.Common as Common
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ApiAuth sn ae uat :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
type ApiAuth sn ae uat = HeaderAuthWithPayload "token" VerifyApi (ApiPayload sn ae uat)

data VerifyApi

data ApiPayload (sn :: DSN.ServerName) (ae :: DMatrix.ApiEntity) (uat :: DMatrix.UserActionType)

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
  forall (sn :: DSN.ServerName) (ae :: DMatrix.ApiEntity) (uat :: DMatrix.UserActionType).
  (SingI sn, SingI ae, SingI uat) =>
  (VerificationPayload DMatrix.ApiAccessLevel) (ApiPayload sn ae uat)
  where
  toPayloadType _ =
    DMatrix.ApiAccessLevel
      { serverName = fromSing (sing @sn),
        apiEntity = fromSing (sing @ae),
        userActionType = fromSing (sing @uat)
      }

verifyAccessLevel ::
  ( BeamFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  DMatrix.ApiAccessLevel ->
  Id DP.Person ->
  m DP.Person
verifyAccessLevel requiredApiAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  maybe (pure ()) (\a -> when (a `elem` [DRole.DASHBOARD_ADMIN, DRole.DASHBOARD_USER]) $ Common.checkPasswordExpiry person) person.dashboardAccessType
  mbAccessMatrixItem <- QAccessMatrix.findByRoleIdAndEntityAndActionType person.roleId requiredApiAccessLevel.apiEntity $ DMatrix.UserActionTypeWrapper requiredApiAccessLevel.userActionType
  let userAccessType = maybe DMatrix.USER_NO_ACCESS (.userAccessType) mbAccessMatrixItem
  unless (checkUserAccess userAccessType) $
    throwError AccessDenied
  pure person

checkUserAccess :: DMatrix.UserAccessType -> Bool
checkUserAccess DMatrix.USER_FULL_ACCESS = True
checkUserAccess DMatrix.USER_NO_ACCESS = False


verifyServer ::
  BeamFlow m r =>
  DSN.ServerName ->
  Id DM.Merchant ->
  m DM.Merchant
verifyServer requiredServerAccess merchantId = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (requiredServerAccess `elem` merchant.serverNames) $ throwError AccessDenied
  return merchant

verifyCity :: MonadFlow m => DM.Merchant -> City.City -> m ()
verifyCity merchant city = unless (city `elem` merchant.supportedOperatingCities) $ throwError AccessDenied

type (/) a b = a b

infixr 0 /
