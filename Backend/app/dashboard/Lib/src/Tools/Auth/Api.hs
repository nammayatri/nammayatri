{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Tools.Auth.Api (module Tools.Auth.Api, module Reexport) where

import Data.Singletons.TH
import Domain.Types.AccessMatrix as Reexport (ApiAccessType (..), ApiEntity (..))
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.ServerName as Reexport (ServerName (..))
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (throwError)
import qualified Storage.Queries.AccessMatrix as QAccessMatrix
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Auth.Common as Common
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ApiAuth sn at ae :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
type ApiAuth sn at ae = HeaderAuthWithPayload "token" VerifyApi (ApiPayload sn at ae)

data VerifyApi

data ApiPayload (sn :: DSN.ServerName) (at :: DMatrix.ApiAccessType) (ae :: DMatrix.ApiEntity)

data ApiTokenInfo = ApiTokenInfo
  { personId :: Id DP.Person,
    merchant :: DM.Merchant
  }

instance VerificationMethod VerifyApi where
  type VerificationResult VerifyApi = ApiTokenInfo
  verificationDescription =
    "Checks whether token is registered and checks person api access. \
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyApi where
  type VerificationPayloadType VerifyApi = DMatrix.ApiAccessLevel

verifyApiAction ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  VerificationActionWithPayload VerifyApi m
verifyApiAction = VerificationActionWithPayload verifyApi

verifyApi ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  DMatrix.ApiAccessLevel ->
  RegToken ->
  m ApiTokenInfo
verifyApi requiredAccessLevel token = do
  (personId, merchantId) <- Common.verifyPerson token
  verifiedPersonId <- verifyAccessLevel requiredAccessLevel personId
  verifiedMerchant <- verifyServer requiredAccessLevel.serverName merchantId
  pure ApiTokenInfo {personId = verifiedPersonId, merchant = verifiedMerchant}

instance
  forall (sn :: DSN.ServerName) (at :: DMatrix.ApiAccessType) (ae :: DMatrix.ApiEntity).
  (SingI sn, SingI at, SingI ae) =>
  (VerificationPayload DMatrix.ApiAccessLevel) (ApiPayload sn at ae)
  where
  toPayloadType _ =
    DMatrix.ApiAccessLevel
      { serverName = fromSing (sing @sn),
        apiAccessType = fromSing (sing @at),
        apiEntity = fromSing (sing @ae)
      }

verifyAccessLevel :: forall m r. EsqDBFlow m r => DMatrix.ApiAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel requiredApiAccessLevel personId = do
  person <- QPerson.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  mbAccessMatrixItem <- QAccessMatrix.findByRoleIdAndEntity person.roleId requiredApiAccessLevel.apiEntity (Proxy @m)
  let userAccessType = maybe DMatrix.USER_NO_ACCESS (.userAccessType) mbAccessMatrixItem
  unless (checkUserAccess userAccessType requiredApiAccessLevel.apiAccessType) $
    throwError AccessDenied
  pure person.id

checkUserAccess :: DMatrix.UserAccessType -> ApiAccessType -> Bool
checkUserAccess DMatrix.USER_FULL_ACCESS _ = True
checkUserAccess DMatrix.USER_READ_ACCESS READ_ACCESS = True
checkUserAccess DMatrix.USER_WRITE_ACCESS WRITE_ACCESS = True
checkUserAccess _ _ = False

verifyServer ::
  forall m r.
  EsqDBFlow m r =>
  DSN.ServerName ->
  Id DM.Merchant ->
  m DM.Merchant
verifyServer requiredServerAccess merchantId = do
  merchant <- QM.findById merchantId (Proxy @m) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (requiredServerAccess == merchant.serverName) $ throwError AccessDenied
  return merchant
