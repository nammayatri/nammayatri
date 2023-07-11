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
import Domain.Types.AccessMatrix as Reexport (ApiAccessLevel (userActionType), UserActionType (..))
import qualified Domain.Types.AccessMatrix as DMatrix
import Domain.Types.AccessMatrix.BAP.CustomerActionType as Reexport
import Domain.Types.AccessMatrix.BAP.MerchantActionType as Reexport
import Domain.Types.AccessMatrix.BAP.RideActionType as Reexport
import Domain.Types.AccessMatrix.BPP as Reexport
import Domain.Types.AccessMatrix.BPP.DriverReferralActionType as Reexport
import Domain.Types.AccessMatrix.BPP.VolunteerActionType as Reexport
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
  SanitizedUrl (ApiAuth uat :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
type ApiAuth uat = HeaderAuthWithPayload "token" VerifyApi (ApiPayload uat)

data VerifyApi

data ApiPayload (uat :: DMatrix.UserActionType)

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
  verifiedMerchant <- verifyServer requiredAccessLevel merchantId
  pure ApiTokenInfo {personId = verifiedPersonId, merchant = verifiedMerchant}

instance
  forall (uat :: DMatrix.UserActionType).
  SingI uat =>
  (VerificationPayload DMatrix.ApiAccessLevel) (ApiPayload uat)
  where
  toPayloadType _ =
    DMatrix.ApiAccessLevel
      { userActionType = fromSing (sing @uat)
      }

verifyAccessLevel :: EsqDBFlow m r => DMatrix.ApiAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel requiredApiAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbAccessMatrixItem <- QAccessMatrix.findByRoleIdAndEntityAndActionType person.roleId requiredApiAccessLevel.userActionType
  let userAccessType = maybe DMatrix.USER_NO_ACCESS (.userAccessType) mbAccessMatrixItem
  unless (checkUserAccess userAccessType) $
    throwError AccessDenied
  pure person.id

checkUserAccess :: DMatrix.UserAccessType -> Bool
checkUserAccess DMatrix.USER_FULL_ACCESS = True
checkUserAccess DMatrix.USER_NO_ACCESS = False

verifyServer ::
  EsqDBFlow m r =>
  DMatrix.ApiAccessLevel ->
  Id DM.Merchant ->
  m DM.Merchant
verifyServer requiredServerAccess merchantId = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case requiredServerAccess.userActionType of
    AppBackendBAP _ ->
      unless (merchant.serverName == DSN.APP_BACKEND) $ throwError AccessDenied
    DriverOfferBPP _ ->
      unless (merchant.serverName == DSN.DRIVER_OFFER_BPP || merchant.serverName == DSN.BECKN_TRANSPORT) $ throwError AccessDenied
    SpecialZones _ ->
      unless (merchant.serverName == DSN.SPECIAL_ZONE) $ throwError AccessDenied
  return merchant
