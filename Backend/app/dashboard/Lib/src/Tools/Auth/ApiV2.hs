{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.ApiV2 (module Tools.Auth.ApiV2, module Reexport) where

import Data.Singletons.TH
import Domain.Types.AccessMatrix as Reexport (ApiEntity (..), UserActionType (..))
import qualified Domain.Types.AccessMatrix as DMatrix
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
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QAccessMatrix
import qualified Storage.Queries.Person as QPerson
import Tools.Auth.Api as Reexport (ApiTokenInfo (..), checkUserAccess, verifyCity, verifyServer)
import qualified Tools.Auth.Common as Common
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ApiAuthV2 sn uat :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
type ApiAuthV2 sn uat = HeaderAuthWithPayload "token" VerifyApiV2 (ApiPayloadV2 sn uat)

data VerifyApiV2

data ApiPayloadV2 (sn :: DSN.ServerName) (uat :: DMatrix.UserActionType)

instance VerificationMethod VerifyApiV2 where
  type VerificationResult VerifyApiV2 = ApiTokenInfo
  verificationDescription =
    "Checks whether token is registered and checks person api access. \
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyApiV2 where
  type VerificationPayloadType VerifyApiV2 = DMatrix.ApiV2AccessLevel

verifyApiV2Action ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  VerificationActionWithPayload VerifyApiV2 m
verifyApiV2Action = VerificationActionWithPayload verifyApiV2

verifyApiV2 ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  DMatrix.ApiV2AccessLevel ->
  RegToken ->
  m ApiTokenInfo
verifyApiV2 requiredAccessLevel token = do
  (personId, merchantId, city) <- Common.verifyPerson token
  verifiedPersonId <- verifyAccessLevelV2 requiredAccessLevel personId
  verifiedMerchant <- verifyServer requiredAccessLevel.serverName merchantId
  _ <- verifyCity verifiedMerchant city
  pure
    ApiTokenInfo
      { personId = verifiedPersonId,
        merchant = verifiedMerchant,
        city = city,
        userActionType = requiredAccessLevel.userActionType
      }

instance
  forall (sn :: DSN.ServerName) (uat :: DMatrix.UserActionType).
  (SingI sn, SingI uat) =>
  (VerificationPayload DMatrix.ApiV2AccessLevel) (ApiPayloadV2 sn uat)
  where
  toPayloadType _ =
    DMatrix.ApiV2AccessLevel
      { serverName = fromSing (sing @sn),
        userActionType = fromSing (sing @uat)
      }

verifyAccessLevelV2 :: BeamFlow m r => DMatrix.ApiV2AccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevelV2 requiredApiAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbAccessMatrixItem <- QAccessMatrix.findByRoleIdAndEntityAndActionType person.roleId Nothing $ DMatrix.UserActionTypeWrapper requiredApiAccessLevel.userActionType
  let userAccessType = maybe DMatrix.USER_NO_ACCESS (.userAccessType) mbAccessMatrixItem
  unless (checkUserAccess userAccessType) $
    throwError AccessDenied
  pure person.id

type (/) a b = a b

infixr 0 /
