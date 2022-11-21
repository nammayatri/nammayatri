{-# LANGUAGE TypeApplications #-}

module Tools.Auth.Api (module Tools.Auth.Api, module Reexport) where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Data.Singletons.TH
import Domain.Types.AccessMatrix as Reexport (ApiAccessType (..), ApiEntity (..))
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Domain.Types.ServerName as Reexport (ServerName (..))
import qualified Domain.Types.ServerName as DSN
import Servant hiding (throwError)
import qualified Storage.Queries.AccessMatrix as QAccessMatrix
import qualified Storage.Queries.Merchant as QMerchant
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

instance VerificationMethod VerifyApi where
  type VerificationResult VerifyApi = (ShortId DMerchant.Merchant)
  verificationDescription =
    "Checks whether token is registered and checks person api access. \
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyApi where
  type VerificationPayloadType VerifyApi = DMatrix.ApiAccessLevel

verifyApiAction ::
  (Common.AuthFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r) =>
  VerificationActionWithPayload VerifyApi m
verifyApiAction = VerificationActionWithPayload verifyApi

verifyApi ::
  (Common.AuthFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r) =>
  DMatrix.ApiAccessLevel ->
  RegToken ->
  m (ShortId DMerchant.Merchant)
verifyApi requiredAccessLevel token = do
  (personId, merchantId) <- Common.verifyPerson token
  _personId <- verifyAccessLevel requiredAccessLevel personId
  verifyServer requiredAccessLevel.serverName merchantId

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

verifyAccessLevel :: EsqDBReplicaFlow m r => DMatrix.ApiAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel requiredApiAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbAccessMatrixItem <- QAccessMatrix.findByRoleIdAndEntity person.roleId requiredApiAccessLevel.apiEntity
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
  EsqDBReplicaFlow m r =>
  DSN.ServerName ->
  Id DMerchant.Merchant ->
  m (ShortId DMerchant.Merchant)
verifyServer requiredServerAccess merchantId = do
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (requiredServerAccess == merchant.serverName) $ throwError AccessDenied
  return merchant.shortId
