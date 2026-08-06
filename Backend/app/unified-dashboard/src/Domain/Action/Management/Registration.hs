{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Management.Registration
  ( postUserLogin,
    postUserLoginVerifyOtp,
    generateToken,
    generateNewToken,
    buildRegistrationToken,
  )
where

import qualified API.Types.Management.Registration
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DMerchantAccess
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RegistrationToken as DR
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (EncFlow, getDbHash)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Tools.Auth.Common as Auth
import Tools.Auth.Merchant
import Tools.Error
import qualified Tools.Utils

postUserLogin :: (Kernel.Types.Id.ShortId DMerchant.Merchant -> City.City -> API.Types.Management.Registration.LoginReq -> Environment.Flow API.Types.Management.Registration.LoginRes)
postUserLogin merchantShortId opCity req = do
  person <-
    case (req.email, req.mobileNumber, req.mobileCountryCode) of
      (Just email, Nothing, _) -> do
        emailDbHash <- getDbHash (T.toLower email)
        passwordDbHash <- getDbHash req.password
        QPerson.findByEmailAndPassword (Just emailDbHash) (Just passwordDbHash) >>= fromMaybeM (PersonDoesNotExist email)
      (Nothing, Just mobileNumber, Just mobileCountryCode) -> do
        mobileNumberHash <- getDbHash mobileNumber
        passwordDbHash <- getDbHash req.password
        QPerson.findByMobileNumberAndPassword (Just mobileNumberHash) (Just mobileCountryCode) (Just passwordDbHash) >>= fromMaybeM (PersonDoesNotExist mobileNumber)
      _ -> throwError $ InvalidRequest "Either email or mobileNumber with mobileCountryCode must be provided"

  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id opCity >>= fromMaybeM AccessDenied
  unless (merchant.enabled == Just True) $ throwError (InvalidRequest "Account deactivated. Please check with Admin")
  token <- generateToken person.id merchant opCity
  pure $ API.Types.Management.Registration.LoginRes token opCity merchant.shortId

postUserLoginVerifyOtp ::
  ( Kernel.Types.Id.ShortId DMerchant.Merchant ->
    City.City ->
    API.Types.Management.Registration.VerifyOtpReq ->
    Environment.Flow API.Types.Management.Registration.LoginRes
  )
postUserLoginVerifyOtp merchantShortId opCity req = do
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- QPerson.findByMobileNumberHash (Just mobileNumberHash) (Just req.mobileCountryCode) >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id opCity >>= fromMaybeM AccessDenied
  let otpKey = Tools.Utils.getMobileNumberOtpKey req.mobileCountryCode req.mobileNumber
  storedOtp <- Redis.get otpKey >>= fromMaybeM (InvalidRequest "OTP expired or not found")
  unless (storedOtp == req.otp) $ throwError $ InvalidRequest "Invalid OTP"
  Redis.del otpKey
  token <- generateToken person.id merchant opCity
  pure $ API.Types.Management.Registration.LoginRes token opCity merchant.shortId

generateToken ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Kernel.Types.Id.Id DPerson.Person ->
  DMerchant.Merchant ->
  City.City ->
  m Text
generateToken personId merchant city = do
  case merchant.singleActiveSessionOnly of
    Just True -> generateNewToken personId merchant.id city
    _ -> do
      findPreviousToken <- QR.findAllByPersonIdAndMerchantIdAndCity personId merchant.id city
      case findPreviousToken of
        [] -> generateNewToken personId merchant.id city
        (token : _) -> pure $ token.token

generateNewToken ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    MonadFlow m
  ) =>
  Kernel.Types.Id.Id DPerson.Person ->
  Kernel.Types.Id.Id DMerchant.Merchant ->
  City.City ->
  m Text
generateNewToken personId merchantId city = do
  regToken <- buildRegistrationToken personId merchantId city
  Auth.cleanCachedTokensByMerchantIdAndCity personId merchantId city
  -- Delete all existing tokens for this person/merchant/city
  existingTokens <- QR.findAllByPersonIdAndMerchantIdAndCity personId merchantId city
  for_ existingTokens $ \token -> QR.deleteById token.id
  QR.create regToken
  pure $ regToken.token

buildRegistrationToken ::
  MonadFlow m =>
  Kernel.Types.Id.Id DPerson.Person ->
  Kernel.Types.Id.Id DMerchant.Merchant ->
  City.City ->
  m DR.RegistrationToken
buildRegistrationToken personId merchantId city = do
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  pure $
    DR.RegistrationToken
      { id = Kernel.Types.Id.Id rtid,
        personId = personId,
        merchantId = merchantId,
        operatingCity = city,
        token = token,
        createdAt = now,
        updatedAt = now,
        enabled = True
      }
