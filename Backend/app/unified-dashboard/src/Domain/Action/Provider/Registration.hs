{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Provider.Registration
  ( postUserLogin,
  )
where

import qualified API.Types.Provider.Registration
import qualified Data.List as List
import Data.Maybe (mapMaybe)
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
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth.Api
import qualified Tools.Auth.Common as Auth
import Tools.Error

postUserLogin ::
  ( Kernel.Types.Id.ShortId DMerchant.Merchant ->
    City.City ->
    API.Types.Provider.Registration.LoginReq ->
    Environment.Flow API.Types.Provider.Registration.LoginRes
  )
postUserLogin _merchantShortId _opCity req = do
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

  merchantAccessList <- B.runInReplica $ QAccess.findAllByPersonId person.id

  (merchant', city') <-
    case merchantAccessList of
      [] -> throwError (InvalidRequest "No access to any merchant")
      merchantAccessList' -> do
        let sortedMerchantAccessList = List.sortOn DMerchantAccess.merchantShortId merchantAccessList'
        let groupedByMerchant = List.groupBy ((==) `on` DMerchantAccess.merchantShortId) sortedMerchantAccessList
        let merchantShortIds =
              mapMaybe
                ( \merchantGroup -> case merchantGroup of
                    (ma : _) -> Just $ DMerchantAccess.merchantShortId ma
                    [] -> Nothing
                )
                groupedByMerchant
        merchants <- forM merchantShortIds $ \shortId -> QMerchant.findByShortId shortId >>= fromMaybeM (MerchantDoesNotExist shortId.getShortId)
        let enabledMerchants = filter (\merchant -> merchant.enabled == Just True) merchants
        case enabledMerchants of
          [] -> throwError (InvalidRequest "Account deactivated. Please check with Admin")
          (merchant : _) -> do
            let merchantWithCityList = map (.operatingCity) $ filter (\ma -> ma.merchantId == merchant.id) merchantAccessList'
            let defaultCityPresent = elem merchant.defaultOperatingCity merchantWithCityList
            let city' = case merchantWithCityList of
                  [] -> merchant.defaultOperatingCity
                  (city : _) -> if defaultCityPresent then merchant.defaultOperatingCity else city
            pure (merchant, city')

  -- Generate token
  token <- generateToken person.id merchant' city'

  -- Return response
  pure $ API.Types.Provider.Registration.LoginRes token city' merchant'.shortId

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
        enabled = True,
        merchantOperatingCityId = Nothing
      }
