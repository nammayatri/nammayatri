{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Provider.Registration
  ( postUserLogin,
  )
where

import qualified API.Types.Management.Registration
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Domain.Action.Management.Registration
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DMerchantAccess
import qualified Domain.Types.Person as DPerson
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (EncFlow, getDbHash)
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QPerson
import Tools.Auth.Api
import Tools.Error

postUserLogin ::
  ( Kernel.Types.Id.ShortId DMerchant.Merchant ->
    City.City ->
    API.Types.Management.Registration.LoginReq ->
    Environment.Flow API.Types.Management.Registration.LoginRes
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
        merchants <- B.runInReplica $ QMerchant.findAllByShortIds merchantShortIds
        -- Validate that all requested merchant IDs were found
        let foundShortIds = map (.shortId) merchants
        forM_ merchantShortIds $ \requestedShortId ->
          unless (requestedShortId `elem` foundShortIds) $
            throwError $ MerchantDoesNotExist requestedShortId.getShortId
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
  token <- Domain.Action.Management.Registration.generateToken person.id merchant' city'

  -- Return response
  pure $ API.Types.Management.Registration.LoginRes token city' merchant'.shortId
