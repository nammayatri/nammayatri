{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Referral
  ( applyReferralCode,
    isCustomerReferralCode,
  )
where

import qualified API.Types.UI.CustomerReferral as APITypes
import Data.Either.Extra (mapRight)
import Data.Fixed (div')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.RefereeLink as DReferral
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.MerchantOperatingCity as QMerchantO
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS
import Tools.Error

data ValidatedRefCode
  = -- | (refCode, referrer, referrerStats)
    Rider Text Person.Person DPS.PersonStats
  | -- | (refCode, mobileNumber, countryCode)
    Driver Text Text Text

applyReferralCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Person.Person -> Bool -> Text -> Maybe LatLong -> m (Either APISuccess.APISuccess APITypes.ReferrerInfo)
applyReferralCode person shouldShareReferrerInfo refCode mbCustomerLocation = withPersonIdLogTag person.id $ do
  validatedCode <- validateRefCode person refCode
  res <- getReferrerInfo person shouldShareReferrerInfo mbCustomerLocation validatedCode
  when (isNothing person.referralCode) $ do
    void $ QPerson.updateRefCode person.id refCode
    case validatedCode of
      Rider _ referrer referrerStats -> do
        void $ QPS.updateReferralCount (referrerStats.referralCount + 1) referrer.id
        void $ QPerson.updateReferredByCustomer person.id referrer.id.getId
      _ -> return ()
  return res

validateRefCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Person.Person -> Text -> m ValidatedRefCode
validateRefCode person refCode =
  withLogTag ("Referral Code Flow:> refCode:-" <> show refCode) $ do
    if isCustomerReferralCode refCode
      then withLogTag "Rider" $ do
        unless (TU.validateAlphaNumericWithLength refCode 6) $
          throwError $ InvalidRequest "Referral Code must have 6 digits and must be Alphanumeric"
        when (isJust person.referralCode) $
          throwError $ InvalidRequest "You have been already referred by someone"
        referredByPerson <- QPerson.findPersonByCustomerReferralCode (Just refCode) >>= fromMaybeM (InvalidRequest "Invalid ReferralCode")
        when (person.id == referredByPerson.id) $
          throwError $ InvalidRequest "Cannot refer yourself"
        referrerStats <- QPS.findByPersonId referredByPerson.id >>= fromMaybeM (PersonStatsNotFound person.id.getId)
        return $ Rider refCode referredByPerson referrerStats
      else withLogTag "Driver" $ do
        unless (TU.validateAllDigitWithMinLength 4 refCode) $
          throwError $ InvalidRequest "Referral Code must have 4 digits"
        mobileNumber <- fromMaybeM (PersonMobileNumberIsNULL person.id.getId) person.mobileNumber >>= decrypt
        countryCode <- person.mobileCountryCode & fromMaybeM (PersonMobileNumberIsNULL person.id.getId)
        return $ Driver refCode mobileNumber countryCode

isCustomerReferralCode :: Text -> Bool
isCustomerReferralCode = T.isPrefixOf "C"

getReferrerInfo :: (CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Person.Person -> Bool -> Maybe LatLong -> ValidatedRefCode -> m (Either APISuccess.APISuccess APITypes.ReferrerInfo)
getReferrerInfo person shouldShareReferrerInfo mbCustomerLocation = \case
  Rider _ referrer referrerStats -> do
    let avgRating = bool Nothing (Just . toCentesimal $ div' referrer.totalRatingScore referrer.totalRatings) (referrer.totalRatings > 0)
        referrerInfo =
          APITypes.ReferrerInfo
            { firstName = referrer.firstName,
              middleName = referrer.middleName,
              lastName = referrer.lastName,
              rating = avgRating,
              registeredAt = referrer.createdAt,
              totalRides = referrerStats.completedRides,
              vehicleNumber = Nothing,
              vehicleVariant = Nothing,
              applicableServiceTiers = Nothing,
              referrerImageUri = Nothing
            }
    return $ bool (Left APISuccess.Success) (Right referrerInfo) shouldShareReferrerInfo
  Driver refCode mobileNumber countryCode ->
    case person.referralCode of
      Just code | not shouldShareReferrerInfo && code /= refCode -> do
        throwError (InvalidRequest "Driver Referral Code is not same")
      Just code | not shouldShareReferrerInfo && code == refCode -> do
        return $ Left APISuccess.Success
      _ -> do
        merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
        merchantOpCity <- QMerchantO.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
        let alreadyReferred = isJust person.referralCode
        isMultipleDeviceIdExist <-
          maybe
            (return Nothing)
            ( \deviceId -> do
                personsWithSameDeviceId <- QPerson.findAllByDeviceId (Just deviceId)
                return $ Just (length personsWithSameDeviceId > 1)
            )
            (bool person.deviceId Nothing alreadyReferred)
        mOpCityId <- merchantOpCity.driverOfferMerchantOperatingCityId & fromMaybeM (MerchantOperatingCityNotFound $ "Need driver offer merchant operating city id to be congifured in rider side merchant operating city table.")
        DReferral.LinkRefereeRes res <- CallBPPInternal.linkReferee merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mOpCityId refCode mobileNumber countryCode isMultipleDeviceIdExist (Just alreadyReferred) (Just shouldShareReferrerInfo) mbCustomerLocation
        return $
          mapRight
            ( \info ->
                APITypes.ReferrerInfo
                  { firstName = Just info.firstName,
                    middleName = info.middleName,
                    lastName = info.lastName,
                    rating = info.rating,
                    registeredAt = info.registeredAt,
                    totalRides = info.totalRides,
                    vehicleNumber = Just info.vehicleNumber,
                    vehicleVariant = Just info.vehicleVariant,
                    applicableServiceTiers = Just info.applicableServiceTiers,
                    referrerImageUri = info.driverImage
                  }
            )
            res
