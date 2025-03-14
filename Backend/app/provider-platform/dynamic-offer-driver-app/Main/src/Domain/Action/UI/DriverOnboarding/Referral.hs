{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.Referral where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import Domain.Types.DriverOperatorAssociation
import qualified Domain.Types.DriverReferral as DR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation (Validate)
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation, validateField)
import qualified SharedLogic.DriverOnboarding as DomainRC
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.Person as QPerson
import Tools.Error

data ReferralReq = ReferralReq
  { value :: Text,
    role :: Maybe Person.Role
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data DriverReferralDetailsRes = DriverReferralDetailsRes
  { driverId :: Id Person.Person,
    name :: Maybe Text,
    role :: Person.Role
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype GetReferredDriverRes = GetReferredDriverRes
  {value :: Int}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data ReferralRes = Success | AlreadyReferred
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON ReferralRes where
  toJSON Success = A.object ["result" .= ("Success" :: Text)]
  toJSON AlreadyReferred = A.object ["result" .= ("AlreadyReferred" :: Text)]

instance FromJSON ReferralRes where
  parseJSON (A.Object obj) = do
    result :: String <- obj .: "result"
    case result of
      "Success" -> pure Success
      "AlreadyReferred" -> pure AlreadyReferred
      _ -> parseFail "Expected \"Success\""
  parseJSON err = typeMismatch "String" err

validateReferralReq :: Validate ReferralReq
validateReferralReq ReferralReq {..} =
  sequenceA_
    [ validateField "value" value $ MinLength 6
    ]

validateReferralCodeAndRole ::
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  ReferralReq ->
  Flow DR.DriverReferral
validateReferralCodeAndRole merchantOpCityId personId req = do
  dr <- B.runInReplica (QDR.findByRefferalCode $ Id req.value) >>= fromMaybeM (InvalidReferralCode req.value)
  let role = fromMaybe Person.DRIVER req.role
  unless (role == dr.role && personId /= dr.driverId) $ throwError (InvalidRequest "Invalid referral role")
  transporterConfig <- CCT.findByMerchantOpCityId (cast merchantOpCityId) Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)
  logTagInfo "validateReferralCodeAndRole" $ "transporterConfig allowedReferralEntities: " <> show transporterConfig.allowedReferralEntities
  unless (role `elem` transporterConfig.allowedReferralEntities) $ throwError (InvalidRequest "Referral not allowed for this merchant")
  return dr

addReferral ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ReferralReq ->
  Flow ReferralRes
addReferral (personId, merchantId, merchantOpCityId) req = do
  runRequestValidation validateReferralReq req
  di <- B.runInReplica (DriverInformation.findById personId) >>= fromMaybeM DriverInfoNotFound
  if isJust di.referralCode || isJust di.referredByDriverId
    then return AlreadyReferred
    else do
      dr <- validateReferralCodeAndRole merchantOpCityId personId req
      case dr.role of
        Person.DRIVER -> do
          DriverInformation.addReferralCode (Just req.value) (Just dr.driverId) personId
          referredByDriver <- B.runInReplica (DriverInformation.findById dr.driverId) >>= fromMaybeM DriverInfoNotFound
          let newtotalRef = fromMaybe 0 referredByDriver.totalReferred + 1
          DriverInformation.incrementReferralCountByPersonId (Just newtotalRef) dr.driverId
          return Success
        Person.OPERATOR -> do
          hasNoAssociation <- checkDriverHasNoAssociation personId
          if hasNoAssociation
            then do
              DriverInformation.updateReferredByOperatorId (Just dr.driverId.getId) personId
              driverOperatorAssData <- makeDriverOperatorAssociation merchantId merchantOpCityId personId dr.driverId.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
              void $ QDOA.create driverOperatorAssData
              incrementOnboardedDriverCountByOperator dr.driverId
              return Success
            else return AlreadyReferred
        _ -> throwError (InvalidRequest "Invalid referral role")

getReferredDrivers :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow GetReferredDriverRes
getReferredDrivers (personId, _, _) = do
  di <- B.runInReplica (DriverInformation.findById personId) >>= fromMaybeM DriverInfoNotFound
  let totalRef = fromMaybe 0 di.totalReferred
  pure $ GetReferredDriverRes {value = totalRef}

incrementOnboardedDriverCountByOperator :: Id Person.Person -> Flow ()
incrementOnboardedDriverCountByOperator referredOperatorId = do
  let lockKey = "Driver:Referral:Increment:" <> getId referredOperatorId
  Redis.withWaitAndLockRedis lockKey 10 5000 $ do
    mbDriverStats <- QDriverStats.findByPrimaryKey referredOperatorId
    case mbDriverStats of
      Nothing -> do
        logTagError "INCREMENT_DRIVER_COUNT" ("DriverStats not found for operator " <> show referredOperatorId)
        throwError $ InternalError "DriverStats not found for operator"
      Just driverStats -> do
        let newCount = driverStats.numDriversOnboarded + 1
        QDriverStats.updateNumDriversOnboarded newCount referredOperatorId
        logTagInfo "INCREMENT_DRIVER_COUNT" $ "Successfully incremented driver count for " <> show referredOperatorId <> " to " <> show newCount

makeDriverOperatorAssociation :: (MonadFlow m) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Text -> Maybe UTCTime -> m DriverOperatorAssociation
makeDriverOperatorAssociation merchantId merchantOpCityId driverId operatorId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DriverOperatorAssociation
      { id = id,
        operatorId = operatorId,
        isActive = True,
        driverId = driverId,
        associatedOn = Just now,
        associatedTill = end,
        onboardingVehicleCategory = Nothing,
        createdAt = now,
        updatedAt = now,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOpCityId
      }

checkDriverHasNoAssociation :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Person.Person -> m Bool
checkDriverHasNoAssociation driverId = do
  mbOperatorAssoc <- QDOA.findByDriverId (cast driverId) True
  mbFleetAssoc <- QFDA.findByDriverId (cast driverId) True
  pure $ isNothing mbOperatorAssoc && isNothing mbFleetAssoc

getDriverDetailsByReferralCode ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ReferralReq ->
  Flow DriverReferralDetailsRes
getDriverDetailsByReferralCode (personId, _, merchantOpCityId) req = do
  runRequestValidation validateReferralReq req
  dr <- validateReferralCodeAndRole merchantOpCityId personId req
  person <- B.runInReplica (QPerson.findById dr.driverId) >>= fromMaybeM (PersonNotFound dr.driverId.getId)
  return $
    DriverReferralDetailsRes
      { driverId = dr.driverId,
        name = Just (person.firstName <> " " <> (fromMaybe "" person.middleName) <> " " <> (fromMaybe "" person.lastName)),
        role = dr.role
      }
