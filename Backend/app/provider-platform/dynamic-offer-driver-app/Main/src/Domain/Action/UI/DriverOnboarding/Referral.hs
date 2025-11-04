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
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverReferral as DR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.TransporterConfig
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation (Validate)
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation, validateField)
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverOnboarding as DomainRC
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
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

data ReferralType = DriverReferral | FleetReferral
  deriving (Eq, Show)

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
  TransporterConfig ->
  Id Person.Person ->
  Text ->
  Maybe Person.Role ->
  Flow DR.DriverReferral
validateReferralCodeAndRole transporterConfig personId value mbRole = do
  dr <- B.runInReplica (QDR.findByRefferalCode $ Id value) >>= fromMaybeM (InvalidReferralCode value)
  let role = fromMaybe Person.DRIVER mbRole
  unless (role == dr.role && personId /= dr.driverId) $ throwError (InvalidRequest "Invalid referral role")
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
  if isJust di.referralCode || isJust di.referredByDriverId || isJust di.referredByOperatorId
    then return AlreadyReferred
    else do
      transporterConfig <- CCT.findByMerchantOpCityId (cast merchantOpCityId) Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)
      dr <- validateReferralCodeAndRole transporterConfig personId req.value req.role
      case dr.role of
        Person.DRIVER -> do
          DriverInformation.addReferralCode (Just req.value) (Just dr.driverId) personId
          referredByDriver <- B.runInReplica (DriverInformation.findById dr.driverId) >>= fromMaybeM DriverInfoNotFound
          let newtotalRef = fromMaybe 0 referredByDriver.totalReferred + 1
          DriverInformation.incrementReferralCountByPersonId (Just newtotalRef) dr.driverId
          return Success
        Person.OPERATOR -> do
          DriverInformation.updateReferredByOperatorId (Just dr.driverId.getId) personId
          driverOperatorAssData <- SA.makeDriverOperatorAssociation merchantId merchantOpCityId personId dr.driverId.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
          void $ QDOA.create driverOperatorAssData
          Analytics.handleDriverAnalyticsAndFlowStatus
            transporterConfig
            dr.driverId
            Nothing
            ( \driverInfo -> do
                when driverInfo.enabled $ Analytics.incrementOperatorAnalyticsDriverEnabled transporterConfig dr.driverId.getId
                Analytics.incrementOperatorAnalyticsActiveDriver transporterConfig dr.driverId.getId
            )
            ( \driverInfo -> do
                DDriverMode.incrementFleetOperatorStatusKeyForDriver Person.OPERATOR dr.driverId.getId driverInfo.driverFlowStatus
            )
          incrementOnboardedCount DriverReferral dr.driverId transporterConfig
          return Success
        _ -> throwError (InvalidRequest "Invalid referral role")

getReferredDrivers :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow GetReferredDriverRes
getReferredDrivers (personId, _, _) = do
  di <- B.runInReplica (DriverInformation.findById personId) >>= fromMaybeM DriverInfoNotFound
  let totalRef = fromMaybe 0 di.totalReferred
  pure $ GetReferredDriverRes {value = totalRef}

getDriverDetailsByReferralCode ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Text ->
  Maybe Person.Role ->
  Flow DriverReferralDetailsRes
getDriverDetailsByReferralCode (personId, _, merchantOpCityId) value mbRole = do
  when (T.length value < 6) $ throwError (InvalidRequest "Referral code should be at least 6 digits long")
  transporterConfig <- CCT.findByMerchantOpCityId (cast merchantOpCityId) Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)
  dr <- validateReferralCodeAndRole transporterConfig personId value mbRole
  person <- B.runInReplica (QPerson.findById dr.driverId) >>= fromMaybeM (PersonNotFound dr.driverId.getId)
  return $
    DriverReferralDetailsRes
      { driverId = dr.driverId,
        name = Just (person.firstName <> " " <> (fromMaybe "" person.middleName) <> " " <> (fromMaybe "" person.lastName)),
        role = dr.role
      }

makeDriverReferredByOperator ::
  forall m r.
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Id Person.Person ->
  m ()
makeDriverReferredByOperator merchantOpCityId driverId referredOperatorId = do
  di <- B.runInReplica (DriverInformation.findById driverId) >>= fromMaybeM DriverInfoNotFound
  unless (isJust di.referralCode || isJust di.referredByDriverId || isJust di.referredByOperatorId) $ do
    transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    DriverInformation.updateReferredByOperatorId (Just referredOperatorId.getId) driverId
    incrementOnboardedCount DriverReferral referredOperatorId transporterConfig

incrementOnboardedCount :: forall m r. (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ReferralType -> Id Person.Person -> TransporterConfig -> m ()
incrementOnboardedCount refType referredEntityId transporterConfig = do
  let lockKey = case refType of
        DriverReferral -> "Driver:Referral:Increment:"
        FleetReferral -> "Fleet:Referral:Increment:"

  Redis.withWaitAndLockRedis (lockKey <> getId referredEntityId) 10 5000 $ do
    incrementDriverStatsOnboardedCountInternal
    incrementDailyStatsOnboardedCountInternal
  where
    incrementDriverStatsOnboardedCountInternal :: m ()
    incrementDriverStatsOnboardedCountInternal = do
      let logTag = case refType of
            DriverReferral -> "INCREMENT_DRIVER_COUNT"
            FleetReferral -> "INCREMENT_FLEET_COUNT"

      mbDriverStats <- QDriverStats.findByPrimaryKey referredEntityId
      case mbDriverStats of
        Nothing -> do
          logTagError logTag ("DriverStats not found for operator " <> show referredEntityId)
          throwError $ InternalError "DriverStats not found for operator"
        Just driverStats -> do
          let (newCount, updateFunc) = case refType of
                DriverReferral ->
                  ( driverStats.numDriversOnboarded + 1,
                    QDriverStats.updateNumDriversOnboarded
                  )
                FleetReferral ->
                  ( driverStats.numFleetsOnboarded + 1,
                    QDriverStats.updateNumFleetsOnboarded
                  )
          updateFunc newCount referredEntityId
          logTagInfo logTag $
            "Successfully incremented "
              <> (if refType == DriverReferral then "driver" else "fleet owner")
              <> " count for "
              <> show referredEntityId
              <> " to "
              <> show newCount

    incrementDailyStatsOnboardedCountInternal :: m ()
    incrementDailyStatsOnboardedCountInternal = do
      let logTagPrefix = case refType of
            DriverReferral -> "DRIVER"
            FleetReferral -> "FLEET"

      localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      mbDailyStats <- QDailyStats.findByDriverIdAndDate referredEntityId (utctDay localTime)
      case mbDailyStats of
        Just stats -> do
          let (newCount, updateFunc) = case refType of
                DriverReferral ->
                  ( stats.numDriversOnboarded + 1,
                    QDailyStats.updateNumDriversOnboardedByDriverId
                  )
                FleetReferral ->
                  ( stats.numFleetsOnboarded + 1,
                    QDailyStats.updateNumFleetsOnboardedByDriverId
                  )
          updateFunc newCount referredEntityId (utctDay localTime)
          logTagInfo ("INCREMENT_DAILY_STATS_" <> logTagPrefix <> "_COUNT") $
            "Successfully incremented daily stats "
              <> (if refType == DriverReferral then "driver" else "fleet owner")
              <> " count for "
              <> show referredEntityId
              <> " to "
              <> show newCount
        Nothing -> createNewDailyStats localTime

    createNewDailyStats :: UTCTime -> m ()
    createNewDailyStats currentTime = do
      logDebug $ "DailyStats not found for driverId : " <> referredEntityId.getId
      newId <- generateGUIDText
      now <- getCurrentTime
      let (driverCount, fleetCount) = case refType of
            DriverReferral -> (1, 0)
            FleetReferral -> (0, 1)
          dailyStatsOfDriver' =
            DDS.DailyStats
              { id = newId,
                driverId = referredEntityId,
                totalEarnings = 0.0,
                numRides = 0,
                totalDistance = 0,
                tollCharges = 0.0,
                bonusEarnings = 0.0,
                merchantLocalDate = utctDay currentTime,
                currency = transporterConfig.currency,
                distanceUnit = Meter,
                activatedValidRides = 0,
                referralEarnings = 0.0,
                referralCounts = 0,
                payoutStatus = DDS.Initialized,
                payoutOrderId = Nothing,
                payoutOrderStatus = Nothing,
                createdAt = now,
                updatedAt = now,
                cancellationCharges = 0.0,
                tipAmount = 0.0,
                totalRideTime = 0,
                numDriversOnboarded = driverCount,
                numFleetsOnboarded = fleetCount,
                merchantId = Just transporterConfig.merchantId,
                merchantOperatingCityId = Just transporterConfig.merchantOperatingCityId,
                onlineDuration = Nothing
              }
      QDailyStats.create dailyStatsOfDriver'
      let logTagPrefix = if refType == DriverReferral then "DRIVER" else "FLEET"
      logTagInfo ("CREATE_DAILY_STATS_" <> logTagPrefix <> "_COUNT") $
        "Successfully created daily stats with "
          <> (if refType == DriverReferral then "driver" else "fleet owner")
          <> " count 1 for "
          <> show referredEntityId
