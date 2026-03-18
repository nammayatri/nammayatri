module Domain.Action.UI.DriverReferral
  ( createDriverReferral,
    ReferralLinkReq (..),
    generateReferralCode,
    checkAndUpdateDynamicReferralCode,
    GenerateReferralCodeRes (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverReferral as D
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DriverReferral as CQD
import qualified Storage.Queries.DriverReferral as QRD
import qualified Storage.Queries.Person as QP
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

data ReferralLinkReq = ReferralLinkReq
  { referralCode :: Text,
    referralLinkPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GenerateReferralCodeRes = GenerateReferralCodeRes
  { referralCode :: Text,
    dynamicReferralCode :: Maybe Text,
    dynamicReferralCodeValidTill :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createDriverReferral ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    MonadTime m
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Bool ->
  SP.Role ->
  ReferralLinkReq ->
  m APISuccess
createDriverReferral (driverId, merchantId, merchantOpCityId) isDashboard role ReferralLinkReq {..} = do
  unless (TU.validateAllDigitWithMinLength 6 referralCode) $
    throwError $ InvalidRequest "Referral Code must have greater than equal to 6 digits."
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (transporterConfig.referralLinkPassword /= referralLinkPassword && not isDashboard) $
    throwError $ InvalidRequest "Invalid Password."
  mbLastReferralCodeWithDriver <- B.runInReplica $ QRD.findById driverId
  whenJust mbLastReferralCodeWithDriver $ \lastReferralCodeWithDriver ->
    unless (lastReferralCodeWithDriver.referralCode.getId == referralCode) $ throwError (InvalidRequest $ "DriverId: " <> driverId.getId <> " already linked with some referralCode.")
  mbReferralCodeAlreadyLinked <- B.runInReplica $ QRD.findByRefferalCode $ Id referralCode
  whenJust mbReferralCodeAlreadyLinked $ \referralCodeAlreadyLinked ->
    unless (referralCodeAlreadyLinked.driverId == driverId) $ throwError (InvalidRequest $ "RefferalCode: " <> referralCode <> " already linked with some other account.")
  driverRefferalRecord <- mkDriverRefferalType referralCode
  when (all isNothing [mbReferralCodeAlreadyLinked, mbLastReferralCodeWithDriver]) $
    void (QRD.create driverRefferalRecord)
  pure Success
  where
    mkDriverRefferalType rc = do
      now <- getCurrentTime
      pure $
        D.DriverReferral
          { referralCode = Id rc,
            driverId = cast driverId,
            dynamicReferralCode = Nothing,
            dynamicReferralCodeValidTill = Nothing,
            linkedAt = now,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            role
          }

checkAndUpdateDynamicReferralCode ::
  ( HasCacheConfig r,
    HasCacConfig r,
    HasInMemEnv r,
    Redis.HedisFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    MonadTime m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Bool ->
  D.DriverReferral ->
  m D.DriverReferral
checkAndUpdateDynamicReferralCode merchantId merchantOperatingCityId transporterConfig isDriverOnRide driverReferralObj = do
  now <- getCurrentTime
  mbDynamicCodeUpdates <-
    case driverReferralObj.dynamicReferralCodeValidTill of
      Just validTill
        | now >= validTill && isDriverOnRide ->
          maybe
            (generateDynamicReferralCode now)
            (\drc -> pure $ Just (drc, getDynamicReferralCodeValidTill now)) -- increasing the validity of same referralCode
            driverReferralObj.dynamicReferralCode
      Just validTill
        | now < validTill ->
          pure Nothing
      _ ->
        generateDynamicReferralCode now
  case mbDynamicCodeUpdates of
    Just (dynamicReferralCode, dynamicReferralCodeValidTill) -> do
      QRD.updateDynamicReferralCode (Just dynamicReferralCode) (Just dynamicReferralCodeValidTill) (Just merchantOperatingCityId) (Just merchantId) driverReferralObj.driverId
      pure $ driverReferralObj {D.dynamicReferralCode = Just dynamicReferralCode, D.dynamicReferralCodeValidTill = Just dynamicReferralCodeValidTill}
    Nothing -> pure driverReferralObj
  where
    getDynamicReferralCodeValidTill now = addUTCTime (fromIntegral transporterConfig.dynamicReferralCodeValidForMinutes) now

    generateDynamicReferralCode now = do
      dynamicReferralCode <- CQD.getDynamicRefferalCode
      let dynamicReferralCodeValidTill = getDynamicReferralCodeValidTill now
      let dynamicReferralCode' = T.pack $ formatReferralCode (show dynamicReferralCode) 4
      return $ Just (dynamicReferralCode', dynamicReferralCodeValidTill)

generateReferralCode ::
  ( HasCacheConfig r,
    HasCacConfig r,
    HasInMemEnv r,
    Redis.HedisFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    MonadTime m
  ) =>
  Maybe SP.Role ->
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m GenerateReferralCodeRes
generateReferralCode mbRole (driverId, merchantId, merchantOpCityId) = do
  mbReferralCodeWithDriver <- B.runInReplica $ QRD.findById driverId
  role <- case mbRole of
    Just role -> pure role
    Nothing -> do
      person <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      pure person.role
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case mbReferralCodeWithDriver of
    Just driverReferral -> pure $ GenerateReferralCodeRes driverReferral.referralCode.getId driverReferral.dynamicReferralCode driverReferral.dynamicReferralCodeValidTill
    Nothing -> do
      Redis.runInMasterCloudRedisCell $
        Redis.withLockRedisAndReturnValue makeLastRefferalCodeKey 60 $ do
          refferalCodeNumber <- CQD.getNextRefferalCode
          dynamicReferralCode <- CQD.getDynamicRefferalCode
          let referralCode' = T.pack $ formatReferralCode (show refferalCodeNumber) 6
          let dynamicReferralCode' = T.pack $ formatReferralCode (show dynamicReferralCode) 4
          driverRefferalRecord <- mkDriverRefferalType referralCode' (Just dynamicReferralCode') transporterConfig role
          void (QRD.create driverRefferalRecord)
          pure $ GenerateReferralCodeRes referralCode' driverRefferalRecord.dynamicReferralCode driverRefferalRecord.dynamicReferralCodeValidTill
  where
    mkDriverRefferalType rc dynamicReferralCode transporterConfig roleData = do
      now <- getCurrentTime
      let dynamicReferralCodeValidTill = Just $ addUTCTime (fromIntegral transporterConfig.dynamicReferralCodeValidForMinutes) now
      pure $
        D.DriverReferral
          { referralCode = Id rc,
            dynamicReferralCode,
            dynamicReferralCodeValidTill,
            driverId = cast driverId,
            linkedAt = now,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            role = roleData
          }

formatReferralCode :: String -> Int -> String
formatReferralCode rc codeMinLength =
  let len = length rc
      zerosToAdd = max 0 (codeMinLength - len)
   in replicate zerosToAdd '0' ++ rc

makeLastRefferalCodeKey :: Text
makeLastRefferalCodeKey = "driver-offer:CachedQueries:DriverReferral:Id-getNextRefferalCode"
