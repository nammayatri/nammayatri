module Domain.Action.UI.DriverReferral
  ( createDriverReferral,
    ReferralLinkReq (..),
    generateReferralCode,
    GenerateReferralCodeRes (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverReferral as D
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import qualified Storage.CachedQueries.DriverReferral as CQD
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.DriverReferral as QRD
import Tools.Error

data ReferralLinkReq = ReferralLinkReq
  { referralCode :: Text,
    referralLinkPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype GenerateReferralCodeRes = GenerateReferralCodeRes
  { referralCode :: Text
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
  ReferralLinkReq ->
  m APISuccess
createDriverReferral (driverId, _, merchantOpCityId) isDashboard ReferralLinkReq {..} = do
  unless (TU.validateAllDigitWithMinLength 6 referralCode) $
    throwError $ InvalidRequest "Referral Code must have 6 digits."
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId 0 Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
            linkedAt = now
          }

generateReferralCode ::
  ( HasCacheConfig r,
    Redis.HedisFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    MonadTime m
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m GenerateReferralCodeRes
generateReferralCode (driverId, _, _) = do
  mbReferralCodeWithDriver <- B.runInReplica $ QRD.findById driverId

  case mbReferralCodeWithDriver of
    Just driverReferral -> pure $ GenerateReferralCodeRes driverReferral.referralCode.getId
    Nothing -> do
      Redis.withLockRedisAndReturnValue makeLastRefferalCodeKey 60 $ do
        refferalCodeNumber <- CQD.getNextRefferalCode
        let referralCode' = T.pack $ formatReferralCode (show refferalCodeNumber)
        driverRefferalRecord <- mkDriverRefferalType referralCode'
        void (QRD.create driverRefferalRecord)
        pure $ GenerateReferralCodeRes referralCode'
  where
    mkDriverRefferalType rc = do
      now <- getCurrentTime
      pure $
        D.DriverReferral
          { referralCode = Id rc,
            driverId = cast driverId,
            linkedAt = now
          }
    formatReferralCode rc =
      let len = length rc
          zerosToAdd = max 0 (6 - len)
       in replicate zerosToAdd '0' ++ rc

makeLastRefferalCodeKey :: Text
makeLastRefferalCodeKey = "driver-offer:CachedQueries:DriverReferral:Id-getNextRefferalCode"
