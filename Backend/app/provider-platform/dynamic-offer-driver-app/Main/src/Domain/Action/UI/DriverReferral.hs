module Domain.Action.UI.DriverReferral
  ( createDriverReferral,
    ReferralLinkReq (..),
  )
where

import qualified Domain.Types.DriverReferral as D
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.CachedQueries.TransporterConfig as QTC
import qualified Storage.Queries.DriverReferral as QRD
import qualified Storage.Queries.Person as QP
import Tools.Error

data ReferralLinkReq = ReferralLinkReq
  { referralCode :: Text,
    referralLinkPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createDriverReferral ::
  ( HasCacheConfig r,
    Redis.HedisFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id SP.Person ->
  Bool ->
  ReferralLinkReq ->
  m APISuccess
createDriverReferral driverId isDashboard ReferralLinkReq {..} = do
  unless (TU.validateAllDigitWithMinLength 6 referralCode) $
    throwError $ InvalidRequest "Referral Code must have 6 digits."
  person <- Esq.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound person.merchantId.getId)
  when (transporterConfig.referralLinkPassword /= referralLinkPassword && not isDashboard) $
    throwError $ InvalidRequest "Invalid Password."
  mbLastReferralCodeWithDriver <- Esq.runInReplica $ QRD.findById driverId
  whenJust mbLastReferralCodeWithDriver $ \lastReferralCodeWithDriver ->
    unless (lastReferralCodeWithDriver.referralCode.getId == referralCode) $ throwError (InvalidRequest $ "DriverId: " <> driverId.getId <> " already linked with some referralCode.")
  mbReferralCodeAlreadyLinked <- Esq.runInReplica $ QRD.findByRefferalCode $ Id referralCode
  whenJust mbReferralCodeAlreadyLinked $ \referralCodeAlreadyLinked ->
    unless (referralCodeAlreadyLinked.driverId == driverId) $ throwError (InvalidRequest $ "RefferalCode: " <> referralCode <> " already linked with some other account.")
  driverRefferalRecord <- mkDriverRefferalType referralCode
  when (all isNothing [mbReferralCodeAlreadyLinked, mbLastReferralCodeWithDriver]) $
    Esq.runTransaction $ QRD.create driverRefferalRecord
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
