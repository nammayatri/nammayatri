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
  ReferralLinkReq ->
  m APISuccess
createDriverReferral driverId ReferralLinkReq {..} = do
  person <- Esq.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbRefCodeLinkage <- QRD.findByRefferalCode $ Id referralCode
  transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound person.merchantId.getId)
  unless (transporterConfig.referralLinkPassword == referralLinkPassword) $
    throwError $ InvalidRequest "Invalid Password."
  case mbRefCodeLinkage of
    Just refCodeLinkage ->
      if refCodeLinkage.driverId == driverId
        then pure Success -- idempotent behaviour
        else throwError (InvalidRequest $ "RefferalCode: " <> referralCode <> " already linked with some other account.")
    Nothing -> do
      unless (TU.validateAllDigitWithMinLength 6 referralCode) $
        throwError $ InvalidRequest "Referral Code must have 6 digits."
      driverRefferalRecord <- mkDriverRefferalType referralCode
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
