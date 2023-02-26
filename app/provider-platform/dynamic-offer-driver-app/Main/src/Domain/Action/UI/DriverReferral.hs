module Domain.Action.UI.DriverReferral
  ( createDriverReferral,
    ReferralLinkReq (..),
  )
where

import qualified Data.Text as DT
import qualified Domain.Types.DriverReferral as D
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
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
  case mbRefCodeLinkage of
    Just refCodeLinkage ->
      if refCodeLinkage.driverId == driverId
        then pure Success -- idempotent behaviour
        else throwError (InvalidRequest $ "RefferalCode: " <> referralCode <> " already linked with some other account.")
    Nothing -> do
      transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound person.merchantId.getId)
      unless (transporterConfig.referralLinkPassword == referralLinkPassword) $
        throwError $ InvalidRequest "Invalid Password."
      unless (DT.length referralCode == 6) $
        throwError $ InvalidRequest "Referral Code must be of length 6."
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
