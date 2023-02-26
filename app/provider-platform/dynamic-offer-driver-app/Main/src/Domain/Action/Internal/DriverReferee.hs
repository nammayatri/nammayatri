module Domain.Action.Internal.DriverReferee where

import qualified Data.Aeson as A
import Data.OpenApi (ToSchema)
import qualified Data.Text as DT
import qualified Domain.Types.DriverReferral as Domain
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as ESQ
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.RiderDetails as QRD

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Id Domain.DriverReferral,
    customerNumberHash :: A.Value
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

linkReferee ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id Merchant ->
  Maybe Text ->
  RefereeLinkInfoReq ->
  m APISuccess
linkReferee merchantId apiKey RefereeLinkInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mobileHash <-
    case A.fromJSON customerNumberHash of
      A.Success hash -> pure hash
      A.Error err -> logTagInfo "DECODING_CUSTOMER_NUMBER_HASH_FAILED" (show err) *> throwError (InternalError "Failed to read the hashed mobile number")
  _ <- QRD.findByMobileNumberHash mobileHash >>= fromMaybeM (InvalidRequest $ "PersonNotFound with mobilehash: " <> show mobileHash)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  unless (DT.length referralCode.getId == 6) $
    throwError $ InvalidRequest "Referral code length should be 6"
  driverReferralLinkage <- QDR.findByRefferalCode referralCode >>= fromMaybeM (InvalidRequest "Invalid referral code.")
  ESQ.runTransaction $
    QRD.updateReferralInfo mobileHash referralCode driverReferralLinkage.driverId
  pure Success
