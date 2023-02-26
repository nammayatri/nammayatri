module Domain.Action.Internal.DriverReferee where

import Data.OpenApi (ToSchema)
import qualified Data.Text as DT
import qualified Domain.Types.DriverReferral as Domain
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (DbHash (DbHash))
import qualified Kernel.Storage.Esqueleto as ESQ
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.RiderDetails as QRD

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Id Domain.DriverReferral,
    customerNumberHash :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

linkReferee ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id Merchant ->
  SignatureAuthResult ->
  RefereeLinkInfoReq ->
  m APISuccess
linkReferee merchantId _ RefereeLinkInfoReq {..} = do
  _ <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (DT.length referralCode.getId == 6) $
    throwError $ InvalidRequest "Referral code length should be 6"
  driverReferralLinkage <- QDR.findByRefferalCode referralCode >>= fromMaybeM (InvalidRequest "Invalid referral code.")
  ESQ.runTransaction $
    QRD.updateReferralInfo (DbHash $ encodeUtf8 customerNumberHash) referralCode driverReferralLinkage.driverId
  pure Success
