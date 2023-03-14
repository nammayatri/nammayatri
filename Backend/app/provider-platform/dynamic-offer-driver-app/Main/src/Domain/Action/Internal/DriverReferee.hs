module Domain.Action.Internal.DriverReferee where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DriverReferral as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.RiderDetails as DRD
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt, getDbHash)
import qualified Kernel.Storage.Esqueleto as ESQ
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.RiderDetails as QRD

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Id Domain.DriverReferral,
    customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

linkReferee ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    MonadReader r m
  ) =>
  Id Merchant ->
  Maybe Text ->
  RefereeLinkInfoReq ->
  m APISuccess
linkReferee merchantId apiKey RefereeLinkInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  unless (TU.validateAllDigitWithMinLength 6 referralCode.getId) $
    throwError $ InvalidRequest "Referral Code must have 6 digits"
  numberHash <- getDbHash customerMobileNumber
  driverReferralLinkage <- QDR.findByRefferalCode referralCode >>= fromMaybeM (InvalidRequest "Invalid referral code.")
  mbRiderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id
  case mbRiderDetails of
    Just _ -> ESQ.runTransaction $ QRD.updateReferralInfo numberHash merchant.id referralCode driverReferralLinkage.driverId
    Nothing -> do
      riderDetails <- mkRiderDetailsObj driverReferralLinkage.driverId
      ESQ.runTransaction $ QRD.create riderDetails
  driverReferral <- QDR.findById driverReferralLinkage.driverId >>= fromMaybeM (PersonNotFound driverReferralLinkage.driverId.getId)
  ESQ.runTransaction $ QDR.increaseReferredCustomerCount driverReferralLinkage.driverId driverReferral.referredCustomerCount
  pure Success
  where
    mkRiderDetailsObj driverId = do
      id <- generateGUID
      now <- getCurrentTime
      encPhoneNumber <- encrypt customerMobileNumber
      pure $
        DRD.RiderDetails
          { id = Id id,
            mobileCountryCode = customerMobileCountryCode,
            mobileNumber = encPhoneNumber,
            merchantId,
            createdAt = now,
            updatedAt = now,
            referralCode = Just referralCode,
            referredByDriver = Just driverId,
            referredAt = Just now,
            hasTakenValidRide = False,
            hasTakenValidRideAt = Nothing
          }
