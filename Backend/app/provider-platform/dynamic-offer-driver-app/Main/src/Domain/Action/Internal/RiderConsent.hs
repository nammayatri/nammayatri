module Domain.Action.Internal.RiderConsent
  ( RiderConsentItem (..),
    SetRiderConsentReq (..),
    SetRiderConsentRes (..),
    setRiderConsent,
  )
where

import qualified Domain.Types.Merchant as DM
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.RiderDetails as SRD
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.RiderDetailsExtra as QRDE

data RiderConsentItem = RiderConsentItem
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    city :: Context.City,
    consentToShareMobileNumber :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SetRiderConsentReq = SetRiderConsentReq
  { bapId :: Text,
    riders :: [RiderConsentItem]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SetRiderConsentRes = SetRiderConsentRes
  { updated :: Int,
    created :: Int,
    failedIndices :: [Int]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Outcome = Updated | Created

setRiderConsent :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id DM.Merchant -> Maybe Text -> SetRiderConsentReq -> m SetRiderConsentRes
setRiderConsent merchantId apiKey req = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  results <- forM (zip [0 ..] req.riders) $ \(idx, item) -> do
    res <- withTryCatch "setRiderConsent" (applyOne merchant item)
    case res of
      Left err -> do
        logError $ "setRiderConsent: failed for item index " <> show idx <> ": " <> show err
        pure (Left idx)
      Right outcome -> pure (Right outcome)
  pure
    SetRiderConsentRes
      { updated = length [() | Right Updated <- results],
        created = length [() | Right Created <- results],
        failedIndices = [i | Left i <- results]
      }
  where
    applyOne merchant item = do
      numberHash <- getDbHash item.customerMobileNumber
      QRDE.findByMobileNumberHashAndMerchant numberHash merchant.id >>= \case
        Just rider -> do
          QRD.updateConsentToShareMobileNumber (Just item.consentToShareMobileNumber) rider.id
          pure Updated
        Nothing -> do
          moc <- CQMOC.findByMerchantIdAndCity merchant.id item.city >>= fromMaybeM (MerchantOperatingCityNotFound (show item.city))
          (riderDetails, _) <- SRD.getRiderDetails moc.currency merchant.id (Just moc.id) item.customerMobileCountryCode item.customerMobileNumber req.bapId False (Just item.consentToShareMobileNumber)
          QRD.create riderDetails
          pure Created
