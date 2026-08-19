module Domain.Action.UI.AssetReleaseInternal
  ( postAssetReleasePublish,
    postAssetReleaseRollback,
    getAssetRelease,
  )
where

import qualified API.Types.UI.AssetReleaseInternal
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Domain.Types.AssetRelease
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.AssetRelease as CQAssetRelease
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AssetRelease as QAssetRelease

postAssetReleasePublish ::
  ( Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    API.Types.UI.AssetReleaseInternal.AssetPublishReq ->
    Environment.Flow API.Types.UI.AssetReleaseInternal.AssetPublishResp
  )
postAssetReleasePublish mbToken req = do
  checkPublisherKey mbToken
  sha256 <- normalizeSha256 req.sha256
  when (req.sizeBytes <= 0) $ throwError (InvalidRequest "sizeBytes must be positive")
  (merchantId, merchantOperatingCityId) <- resolveCity req.merchantShortId req.city
  mbLatest <- findLatest req.assetType merchantId merchantOperatingCityId
  case mbLatest of
    Just latest
      | latest.sha256 == sha256 && latest.url == req.url && latest.version == req.version ->
        pure $
          API.Types.UI.AssetReleaseInternal.AssetPublishResp
            { releaseId = latest.id.getId,
              changed = False,
              version = latest.version,
              sha256 = latest.sha256
            }
    _ -> do
      now <- getCurrentTime
      releaseId <- generateGUID
      QAssetRelease.create
        Domain.Types.AssetRelease.AssetRelease
          { id = Id releaseId,
            assetType = req.assetType,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            version = req.version,
            url = req.url,
            sha256 = sha256,
            sizeBytes = req.sizeBytes,
            sourceRef = req.sourceRef,
            rolledBackAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
      CQAssetRelease.clearCache req.assetType merchantId merchantOperatingCityId
      logInfo $
        "AssetRelease published: " <> show req.assetType <> " " <> req.merchantShortId <> "/" <> show req.city
          <> " releaseId="
          <> releaseId
          <> " version="
          <> req.version
          <> " sha256="
          <> sha256
      pure $
        API.Types.UI.AssetReleaseInternal.AssetPublishResp
          { releaseId = releaseId,
            changed = True,
            version = req.version,
            sha256 = sha256
          }

postAssetReleaseRollback ::
  ( Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    API.Types.UI.AssetReleaseInternal.AssetRollbackReq ->
    Environment.Flow API.Types.UI.AssetReleaseInternal.AssetRollbackResp
  )
postAssetReleaseRollback mbToken req = do
  checkPublisherKey mbToken
  target <-
    QAssetRelease.findByPrimaryKey (Id req.targetReleaseId)
      >>= fromMaybeM (InvalidRequest $ "No release with id " <> req.targetReleaseId)
  current <-
    findLatest target.assetType target.merchantId target.merchantOperatingCityId
      >>= fromMaybeM (InvalidRequest $ "No current release for " <> show target.assetType)
  when (target.id == current.id) $
    throwError (InvalidRequest "Rollback target is already the latest release")
  now <- getCurrentTime
  releaseId <- generateGUID
  QAssetRelease.create target {Domain.Types.AssetRelease.id = Id releaseId, Domain.Types.AssetRelease.rolledBackAt = Nothing, Domain.Types.AssetRelease.createdAt = now, Domain.Types.AssetRelease.updatedAt = now}
  QAssetRelease.updateRolledBackAt (Just now) current.id
  CQAssetRelease.clearCache target.assetType target.merchantId target.merchantOperatingCityId
  logInfo $
    "AssetRelease rolled back: " <> show target.assetType
      <> " from "
      <> current.id.getId
      <> " to "
      <> target.id.getId
      <> " as "
      <> releaseId
  pure $
    API.Types.UI.AssetReleaseInternal.AssetRollbackResp
      { releaseId = releaseId,
        version = target.version,
        sha256 = target.sha256,
        rolledBackReleaseId = current.id.getId
      }

getAssetRelease ::
  ( Kernel.Prelude.Maybe Domain.Types.AssetRelease.AssetType ->
    Kernel.Prelude.Maybe Context.City ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Environment.Flow (Kernel.Prelude.Maybe API.Types.UI.AssetReleaseInternal.AssetReleaseResp)
  )
getAssetRelease mbAssetType mbCity mbMerchantShortId mbToken = do
  checkPublisherKey mbToken
  assetType <- mbAssetType & fromMaybeM (InvalidRequest "assetType is required")
  merchantShortId <- mbMerchantShortId & fromMaybeM (InvalidRequest "merchantShortId is required")
  city <- mbCity & fromMaybeM (InvalidRequest "city is required")
  (merchantId, merchantOperatingCityId) <- resolveCity merchantShortId city
  mbRelease <- findLatest assetType merchantId merchantOperatingCityId
  pure $ toResp <$> mbRelease

findLatest ::
  Domain.Types.AssetRelease.AssetType ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Environment.Flow (Kernel.Prelude.Maybe Domain.Types.AssetRelease.AssetRelease)
findLatest assetType merchantId merchantOperatingCityId =
  Kernel.Prelude.listToMaybe <$> QAssetRelease.findLatestByAssetTypeAndCity (Just 1) Nothing assetType merchantId merchantOperatingCityId

checkPublisherKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow ()
checkPublisherKey mbToken = do
  assetPublisherApiKey <- asks (.assetPublisherApiKey)
  unless (Just assetPublisherApiKey == mbToken) $
    throwError (AuthBlocked "Invalid asset publisher api key")

normalizeSha256 :: Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text
normalizeSha256 sha256 = do
  let normalized = T.toLower (T.strip sha256)
  unless (T.length normalized == 64 && T.all Char.isHexDigit normalized) $
    throwError (InvalidRequest "sha256 must be 64 hex characters")
  pure normalized

resolveCity ::
  Kernel.Prelude.Text ->
  Context.City ->
  Environment.Flow (Id Domain.Types.Merchant.Merchant, Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
resolveCity merchantShortId city = do
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity (ShortId merchantShortId) city
      >>= fromMaybeM (InvalidRequest $ "No operating city " <> show city <> " for merchant " <> merchantShortId)
  pure (merchantOperatingCity.merchantId, merchantOperatingCity.id)

toResp :: Domain.Types.AssetRelease.AssetRelease -> API.Types.UI.AssetReleaseInternal.AssetReleaseResp
toResp release =
  API.Types.UI.AssetReleaseInternal.AssetReleaseResp
    { releaseId = release.id.getId,
      assetType = release.assetType,
      version = release.version,
      url = release.url,
      sha256 = release.sha256,
      sizeBytes = release.sizeBytes,
      sourceRef = release.sourceRef,
      publishedAt = release.createdAt
    }
