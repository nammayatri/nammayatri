module Domain.Action.UI.AssetManifest (getAssetManifest) where

import qualified API.Types.UI.AssetManifest
import Data.List (nub)
import qualified Domain.Types.AssetRelease
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.AssetRelease as CQAssetRelease
import qualified Storage.Queries.Person as QP

getAssetManifest ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe [Domain.Types.AssetRelease.AssetType] ->
    Environment.Flow API.Types.UI.AssetManifest.AssetManifestResp
  )
getAssetManifest (mbPersonId, merchantId) mbAssetTypes = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  assetTypes <- case mbAssetTypes of
    Just types@(_ : _) -> pure (nub types)
    _ -> throwError (InvalidRequest "assetTypes is required")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  releases <- CQAssetRelease.findAllLatest assetTypes merchantId person.merchantOperatingCityId
  pure $ API.Types.UI.AssetManifest.AssetManifestResp {assets = map toEntry releases}

toEntry :: Domain.Types.AssetRelease.AssetRelease -> API.Types.UI.AssetManifest.AssetEntry
toEntry release =
  API.Types.UI.AssetManifest.AssetEntry
    { assetType = release.assetType,
      version = release.version,
      url = release.url,
      sha256 = release.sha256,
      publishedAt = release.createdAt
    }
