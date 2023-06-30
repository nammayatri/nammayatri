module Storage.Queries.AppInstalls where

import Domain.Types.AppInstalls as AppInstalls
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Types.Version
import Kernel.Utils.Version
import qualified Storage.Beam.AppInstalls as BeamAI
import Storage.Tabular.AppInstalls

upsert :: AppInstalls.AppInstalls -> SqlDB ()
upsert appInstalls =
  Esq.upsert
    appInstalls
    [ AppInstallsDeviceToken =. val (appInstalls.deviceToken),
      AppInstallsSource =. val (appInstalls.source),
      AppInstallsMerchantId =. val (toKey appInstalls.merchantId),
      AppInstallsAppVersion =. val (versionToText <$> appInstalls.appVersion),
      AppInstallsBundleVersion =. val (versionToText <$> appInstalls.bundleVersion),
      AppInstallsPlatform =. val (appInstalls.platform),
      AppInstallsUpdatedAt =. val (appInstalls.updatedAt)
    ]

transformBeamAppInstallsToDomain :: (L.MonadFlow m, Log m) => BeamAI.AppInstalls -> m (Maybe AppInstalls)
transformBeamAppInstallsToDomain BeamAI.AppInstallsT {..} = do
  bundleVersion' <- forM bundleVersion readVersion
  appVersion' <- forM appVersion readVersion
  pure $
    Just
      AppInstalls
        { id = Id id,
          deviceToken = deviceToken,
          source = source,
          merchantId = Id merchantId,
          appVersion = appVersion',
          bundleVersion = bundleVersion',
          platform = platform,
          createdAt = createdAt,
          updatedAt = updatedAt
        }

transformDomainAppInstallsToBeam :: AppInstalls -> BeamAI.AppInstalls
transformDomainAppInstallsToBeam AppInstalls {..} =
  BeamAI.defaultAppInstalls
    { BeamAI.id = getId id,
      BeamAI.deviceToken = deviceToken,
      BeamAI.source = source,
      BeamAI.merchantId = getId merchantId,
      BeamAI.appVersion = versionToText <$> appVersion,
      BeamAI.bundleVersion = versionToText <$> bundleVersion,
      BeamAI.platform = platform,
      BeamAI.createdAt = createdAt,
      BeamAI.updatedAt = updatedAt
    }
