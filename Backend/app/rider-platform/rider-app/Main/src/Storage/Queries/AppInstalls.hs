module Storage.Queries.AppInstalls where

import Domain.Types.AppInstalls as AppInstalls
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Version
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
