module Storage.Queries.AppInstallsExtra where

import Domain.Types.AppInstalls as AppInstalls
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.AppInstalls as BeamAI
import Storage.Queries.OrphanInstances.AppInstalls ()

-- Extra code goes here --

upsert :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => AppInstalls.AppInstalls -> m ()
upsert a@AppInstalls {..} = do
  res <- findOneWithKV [Se.And [Se.Is BeamAI.merchantId $ Se.Eq (getId a.merchantId), Se.Is BeamAI.source $ Se.Eq a.source, Se.Is BeamAI.deviceToken $ Se.Eq a.deviceToken]]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamAI.deviceToken deviceToken,
          Se.Set BeamAI.source source,
          Se.Set BeamAI.merchantId $ getId merchantId,
          Se.Set BeamAI.appVersion (versionToText <$> appVersion),
          Se.Set BeamAI.bundleVersion (versionToText <$> bundleVersion),
          Se.Set BeamAI.platform platform,
          Se.Set BeamAI.updatedAt updatedAt
        ]
        [Se.And [Se.Is BeamAI.merchantId $ Se.Eq (getId a.merchantId), Se.Is BeamAI.source $ Se.Eq a.source, Se.Is BeamAI.deviceToken $ Se.Eq a.deviceToken]]
    else createWithKV a
