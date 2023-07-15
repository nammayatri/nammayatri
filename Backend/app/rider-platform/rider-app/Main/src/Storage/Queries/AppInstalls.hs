module Storage.Queries.AppInstalls where

import Domain.Types.AppInstalls as AppInstalls
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Types.Version
import Kernel.Utils.Version
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.AppInstalls as BeamAI

upsert :: (L.MonadFlow m, Log m) => AppInstalls.AppInstalls -> m ()
upsert a@AppInstalls {..} = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamAI.AppInstallsT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      res <- do
        result <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamAI.id $ Se.Eq (getId a.id)]
        case result of
          Right (Just result') -> transformBeamAppInstallsToDomain result'
          _ -> pure Nothing
      if isJust res
        then
          void $
            KV.updateWoReturningWithKVConnector
              dbCOnf'
              updatedMeshConfig
              [ Se.Set BeamAI.deviceToken deviceToken,
                Se.Set BeamAI.source source,
                Se.Set BeamAI.merchantId $ getId merchantId,
                Se.Set BeamAI.appVersion (versionToText <$> appVersion),
                Se.Set BeamAI.bundleVersion (versionToText <$> bundleVersion),
                Se.Set BeamAI.platform platform,
                Se.Set BeamAI.updatedAt updatedAt
              ]
              [Se.Is BeamAI.id (Se.Eq $ getId id)]
        else void $ KV.createWoReturingKVConnector dbCOnf' updatedMeshConfig (transformDomainAppInstallsToBeam a)
    Nothing -> pure ()

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
  BeamAI.AppInstallsT
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
