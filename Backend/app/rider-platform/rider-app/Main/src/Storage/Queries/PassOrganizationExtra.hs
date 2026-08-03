{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassOrganizationExtra where

import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.PassType as DPassType
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.Common as BeamCommon
import Storage.Queries.OrphanInstances.PassOrganization

-- Extra code goes here --

-- Organizations for a city and pass type, optionally narrowed by a
-- case-insensitive substring of the organization name. Hand-written because
-- Sequelize has no ILIKE term and cannot wrap a column in lower(), so this
-- reads the replica directly instead of going through KV.
findAllByMerchantOperatingCityIdAndPassEnumWithSearch ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DPassType.PassEnum ->
  Maybe Text ->
  Int ->
  Int ->
  m [DPassOrganization.PassOrganization]
findAllByMerchantOperatingCityIdAndPassEnumWithSearch merchantOperatingCityId passEnum mbSearchString limitVal offsetVal = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limitVal) $
            B.offset_ (fromIntegral offsetVal) $
              B.orderBy_ (\passOrganization -> B.asc_ passOrganization.name) $
                B.filter_'
                  ( \passOrganization ->
                      passOrganization.merchantOperatingCityId B.==?. B.val_ (getId merchantOperatingCityId)
                        B.&&?. passOrganization.passEnum B.==?. B.val_ passEnum
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          (\searchString -> B.sqlBool_ (B.lower_ passOrganization.name `B.like_` B.val_ ("%" <> T.toLower searchString <> "%")))
                          mbSearchString
                  )
                  $ B.all_ (BeamCommon.passOrganization BeamCommon.atlasDB)
  case res of
    Right passOrganizations -> catMaybes <$> mapM fromTType' passOrganizations
    Left err -> do
      logError $ "Failed to fetch pass organizations: " <> show err
      pure []
