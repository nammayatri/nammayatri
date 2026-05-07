{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassDetailsExtra where

import qualified Domain.Types.PassDetails as DPassDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PassDetails as Beam
import Storage.Queries.OrphanInstances.PassDetails ()

getLastReferenceNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  m Int
getLastReferenceNumber = do
  (passDetails :: [DPassDetails.PassDetails]) <-
    findAllWithOptionsDb
      [Se.Is Beam.referenceNumber $ Se.Not $ Se.Eq Nothing]
      (Se.Desc Beam.referenceNumber)
      (Just 1)
      (Just 0)
  pure $ case listToMaybe passDetails of
    Just pd -> fromMaybe 0 pd.referenceNumber
    Nothing -> 0
