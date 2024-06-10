{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.KioskLocationTranslation where

import qualified Domain.Types.KioskLocation
import qualified Domain.Types.KioskLocationTranslation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.KioskLocationTranslation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KioskLocationTranslation.KioskLocationTranslation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.KioskLocationTranslation.KioskLocationTranslation] -> m ())
createMany = traverse_ create

findByKioskLocationIdAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.KioskLocation.KioskLocation -> Kernel.External.Types.Language -> m (Maybe Domain.Types.KioskLocationTranslation.KioskLocationTranslation))
findByKioskLocationIdAndLanguage kioskLocationId language = do findOneWithKV [Se.And [Se.Is Beam.kioskLocationId $ Se.Eq (Kernel.Types.Id.getId kioskLocationId), Se.Is Beam.language $ Se.Eq language]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.KioskLocation.KioskLocation -> m (Maybe Domain.Types.KioskLocationTranslation.KioskLocationTranslation))
findByPrimaryKey kioskLocationId = do findOneWithKV [Se.And [Se.Is Beam.kioskLocationId $ Se.Eq (Kernel.Types.Id.getId kioskLocationId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KioskLocationTranslation.KioskLocationTranslation -> m ())
updateByPrimaryKey (Domain.Types.KioskLocationTranslation.KioskLocationTranslation {..}) = do
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.landmark landmark,
      Se.Set Beam.language language
    ]
    [Se.And [Se.Is Beam.kioskLocationId $ Se.Eq (Kernel.Types.Id.getId kioskLocationId)]]

instance FromTType' Beam.KioskLocationTranslation Domain.Types.KioskLocationTranslation.KioskLocationTranslation where
  fromTType' (Beam.KioskLocationTranslationT {..}) = do
    pure $
      Just
        Domain.Types.KioskLocationTranslation.KioskLocationTranslation
          { address = address,
            kioskLocationId = Kernel.Types.Id.Id kioskLocationId,
            landmark = landmark,
            language = language
          }

instance ToTType' Beam.KioskLocationTranslation Domain.Types.KioskLocationTranslation.KioskLocationTranslation where
  toTType' (Domain.Types.KioskLocationTranslation.KioskLocationTranslation {..}) = do
    Beam.KioskLocationTranslationT
      { Beam.address = address,
        Beam.kioskLocationId = Kernel.Types.Id.getId kioskLocationId,
        Beam.landmark = landmark,
        Beam.language = language
      }
