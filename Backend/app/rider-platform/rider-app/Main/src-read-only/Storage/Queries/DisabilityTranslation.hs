{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DisabilityTranslation where

import qualified Domain.Types.DisabilityTranslation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DisabilityTranslation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DisabilityTranslation.DisabilityTranslation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DisabilityTranslation.DisabilityTranslation] -> m ())
createMany = traverse_ create

instance FromTType' Beam.DisabilityTranslation Domain.Types.DisabilityTranslation.DisabilityTranslation where
  fromTType' (Beam.DisabilityTranslationT {..}) = do
    pure $
      Just
        Domain.Types.DisabilityTranslation.DisabilityTranslation
          { disabilityId = Kernel.Types.Id.Id disabilityId,
            disabilityTag = disabilityTag,
            language = language,
            translation = translation
          }

instance ToTType' Beam.DisabilityTranslation Domain.Types.DisabilityTranslation.DisabilityTranslation where
  toTType' (Domain.Types.DisabilityTranslation.DisabilityTranslation {..}) = do
    Beam.DisabilityTranslationT
      { Beam.disabilityId = Kernel.Types.Id.getId disabilityId,
        Beam.disabilityTag = disabilityTag,
        Beam.language = language,
        Beam.translation = translation
      }
