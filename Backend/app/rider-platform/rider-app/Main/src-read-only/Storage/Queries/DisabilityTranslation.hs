{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.DisabilityTranslation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.DisabilityTranslation
import qualified Storage.Beam.DisabilityTranslation as Beam
import qualified Kernel.Types.Id



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DisabilityTranslation.DisabilityTranslation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DisabilityTranslation.DisabilityTranslation] -> m ())
createMany = traverse_ create



instance FromTType' Beam.DisabilityTranslation Domain.Types.DisabilityTranslation.DisabilityTranslation
    where fromTType' (Beam.DisabilityTranslationT {..}) = do pure $ Just Domain.Types.DisabilityTranslation.DisabilityTranslation{disabilityId = Kernel.Types.Id.Id disabilityId,
                                                                                                                                  disabilityTag = disabilityTag,
                                                                                                                                  language = language,
                                                                                                                                  translation = translation}
instance ToTType' Beam.DisabilityTranslation Domain.Types.DisabilityTranslation.DisabilityTranslation
    where toTType' (Domain.Types.DisabilityTranslation.DisabilityTranslation {..}) = do Beam.DisabilityTranslationT{Beam.disabilityId = Kernel.Types.Id.getId disabilityId,
                                                                                                                    Beam.disabilityTag = disabilityTag,
                                                                                                                    Beam.language = language,
                                                                                                                    Beam.translation = translation}



