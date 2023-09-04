{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Disability where

import Domain.Action.UI.Profile
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.DisabilityTranslation as BeamDT

findAllByLanguage :: MonadFlow m => Language -> m [DisabilityItem]
findAllByLanguage language = do
  let langString = show language
  findAllWithDb [Se.Is BeamDT.language $ Se.Eq langString]

instance FromTType' BeamDT.DisabilityTranslation DisabilityItem where
  fromTType' BeamDT.DisabilityTranslationT {..} = do
    pure $
      Just
        DisabilityItem
          { id = Id disabilityId,
            tag = disabilityTag,
            description = translation
          }
