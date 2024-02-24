{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.QuestionInformation where

import qualified Data.Aeson
import qualified Domain.Types.LmsEnumTypes
import qualified Domain.Types.QuestionInformation
import qualified Domain.Types.QuestionModuleMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.QuestionInformation as Beam

convertOptionsToTable :: [Domain.Types.QuestionInformation.OptionEntity] -> Data.Aeson.Value
convertOptionsToTable = error "TODO"

getOptionsFromTable :: MonadFlow m => Data.Aeson.Value -> m ([Domain.Types.QuestionInformation.OptionEntity])
getOptionsFromTable _options = error "TODO"
