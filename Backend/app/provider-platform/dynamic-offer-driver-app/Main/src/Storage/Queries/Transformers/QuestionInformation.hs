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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, KvDbFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Kernel.Utils.Error.Throwing (throwError)
import qualified Sequelize as Se
import qualified Storage.Beam.QuestionInformation as Beam
import Tools.Error

convertOptionsToTable :: [Domain.Types.QuestionInformation.OptionEntity] -> Data.Aeson.Value
convertOptionsToTable = Data.Aeson.toJSON

getOptionsFromTable :: KvDbFlow m r => Data.Aeson.Value -> m [Domain.Types.QuestionInformation.OptionEntity]
getOptionsFromTable options = valueToMaybe options >>= fromMaybeM NotAbleToDecodeTheOptionsInLms

valueToMaybe :: (MonadFlow m, FromJSON Domain.Types.QuestionInformation.OptionEntity) => Data.Aeson.Value -> m (Maybe [Domain.Types.QuestionInformation.OptionEntity])
valueToMaybe options = return $ case Data.Aeson.fromJSON options of
  Data.Aeson.Success a -> Just a
  Data.Aeson.Error _ -> Nothing
