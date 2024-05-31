module Storage.Queries.Transformers.QuestionInformation where

import qualified Data.Aeson
import qualified Domain.Types.QuestionInformation
import Kernel.Prelude
import Kernel.Utils.Common (EsqDBFlow, MonadFlow, fromMaybeM)
import Kernel.Utils.JSON (valueToMaybe)
import Tools.Error

convertOptionsToTable :: [Domain.Types.QuestionInformation.OptionEntity] -> Data.Aeson.Value
convertOptionsToTable = Data.Aeson.toJSON

getOptionsFromTable :: (MonadFlow m, EsqDBFlow m r) => Data.Aeson.Value -> m [Domain.Types.QuestionInformation.OptionEntity]
getOptionsFromTable options = valueToMaybe @[Domain.Types.QuestionInformation.OptionEntity] options & fromMaybeM NotAbleToDecodeTheOptionsInLms
