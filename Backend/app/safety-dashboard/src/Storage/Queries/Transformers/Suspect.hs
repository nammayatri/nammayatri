module Storage.Queries.Transformers.Suspect where

import qualified Data.Aeson
import qualified Domain.Types.Suspect
import Kernel.Prelude
import Kernel.Utils.Common (MonadFlow, fromMaybeM)
import Kernel.Utils.JSON (valueToMaybe)
import Tools.Error

convertFlaggedByToTable :: [Domain.Types.Suspect.FlaggedBy] -> Data.Aeson.Value
convertFlaggedByToTable = Data.Aeson.toJSON

getFlaggedByFromTable :: MonadFlow m => Data.Aeson.Value -> m [Domain.Types.Suspect.FlaggedBy]
getFlaggedByFromTable flaggedBy = valueToMaybe @[Domain.Types.Suspect.FlaggedBy] flaggedBy & fromMaybeM FlaggedByDecodeError
