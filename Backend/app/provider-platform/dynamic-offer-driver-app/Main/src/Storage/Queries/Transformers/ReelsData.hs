module Storage.Queries.Transformers.ReelsData where

import qualified Data.Aeson
import qualified Domain.Types.ReelsData
import Kernel.Prelude
import Kernel.Utils.JSON (valueToMaybe)

convertBottomButtonConfigToTable :: [Domain.Types.ReelsData.ReelRowButtonConfig] -> Data.Aeson.Value
convertBottomButtonConfigToTable = Data.Aeson.toJSON

convertSideButtonConfigToTable :: [Domain.Types.ReelsData.ReelRowButtonConfig] -> Data.Aeson.Value
convertSideButtonConfigToTable = Data.Aeson.toJSON

getBottomButtonConfigFromTable :: Data.Aeson.Value -> [Domain.Types.ReelsData.ReelRowButtonConfig]
getBottomButtonConfigFromTable bottomButtonConfig = fold (valueToMaybe @[Domain.Types.ReelsData.ReelRowButtonConfig] bottomButtonConfig)

getSideButtonConfigFromTable :: Data.Aeson.Value -> [Domain.Types.ReelsData.ReelRowButtonConfig]
getSideButtonConfigFromTable sideButtonConfig = fold (valueToMaybe @[Domain.Types.ReelsData.ReelRowButtonConfig] sideButtonConfig)
