{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.ReelsData where

import qualified Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ReelsData
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ReelsData as Beam

convertBottomButtonConfigToTable :: [Domain.Types.ReelsData.ReelRowButtonConfig] -> Data.Aeson.Value
convertBottomButtonConfigToTable = Data.Aeson.toJSON

convertSideButtonConfigToTable :: [Domain.Types.ReelsData.ReelRowButtonConfig] -> Data.Aeson.Value
convertSideButtonConfigToTable = Data.Aeson.toJSON

getBottomButtonConfigFromTable :: Data.Aeson.Value -> [Domain.Types.ReelsData.ReelRowButtonConfig]
getBottomButtonConfigFromTable bottomButtonConfig = fromMaybe [] (valueToMaybe bottomButtonConfig)

getSideButtonConfigFromTable :: Data.Aeson.Value -> [Domain.Types.ReelsData.ReelRowButtonConfig]
getSideButtonConfigFromTable sideButtonConfig = fromMaybe [] (valueToMaybe sideButtonConfig)

valueToMaybe :: Data.Aeson.Value -> Maybe [Domain.Types.ReelsData.ReelRowButtonConfig]
valueToMaybe buttonConfig =
  case Data.Aeson.fromJSON buttonConfig of
    Data.Aeson.Success a -> Just a
    Data.Aeson.Error _ -> Nothing
