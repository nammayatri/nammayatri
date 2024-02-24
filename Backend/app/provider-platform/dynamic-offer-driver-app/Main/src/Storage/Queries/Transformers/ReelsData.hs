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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ReelsData as Beam

convertBottomButtonConfigToTable :: [Domain.Types.ReelsData.ReelRowButtonConfig] -> Data.Aeson.Value
convertBottomButtonConfigToTable = error "TODO"

convertSideButtonConfigToTable :: [Domain.Types.ReelsData.ReelRowButtonConfig] -> Data.Aeson.Value
convertSideButtonConfigToTable = error "TODO"

getBottomButtonConfigFromTable :: Data.Aeson.Value -> [Domain.Types.ReelsData.ReelRowButtonConfig]
getBottomButtonConfigFromTable _bottomButtonConfig = error "TODO"

getSideButtonConfigFromTable :: Data.Aeson.Value -> [Domain.Types.ReelsData.ReelRowButtonConfig]
getSideButtonConfigFromTable _sideButtonConfig = error "TODO"
