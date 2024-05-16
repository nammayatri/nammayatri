{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Suspect where

import qualified Data.Aeson
import qualified Domain.Types.Suspect
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (MonadFlow, fromMaybeM, getCurrentTime)
import Tools.Error

convertFlaggedByToTable :: [Domain.Types.Suspect.FlaggedBy] -> Data.Aeson.Value
convertFlaggedByToTable = Data.Aeson.toJSON

getFlaggedByFromTable :: MonadFlow m => Data.Aeson.Value -> m ([Domain.Types.Suspect.FlaggedBy])
getFlaggedByFromTable flaggedBy = valueToMaybe flaggedBy >>= fromMaybeM FlaggedByDecodeError

valueToMaybe :: (MonadFlow m, FromJSON Domain.Types.Suspect.FlaggedBy) => Data.Aeson.Value -> m (Maybe [Domain.Types.Suspect.FlaggedBy])
valueToMaybe flaggedBy = return $ case Data.Aeson.fromJSON flaggedBy of
  Data.Aeson.Success a -> Just a
  Data.Aeson.Error _ -> Nothing
