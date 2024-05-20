{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Overlay where

import qualified Data.Aeson
import qualified Data.Aeson as A
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

valueToMaybe :: FromJSON a => A.Value -> Maybe a
valueToMaybe value = case A.fromJSON value of
  A.Success a -> Just a
  A.Error _ -> Nothing
