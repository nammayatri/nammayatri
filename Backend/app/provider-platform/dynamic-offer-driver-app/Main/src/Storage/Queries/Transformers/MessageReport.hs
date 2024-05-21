{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.MessageReport where

import qualified Data.Aeson
import qualified Domain.Types.Extra.MessageReport
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

getMessageDynamicFields :: (Data.Aeson.Value -> Domain.Types.Extra.MessageReport.MessageDynamicFieldsType)
getMessageDynamicFields _messageDynamicFields = error "TODO"
