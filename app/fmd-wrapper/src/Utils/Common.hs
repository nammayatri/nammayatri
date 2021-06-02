{-# LANGUAGE OverloadedLabels #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common as CoreCommon
import EulerHS.Prelude
import Types.Error

getClientConfig :: FromJSON a => Organization -> Flow a
getClientConfig org =
  let mconfig = org ^. #info >>= decodeFromText
   in fromMaybeM (InternalError "Client config decode error.") mconfig

fromMaybeErr :: Text -> Maybe ErrorCode -> Maybe a -> Flow a
fromMaybeErr msg errCode =
  fromMaybeM $ ErrorCodeWithMessage msg (fromMaybe CORE001 errCode)
