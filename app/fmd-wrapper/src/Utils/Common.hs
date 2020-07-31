{-# LANGUAGE OverloadedLabels #-}

module Utils.Common where

import App.Types
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (decodeFromText, fromMaybeM500)
import EulerHS.Prelude

getClientConfig :: FromJSON a => Organization -> Flow a
getClientConfig org =
  let mconfig = org ^. #_info >>= decodeFromText
   in fromMaybeM500 "CLIENT_CONFIG_DECODE_ERROR" mconfig
