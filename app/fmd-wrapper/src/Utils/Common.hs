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
  let mconfig = org ^. #_info >>= decodeFromText
   in fromMaybeM (InternalError "Client config decode error.") mconfig

fromMaybe400 :: Text -> Maybe ErrorCode -> Maybe a -> Flow a
fromMaybe400 _ _ (Just a) = return a
fromMaybe400 msg errCode Nothing = do
  throwError $
    ErrorCodeWithMessage
      msg
      (fromMaybe CORE001 errCode) -- FIXME
