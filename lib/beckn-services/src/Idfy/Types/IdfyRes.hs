{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Idfy.Types.IdfyRes where

import Data.Aeson hiding (Error)
import Data.OpenApi
import EulerHS.Prelude hiding (state)

newtype IdfyRes = IdfyRes {request_id :: Text}
  deriving (Show, Generic)

deriving newtype instance ToJSON IdfyRes

deriving newtype instance FromJSON IdfyRes

deriving newtype instance ToSchema IdfyRes
