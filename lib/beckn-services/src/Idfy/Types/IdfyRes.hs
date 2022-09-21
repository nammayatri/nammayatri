{-# LANGUAGE DerivingStrategies #-}

module Idfy.Types.IdfyRes where

import Data.Aeson hiding (Error)
import Data.OpenApi
import EulerHS.Prelude hiding (state)

data IdfyRes = IdfyRes {request_id :: Text, _a :: Maybe Text}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
