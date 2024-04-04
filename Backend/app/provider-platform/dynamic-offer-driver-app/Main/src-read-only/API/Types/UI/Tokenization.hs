{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Tokenization where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data GetTokenRes = GetTokenRes {token :: Kernel.Prelude.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)
