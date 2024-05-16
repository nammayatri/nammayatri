{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CustomerReferral where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data ReferredCustomers = ReferredCustomers {count :: Kernel.Prelude.Int} deriving (Generic, ToJSON, FromJSON, ToSchema)
