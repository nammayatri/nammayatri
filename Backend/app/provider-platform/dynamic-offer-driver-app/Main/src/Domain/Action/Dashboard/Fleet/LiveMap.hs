{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Fleet.LiveMap (getLiveMapDrivers) where

import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getLiveMapDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes])
getLiveMapDrivers _merchantShortId _opCity = do error "Logic yet to be decided"
