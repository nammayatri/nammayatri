{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.SearchTry (postSearchTryRecentSearchTries) where

import qualified API.Types.RiderPlatform.Management.SearchTry
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

postSearchTryRecentSearchTries :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.SearchTry.RecentSearchTriesReq -> Environment.Flow API.Types.RiderPlatform.Management.SearchTry.RecentSearchTriesRes)
postSearchTryRecentSearchTries _merchantShortId _opCity req = do error "Logic yet to be decided" req
