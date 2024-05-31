{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FavouriteDriver where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data FavouriteDriverResp = FavouriteDriverResp {driverName :: Data.Text.Text, driverPhone :: Data.Text.Text, driverRating :: Kernel.Prelude.Double, favCount :: Kernel.Prelude.Int, id :: Data.Text.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)
