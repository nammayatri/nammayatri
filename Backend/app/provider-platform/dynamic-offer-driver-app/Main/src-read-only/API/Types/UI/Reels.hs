{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.Reels where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Domain.Types.ReelsData



data ReelsResp
    = ReelsResp {reels :: [Domain.Types.ReelsData.ReelsData]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



