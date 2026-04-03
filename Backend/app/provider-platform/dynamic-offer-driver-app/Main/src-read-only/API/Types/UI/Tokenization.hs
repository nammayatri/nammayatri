{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.Tokenization where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude



data GetTokenRes
    = GetTokenRes {expiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, token :: Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



