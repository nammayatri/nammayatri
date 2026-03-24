{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.DeletedPerson where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude



data DeletedPersonReq
    = DeletedPersonReq {reasonToDelete :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



