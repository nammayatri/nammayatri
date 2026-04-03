{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.DriverProfile where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person
import qualified Kernel.Prelude



data TriggerUpdateAuthOTPReq
    = TriggerUpdateAuthOTPReq {identifier :: Domain.Types.Person.IdentifierType, mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, value :: Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data VerifyUpdateAuthOTPReq
    = VerifyUpdateAuthOTPReq {identifier :: Kernel.Prelude.Maybe Domain.Types.Person.IdentifierType, otp :: Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



