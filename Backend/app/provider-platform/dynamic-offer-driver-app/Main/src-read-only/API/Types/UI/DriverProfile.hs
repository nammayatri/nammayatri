{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverProfile where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Servant
import Tools.Auth

data TriggerUpdateAuthOTPReq = TriggerUpdateAuthOTPReq {identifier :: Domain.Types.Person.IdentifierType, mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, value :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateOperatingCityReq = UpdateOperatingCityReq {lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateOperatingCityRes = UpdateOperatingCityRes {city :: Kernel.Types.Beckn.Context.City, isCityChanged :: Kernel.Prelude.Bool, state :: Kernel.Types.Beckn.Context.IndianState}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyUpdateAuthOTPReq = VerifyUpdateAuthOTPReq {identifier :: Kernel.Prelude.Maybe Domain.Types.Person.IdentifierType, otp :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
