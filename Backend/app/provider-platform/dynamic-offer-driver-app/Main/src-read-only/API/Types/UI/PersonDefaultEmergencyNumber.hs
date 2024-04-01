{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PersonDefaultEmergencyNumber where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DefaultEmergencyNumbersEntity = DefaultEmergencyNumbersEntity {defaultEmergencyNumbers :: [API.Types.UI.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumber {mobileCountryCode :: Kernel.Prelude.Text, mobileNumber :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)
