{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverGroupInsurance where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.DriverGroupInsurance
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DriverGroupInsuranceRes = DriverGroupInsuranceRes {insurance :: Domain.Types.DriverGroupInsurance.DriverGroupInsurance}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverGroupInsuranceUpsertReq = DriverGroupInsuranceUpsertReq
  { age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dob :: Kernel.Prelude.Maybe Data.Time.Day,
    fullName :: Kernel.Prelude.Text,
    gender :: Kernel.Prelude.Maybe Domain.Types.DriverGroupInsurance.DriverGroupInsuranceGender,
    insuranceType :: Domain.Types.DriverGroupInsurance.DriverGroupInsuranceType,
    mobile :: Kernel.Prelude.Text,
    nomineeDob :: Kernel.Prelude.Maybe Data.Time.Day,
    nomineeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nomineeRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
