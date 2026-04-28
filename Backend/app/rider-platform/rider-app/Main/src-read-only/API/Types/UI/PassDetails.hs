{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PassDetails where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.PassDetails
import qualified Domain.Types.PassOrganization
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data GetOrganizationResp = GetOrganizationResp {address :: Kernel.Prelude.Maybe Kernel.Prelude.Text, id :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsDataResp = PassDetailsDataResp
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    passDetailsId :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    passOrganizationName :: Kernel.Prelude.Text,
    registerNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routePairs :: [Domain.Types.PassDetails.RoutePair],
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsUpdateReq = PassDetailsUpdateReq
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    destStopId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    destStopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    registerNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    srcStopId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    srcStopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassStatusResp = PassStatusResp {remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text, verificationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime, verificationStatus :: Domain.Types.PassDetails.VerificationStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
