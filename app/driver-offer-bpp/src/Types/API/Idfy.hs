module Types.API.Idfy where

import Data.Text
import Data.OpenApi (ToSchema (..))
import EulerHS.Prelude 
import Domain.Types.Driveronboarding.VehicleRegistrationCert (IdfyStatus)

data IdfyDLReq = IdfyDLReq 
  { request_id :: Text,
    status :: IdfyStatus,
    result :: DLVerificationResult
   }
   deriving ( Generic, Eq, Show, ToJSON, FromJSON, ToSchema )

newtype DLVerificationResult = DLVerificationResult { source_output :: SourceOutput } 
  deriving ( Generic, Eq, Show, ToJSON, FromJSON, ToSchema )

data SourceOutput = SourceOutput
  { name :: Text,
    field :: [CovDetails],
    nt_validity_from :: Maybe Text,
    nt_validity_to :: Maybe Text
  }
  deriving ( Generic, Eq, Show, ToJSON, FromJSON, ToSchema )

newtype CovDetails = CovDetails { cov :: Text }
 deriving ( Generic, Eq, Show, ToJSON, FromJSON, ToSchema )

data  IdfyRCReq = IdfyRCReq
  { status :: IdfyStatus,
    request_id :: Text,
    result :: RCVerificationResult
  }
  deriving ( Generic, Eq, Show, ToJSON, FromJSON, ToSchema )

newtype RCVerificationResult = RCVerificationResult { extraction_output :: ExtractionOutput }
  deriving ( Generic, Eq, Show, ToJSON, FromJSON, ToSchema )

data ExtractionOutput = ExtractionOutput
  { registration_date :: Maybe Text,
    fitness_upto :: Maybe Text,
    insurance_validity :: Maybe Text,
    permit_validity :: Maybe Text,
    vehicle_class :: Maybe Text,
    registration_number :: Maybe Text
  }
  deriving ( Generic, Eq, Show, ToJSON, FromJSON, ToSchema )

