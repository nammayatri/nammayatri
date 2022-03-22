module Types.API.Transporter where

import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Organization as SO
import EulerHS.Prelude hiding (id, state)

newtype TransporterRec = TransporterRec
  { organization :: SO.OrganizationAPIEntity
  }
  deriving (Generic, ToJSON, ToSchema)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateTransporterRes = SO.OrganizationAPIEntity

validateUpdateTransporterReq :: Validate UpdateTransporterReq
validateUpdateTransporterReq UpdateTransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name
    ]
