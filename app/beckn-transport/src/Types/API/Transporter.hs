module Types.API.Transporter where

import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.Organization as SO

newtype TransporterRec = TransporterRec
  { organization :: SO.Organization
  }
  deriving (Generic, ToJSON)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON)

type UpdateTransporterRes = SO.OrganizationAPIEntity

validateUpdateTransporterReq :: Validate UpdateTransporterReq
validateUpdateTransporterReq UpdateTransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name
    ]