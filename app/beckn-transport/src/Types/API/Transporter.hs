module Types.API.Transporter where

import Beckn.Types.Common
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
    headCount :: Maybe Int,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON)

validateUpdateTransporterReq :: Validate UpdateTransporterReq
validateUpdateTransporterReq UpdateTransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name
    ]

modifyOrganization :: DBFlow m r => UpdateTransporterReq -> SO.Organization -> m SO.Organization
modifyOrganization req org = do
  now <- getCurrentTime
  return $
    org{SO.name = fromMaybe (org.name) (req.name),
        SO.description = (req.description) <|> (org.description),
        SO.headCount = (req.headCount) <|> (org.headCount),
        SO.enabled = fromMaybe (org.enabled) (req.enabled),
        SO.updatedAt = now
       }
