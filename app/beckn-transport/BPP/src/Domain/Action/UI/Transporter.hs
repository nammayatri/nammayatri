module Domain.Action.UI.Transporter
  ( TransporterRec (..),
    UpdateTransporterReq (..),
    UpdateTransporterRes,
    updateTransporter,
    getTransporter,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Common
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Organization as SO
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Storage.CachedQueries.Organization as QO
import qualified Storage.Queries.Person as QP
import Tools.Error

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

updateTransporter :: (HedisFlow m r, EsqDBFlow m r) => SP.Person -> Id SO.Organization -> UpdateTransporterReq -> m UpdateTransporterRes
updateTransporter admin orgId req = do
  unless (Just orgId == admin.organizationId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  org <-
    QO.findById orgId
      >>= fromMaybeM (OrgDoesNotExist orgId.getId)
  let updOrg =
        org{SO.name = fromMaybe (org.name) (req.name),
            SO.description = (req.description) <|> (org.description),
            SO.enabled = fromMaybe (org.enabled) (req.enabled)
           }
  Esq.runTransaction $ QO.update updOrg
  QO.clearCache updOrg
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateTransporter : ") (show updOrg)
  return $ SO.makeOrganizationAPIEntity updOrg

getTransporter :: (HedisFlow m r, EsqDBFlow m r) => Id SP.Person -> m TransporterRec
getTransporter personId = do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  case person.organizationId of
    Just orgId -> TransporterRec . SO.makeOrganizationAPIEntity <$> (QO.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId))
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
