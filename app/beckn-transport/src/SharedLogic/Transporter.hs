module SharedLogic.Transporter where

import App.Types
import Beckn.Types.Id
import qualified Domain.Types.Organization as DOrg
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import Types.Error
import Utils.Common

findTransporter :: Id DOrg.Organization -> Flow DOrg.Organization
findTransporter transporterId = do
  transporter <- QOrg.findById transporterId >>= fromMaybeM (OrgDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  pure transporter
