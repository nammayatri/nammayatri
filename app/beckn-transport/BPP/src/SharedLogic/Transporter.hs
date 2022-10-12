module SharedLogic.Transporter where

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Organization as DOrg
import Environment
import EulerHS.Prelude hiding (id)
import qualified Storage.CachedQueries.Organization as QOrg
import Tools.Error

findTransporter :: Id DOrg.Organization -> Flow DOrg.Organization
findTransporter transporterId = do
  transporter <- QOrg.findById transporterId >>= fromMaybeM (OrgDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  pure transporter
