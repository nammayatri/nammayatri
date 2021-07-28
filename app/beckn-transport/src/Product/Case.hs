module Product.Case where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Case as Case
import Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import Storage.Queries.SearchReqLocation as LQ
import Types.API.Case
import Types.Error
import Types.Storage.Case as Case
import Types.Storage.Organization as Organization
import qualified Types.Storage.Person as SP
import Types.Storage.SearchReqLocation as Location
import Utils.Common
import qualified Utils.Defaults as Default

list :: Id SP.Person -> [CaseStatus] -> CaseType -> Maybe Int -> Maybe Int -> FlowHandler CaseListRes
list personId status csType limitM offsetM = withFlowHandlerAPI $ do
  person <- QP.findPersonById personId >>= fromMaybeM PersonNotFound
  now <- getCurrentTime
  case person.organizationId of
    Just orgId -> do
      org <- OQ.findOrganizationById orgId >>= fromMaybeM OrgNotFound
      when (org.status /= Organization.APPROVED) $
        throwError Unauthorized
      caseList <-
        if not (org.enabled)
          then Case.findAllByTypeStatusTime limit offset csType status orgId now $ fromMaybe now (org.fromTime)
          else Case.findAllByTypeStatuses limit offset csType status orgId now
      locList <- LQ.findAllByLocIds (Case.fromLocationId <$> caseList) (Case.toLocationId <$> caseList)
      return $ catMaybes $ joinByIds locList <$> caseList
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM
    joinByIds locList cs =
      find (\x -> Case.fromLocationId cs == Location.id x) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case.toLocationId cs == Location.id x) locList
        prepare pcs from to =
          CaseRes
            { _case = pcs,
              fromLocation = from,
              toLocation = to
            }
