module Product.Feedback where

import qualified App.Types as App
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.API.Feedback as Beckn
import qualified Beckn.Types.Core.Description as Beckn
import qualified Beckn.Types.Core.Rating as Beckn
import Beckn.Types.Id
import Beckn.Utils.Logging
import EulerHS.Prelude hiding (product)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Types.API.Feedback as API
import Types.Error
import qualified Types.Storage.Person as Person
import Utils.Common
  ( buildContext,
    fromMaybeM,
    throwError,
    withFlowHandlerAPI,
  )

feedback :: Id Person.Person -> API.FeedbackReq -> App.FlowHandler API.FeedbackRes
feedback personId request = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let orderPIId = request.productInstanceId
  orderPI <- ProductInstance.findOrderPIById (Id orderPIId) >>= fromMaybeM PIDoesNotExist
  orderCase <- Case.findIdByPersonId personId (orderPI.caseId) >>= fromMaybeM CaseNotFound
  txnId <- getId <$> orderCase.parentCaseId & fromMaybeM (CaseFieldNotPresent "parentCaseId")
  searchPIId <- getId <$> orderPI.parentId & fromMaybeM (PIFieldNotPresent "parentId")
  context <- buildContext "feedback" txnId Nothing Nothing
  organization <-
    Organization.findOrganizationById (orderPI.organizationId)
      >>= fromMaybeM OrgNotFound
  let feedbackMsg =
        Beckn.FeedbackReqMessage
          { order_id = searchPIId,
            rating =
              Beckn.Rating
                { value = show ratingValue,
                  unit = "U+2B50",
                  max_value = Just "5",
                  direction = Just "UP"
                },
            description =
              Beckn.Description
                { name = "Ride order rating",
                  code = "RIDE_ORDER_RATING",
                  symbol = Nothing,
                  short_desc = Nothing,
                  long_desc = Nothing,
                  images = [],
                  audio = Nothing,
                  _3d_render = Nothing
                }
          }
  gatewayUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  ExternalAPI.feedback gatewayUrl (Beckn.FeedbackReq context feedbackMsg)
  return Success
