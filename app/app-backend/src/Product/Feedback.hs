{-# LANGUAGE OverloadedLabels #-}

module Product.Feedback where

import qualified App.Types as App
import qualified Beckn.Types.API.Feedback as Beckn
import Beckn.Types.App
  ( CaseId (_getCaseId),
    OrganizationId (OrganizationId),
    ProductInstanceId (ProductInstanceId),
  )
import qualified Beckn.Types.Core.Description as Beckn
import qualified Beckn.Types.Core.Rating as Beckn
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (fromMaybeM500, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
  ( Maybe (Just, Nothing),
    Monad ((>>=)),
    ($),
    (&),
    (^.),
  )
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as Case
import qualified Models.ProductInstance as ProductInstance
import qualified Storage.Queries.Organization as Organization
import qualified Types.API.Feedback as API
import Utils.Routes (buildContext)

feedback :: Person.Person -> API.FeedbackReq -> App.FlowHandler API.FeedbackRes
feedback person request = withFlowHandler $ do
  -- TODO validate the rating input?
  let prodInstId = request ^. #productInstanceId
  product <- ProductInstance.findById $ ProductInstanceId prodInstId
  order <- Case.findIdByPerson person $ product ^. #_caseId
  messageId <- L.generateGUID
  let txnId = _getCaseId $ order ^. #_id
  context <- buildContext "feedback" txnId messageId
  organization <-
    Organization.findOrganizationById (OrganizationId $ product ^. #_organizationId)
      >>= fromMaybeM500 "INVALID_PROVIDER_ID"
  let feedbackMsg =
        Beckn.FeedbackReqMessage
          { order_id = prodInstId,
            rating =
              Beckn.Rating
                { _value = request ^. #rating,
                  _unit = "",
                  _max_value = Nothing, -- TODO should make it configurable?
                  _direction = Just "UP"
                },
            description =
              Beckn.Description
                { _name = "Ride order rating",
                  _code = "RIDE_ORDER_RATING",
                  _symbol = Nothing,
                  _short_desc = Nothing,
                  _long_desc = Nothing,
                  _images = [],
                  _audio = Nothing,
                  _3d_render = Nothing
                }
          }
  gatewayUrl <- organization ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  Gateway.feedback gatewayUrl organization $ Beckn.FeedbackReq context feedbackMsg
