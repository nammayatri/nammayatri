module Product.Feedback where

import qualified App.Types as App
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Core.ACL.Rating as ACL
import qualified Domain.Action.UI.Feedback as DFeedback
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (product)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Types.API.Feedback as API
import Utils.Common

feedback :: Id Person.Person -> API.FeedbackReq -> App.FlowHandler API.FeedbackRes
feedback personId request = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dRatingReq <- DFeedback.feedback request
  becknReq <- ACL.buildRatingReq dRatingReq
  void $ ExternalAPI.feedback dRatingReq.providerUrl becknReq
  pure Success
