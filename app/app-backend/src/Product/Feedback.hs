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
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Types.API.Feedback as API
import Types.Error
import qualified Types.Storage.Person as Person
import Utils.Common
  ( buildMobilityContext,
    fromMaybeM,
    throwError,
    withFlowHandlerAPI,
  )

feedback :: Id Person.Person -> API.FeedbackReq -> App.FlowHandler API.FeedbackRes
feedback personId request = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM RideBookingNotFound
  let txnId = getId rideBooking.requestId
  let quoteId = getId rideBooking.quoteId
  context <- buildMobilityContext "feedback" txnId Nothing Nothing
  organization <-
    Organization.findOrganizationById (rideBooking.providerId)
      >>= fromMaybeM OrgNotFound
  let feedbackMsg =
        Beckn.FeedbackReqMessage
          { order_id = quoteId,
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
