module Product.BecknProvider.Feedback where

import App.Types
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Feedback as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.Rating as Rating
  ( Rating,
    RatingT (..),
  )
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import qualified Product.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Rating as Rating
import Types.Error
import Utils.Common

feedback :: Id Organization -> Organization -> API.FeedbackReq -> FlowHandler API.FeedbackRes
feedback _ _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "FeedbackAPI" "Received feedback API call."
    let context = req.context
    BP.validateContext "feedback" context
    let productInstanceId = Id $ req.message.order_id
    productInstances <- ProductInstance.findAllByParentId productInstanceId
    personId <- getPersonId productInstances & fromMaybeM (PIFieldNotPresent "person")
    orderPi <-
      ProductInstance.findByIdType (ProductInstance.id <$> productInstances) Case.RIDEORDER
        >>= fromMaybeM PINotFound
    unless (orderPi.status == ProductInstance.COMPLETED) $
      throwError $ PIInvalidStatus "Order is not ready for rating."
    ratingValue :: Int <-
      decodeFromText (req.message.rating.value)
        & fromMaybeM (InvalidRequest "Invalid rating type.")
    let orderId = orderPi.id
    mbRating <- Rating.findByProductInstanceId orderId
    case mbRating of
      Nothing -> do
        logTagInfo "FeedbackAPI" $
          "Creating a new record for " +|| orderId ||+ " with rating " +|| ratingValue ||+ "."
        newRating <- mkRating orderId ratingValue
        Rating.create newRating
      Just rating -> do
        logTagInfo "FeedbackAPI" $
          "Updating existing rating for " +|| orderPi.id ||+ " with new rating " +|| ratingValue ||+ "."
        Rating.updateRatingValue (rating.id) ratingValue
    Person.calculateAverageRating personId
    return Ack
  where
    getPersonId (productI : _) = productI.personId
    getPersonId _ = Nothing

mkRating :: MonadFlow m => Id ProductInstance.ProductInstance -> Int -> m Rating.Rating
mkRating productInstanceId ratingValue = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ Rating.Rating {..}
