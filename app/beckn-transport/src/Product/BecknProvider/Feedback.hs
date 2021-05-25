{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.Feedback where

import App.Types
import Beckn.Types.Common
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
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Product.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Rating as Rating
import Types.Error
import Utils.Common

feedback :: Id Organization -> Organization -> API.FeedbackReq -> FlowHandler API.FeedbackRes
feedback _transporterId _organization req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "FeedbackAPI" "Received feedback API call."
    let context = req ^. #context
    BP.validateContext "feedback" context
    let productInstanceId = Id $ req ^. #message . #order_id
    productInstances <- ProductInstance.findAllByParentId productInstanceId
    personId <- getPersonId productInstances & fromMaybeM (PIFieldNotPresent "person")
    orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> productInstances) Case.RIDEORDER
    unless (orderPi ^. #_status == ProductInstance.COMPLETED) $
      throwError $ PIInvalidStatus "Order is not ready for rating."
    ratingValue :: Int <-
      decodeFromText (req ^. #message . #rating . #_value)
        & fromMaybeM (InvalidRequest "Invalid rating type.")
    let orderId = orderPi ^. #_id
    mbRating <- Rating.findByProductInstanceId orderId
    case mbRating of
      Nothing -> do
        logTagInfo "FeedbackAPI" $
          "Creating a new record for " +|| orderId ||+ " with rating " +|| ratingValue ||+ "."
        newRating <- mkRating orderId ratingValue
        Rating.create newRating
      Just rating -> do
        logTagInfo "FeedbackAPI" $
          "Updating existing rating for " +|| orderPi ^. #_id ||+ " with new rating " +|| ratingValue ||+ "."
        Rating.updateRatingValue (rating ^. #_id) ratingValue
    Person.calculateAverageRating personId
    return Ack
  where
    getPersonId (productI : _) = productI ^. #_personId
    getPersonId _ = Nothing

mkRating :: Id ProductInstance.ProductInstance -> Int -> Flow Rating.Rating
mkRating productInstanceId ratingValue = do
  _id <- Id <$> L.generateGUID
  let _productInstanceId = productInstanceId
  now <- getCurrentTime
  let _createdAt = now
  let _updatedAt = now
  let _ratingValue = ratingValue
  pure $ Rating.Rating {..}
