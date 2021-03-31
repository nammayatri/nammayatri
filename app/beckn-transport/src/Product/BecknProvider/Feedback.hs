{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.Feedback where

import App.Types (Flow, FlowHandler)
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Feedback as API
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.Rating as Rating
  ( Rating,
    RatingT (..),
  )
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Product.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Rating as Rating
import Types.Error

feedback :: Id Organization -> Organization -> API.FeedbackReq -> FlowHandler API.FeedbackRes
feedback _transporterId _organization request = withFlowHandler $ do
  logInfo "FeedbackAPI" "Received feedback API call."
  BP.validateContext "feedback" $ request ^. #context
  let productInstanceId = Id $ request ^. #message . #order_id
  productInstances <- ProductInstance.findAllByParentId $ Just productInstanceId
  personId <- getPersonId productInstances & fromMaybeMWithInfo PIPersonNotPresent "Driver is not assigned."
  orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> productInstances) Case.RIDEORDER
  unless (orderPi ^. #_status == ProductInstance.COMPLETED) $
    throwErrorWithInfo PIInvalidStatus "Order is not ready for rating."
  ratingValue :: Int <-
    decodeFromText (request ^. #message . #rating . #_value)
      & fromMaybeMWithInfo InvalidRequest "Invalid rating type."
  let orderId = orderPi ^. #_id
  mbRating <- Rating.findByProductInstanceId orderId
  case mbRating of
    Nothing -> do
      logInfo "FeedbackAPI" $
        "Creating a new record for " +|| orderId ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- mkRating orderId ratingValue
      Rating.create newRating
    Just rating -> do
      logInfo "FeedbackAPI" $
        "Updating existing rating for " +|| orderPi ^. #_id ||+ " with new rating " +|| ratingValue ||+ "."
      Rating.updateRatingValue (rating ^. #_id) ratingValue
  Person.calculateAverageRating personId
  uuid <- L.generateGUID
  mkAckResponse uuid "feedback"
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
