{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.Feedback where

import App.Types (Flow, FlowHandler, Log (..))
import Beckn.Types.App
  ( RatingId (RatingId),
  )
import qualified Beckn.Types.Core.API.Feedback as API
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.Rating as Rating
  ( Rating,
    RatingT (..),
  )
import Beckn.Utils.Common
  ( decodeFromText,
    fromMaybeM400,
    fromMaybeM500,
    getCurrTime,
    mkAckResponse,
    throwBecknError401,
    withFlowHandler,
  )
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Product.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Rating as Rating

feedback :: ID Organization -> Organization -> API.FeedbackReq -> FlowHandler API.FeedbackRes
feedback _transporterId _organization request = withFlowHandler $ do
  logInfo "FeedbackAPI" "Received feedback API call."
  BP.validateContext "feedback" $ request ^. #context
  let productInstanceId = ID $ request ^. #message . #order_id
  productInstances <- ProductInstance.findAllByParentId $ Just productInstanceId
  personId <- getPersonId productInstances & fromMaybeM500 "NO_DRIVER_ASSIGNED_FOR_ORDER"
  orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> productInstances) Case.RIDEORDER
  unless (orderPi ^. #_status == ProductInstance.COMPLETED) $
    throwBecknError401 "ORDER_NOT_READY_FOR_RATING"
  ratingValue :: Int <-
    decodeFromText (request ^. #message . #rating . #_value)
      & fromMaybeM400 "INVALID_RATING_TYPE"
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

mkRating :: ID ProductInstance.ProductInstance -> Int -> Flow Rating.Rating
mkRating productInstanceId ratingValue = do
  _id <- RatingId <$> L.generateGUID
  let _productInstanceId = productInstanceId
  now <- getCurrTime
  let _createdAt = now
  let _updatedAt = now
  let _ratingValue = ratingValue
  pure $ Rating.Rating {..}
