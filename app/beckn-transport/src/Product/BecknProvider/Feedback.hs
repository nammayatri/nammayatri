{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.BecknProvider.Feedback where

import App.Types (Flow)
import qualified Beckn.Types.API.Feedback as API
import Beckn.Types.App
  ( PersonId,
    ProductInstanceId (ProductInstanceId),
    RatingId (RatingId),
  )
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
  )
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Rating as Rating

feedback :: Organization -> API.FeedbackReq -> Flow API.FeedbackRes
feedback _organization request = do
  L.logInfo @Text "FeedbackAPI" "Received feedback API call."
  BP.validateContext "feedback" $ request ^. #context
  let productInstanceId = ProductInstanceId $ request ^. #message . #order_id
  productInstances <- ProductInstance.findAllByParentId $ Just productInstanceId
  orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> productInstances) Case.RIDEORDER
  unless (orderPi ^. #_status == ProductInstance.COMPLETED) $
    throwBecknError401 "ORDER_NOT_READY_FOR_RATING"
  personId <- orderPi ^. #_personId & fromMaybeM500 "NO_DRIVER_FOR_RATE"
  ratingValue :: Int <-
    decodeFromText (request ^. #message . #rating . #_value)
      & fromMaybeM400 "INVALID_RATING_TYPE"
  mbRating <- Rating.findByProductInstanceId productInstanceId
  case mbRating of
    Nothing -> do
      L.logInfo @Text "FeedbackAPI" $
        "Creating a new record for " +|| productInstanceId ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- mkRating productInstanceId personId ratingValue
      Rating.create newRating
    Just rating -> do
      L.logInfo @Text "FeedbackAPI" $
        "Updating existing rating for " +|| productInstanceId ||+ " with new rating " +|| ratingValue ||+ "."
      Rating.updateRatingValue (rating ^. #_id) ratingValue
  uuid <- L.generateGUID
  mkAckResponse uuid "feedback"

mkRating :: ProductInstanceId -> PersonId -> Int -> Flow Rating.Rating
mkRating productInstanceId personId ratingValue = do
  _id <- RatingId <$> L.generateGUID
  let _productInstanceId = productInstanceId
  let _personId = personId
  now <- getCurrTime
  let _createdAt = now
  let _updatedAt = now
  let _ratingValue = ratingValue
  pure $ Rating.Rating {..}
