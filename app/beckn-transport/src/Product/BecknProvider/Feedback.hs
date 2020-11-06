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
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.Rating as Rating
  ( Rating,
    RatingT (..),
  )
import Beckn.Utils.Common
  ( fromMaybeM500,
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
  BP.validateContext "search" $ request ^. #context
  let productInstanceId = ProductInstanceId $ request ^. #message . #order_id
  productInstance <- ProductInstance.findById productInstanceId
  unless (productInstance ^. #_status == ProductInstance.COMPLETED) $
    throwBecknError401 "ORDER_NOT_READY_FOR_RATING"
  personId <- productInstance ^. #_personId & fromMaybeM500 "NO_DRIVER_FOR_RATE"
  let ratingValue = request ^. #message . #rating . #_value
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

mkRating :: ProductInstanceId -> PersonId -> Text -> Flow Rating.Rating
mkRating productInstanceId personId ratingValue = do
  _id <- RatingId <$> L.generateGUID
  let _productInstanceId = productInstanceId
  let _personId = personId
  now <- getCurrTime
  let _createdAt = now
  let _updatedAt = now
  let _ratingValue = ratingValue
  pure $ Rating.Rating {..}
