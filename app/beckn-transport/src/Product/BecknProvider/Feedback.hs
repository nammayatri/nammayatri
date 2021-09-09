module Product.BecknProvider.Feedback where

import App.Types
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Feedback as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Rating as Rating
import Types.Error
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as SP
import qualified Types.Storage.ProductInstance as ProductInstance
import Types.Storage.Rating as Rating
  ( Rating,
    RatingT (..),
  )
import Utils.Common

feedback ::
  Id Organization ->
  SignatureAuthResult Organization ->
  API.FeedbackReq ->
  FlowHandler API.FeedbackRes
feedback _ _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "FeedbackAPI" "Received feedback API call."
    let context = req.context
    BP.validateContext "feedback" context
    let searchPIId = Id $ req.message.order_id
    orderPI <- ProductInstance.findOrderPIByParentId searchPIId >>= fromMaybeM PIDoesNotExist
    driverId <- orderPI.personId & fromMaybeM (PIFieldNotPresent "person")
    unless (orderPI.status == ProductInstance.COMPLETED) $
      throwError $ PIInvalidStatus "Order is not ready for rating."
    ratingValue :: Int <-
      decodeFromText (req.message.rating.value)
        & fromMaybeM (InvalidRequest "Invalid rating type.")
    let orderId = orderPI.id
    mbRating <- Rating.findByProductInstanceId orderId
    case mbRating of
      Nothing -> do
        logTagInfo "FeedbackAPI" $
          "Creating a new record for " +|| orderId ||+ " with rating " +|| ratingValue ||+ "."
        newRating <- mkRating orderId driverId ratingValue
        Rating.create newRating
      Just rating -> do
        logTagInfo "FeedbackAPI" $
          "Updating existing rating for " +|| orderPI.id ||+ " with new rating " +|| ratingValue ||+ "."
        Rating.updateRatingValue rating.id driverId ratingValue
    calculateAverageRating driverId
    return Ack

calculateAverageRating ::
  (DBFlow m r, EncFlow m r, HasFlowEnv m r '["minimumDriverRatesCount" ::: Int]) =>
  Id SP.Person ->
  m ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratingsSum :: Double = fromIntegral $ sum (allRatings <&> (.ratingValue))
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  minimumDriverRatesCount <- asks (.minimumDriverRatesCount)
  when (ratingCount >= minimumDriverRatesCount) $ do
    let newAverage = ratingsSum / fromIntegral ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
    QP.updateAverageRating personId newAverage

mkRating :: MonadFlow m => Id ProductInstance.ProductInstance -> Id SP.Person -> Int -> m Rating.Rating
mkRating productInstanceId driverId ratingValue = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ Rating.Rating {..}
