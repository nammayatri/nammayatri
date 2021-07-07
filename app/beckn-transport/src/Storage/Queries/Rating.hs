module Storage.Queries.Rating where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam (SqlEq ((==.)), (<-.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.ProductInstance as PI
import qualified Types.Storage.DB as DB
import Types.Storage.Person (Person)
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Rating as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RatingT))
getDbTable =
  DB.rating . DB.transporterDb <$> getSchemaName

create :: DBFlow m r => Storage.Rating -> m ()
create rating = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression rating)

updateRatingValue :: DBFlow m r => Id Storage.Rating -> Int -> m ()
updateRatingValue ratingId newRatingValue = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update
    dbTable
    ( \Storage.Rating {..} ->
        mconcat
          [ ratingValue <-. B.val_ newRatingValue,
            updatedAt <-. B.val_ now
          ]
    )
    (\Storage.Rating {..} -> id ==. B.val_ ratingId)

findByProductInstanceId :: DBFlow m r => Id PI.ProductInstance -> m (Maybe Storage.Rating)
findByProductInstanceId productInsId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Rating {..} = productInstanceId ==. B.val_ productInsId

findAllRatingsForPerson :: DBFlow m r => Id Person -> m [Storage.Rating]
findAllRatingsForPerson personId_ = do
  ratingTable <- getDbTable
  productInstanceTable <- PI.getDbTable
  DB.findAllByJoin
    (B.orderBy_ orderBy)
    (joinPredicate ratingTable productInstanceTable)
  where
    orderBy Storage.Rating {..} = B.desc_ createdAt
    joinPredicate ratingTable productInstanceTable = do
      productInstance <-
        B.filter_
          (\PI.ProductInstance {..} -> personId ==. B.val_ (Just personId_))
          $ B.all_ productInstanceTable
      rating <- B.all_ ratingTable
      B.guard_ $ PI.ProductInstancePrimaryKey (Storage.productInstanceId rating) `B.references_` productInstance
      pure rating
