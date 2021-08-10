module Storage.Queries.Rating where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam (SqlEq ((==.)), (&&.), (<-.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import Types.Storage.Person (Person)
import qualified Types.Storage.Person as SP
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Rating as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RatingT))
getDbTable =
  DB.rating . DB.transporterDb <$> getSchemaName

create :: DBFlow m r => Storage.Rating -> m ()
create rating = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue rating)

updateRatingValue :: DBFlow m r => Id Storage.Rating -> Id SP.Person -> Int -> m ()
updateRatingValue ratingId driverId' newRatingValue = do
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
    ( \Storage.Rating {..} ->
        id ==. B.val_ ratingId
          &&. driverId ==. B.val_ driverId'
    )

findByProductInstanceId :: DBFlow m r => Id PI.ProductInstance -> m (Maybe Storage.Rating)
findByProductInstanceId productInsId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Rating {..} = productInstanceId ==. B.val_ productInsId

findAllRatingsForPerson :: DBFlow m r => Id Person -> m [Storage.Rating]
findAllRatingsForPerson driverId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Rating {..} = driverId ==. B.val_ driverId_
