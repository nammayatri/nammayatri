module Storage.Queries.Rating where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Types.Storage.Person (Person)
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.Rating as Storage
import Database.Beam (SqlEq ((==.)), (<-.))
import qualified Database.Beam as B
import EulerHS.Prelude
import qualified Storage.Queries.ProductInstance as PI
import qualified Types.Storage.DB as DB
import Utils.Common

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RatingT))
getDbTable =
  DB._rating . DB.transporterDb <$> getSchemaName

create :: Storage.Rating -> Flow ()
create rating = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression rating)
    >>= checkDBError

updateRatingValue :: Id Storage.Rating -> Int -> Flow ()
updateRatingValue ratingId newRatingValue = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update
    dbTable
    ( \Storage.Rating {..} ->
        mconcat
          [ _ratingValue <-. B.val_ newRatingValue,
            _updatedAt <-. B.val_ now
          ]
    )
    (\Storage.Rating {..} -> _id ==. B.val_ ratingId)
    >>= checkDBError

findByProductInstanceId :: Id PI.ProductInstance -> Flow (Maybe Storage.Rating)
findByProductInstanceId productInsId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Storage.Rating {..} = _productInstanceId ==. B.val_ productInsId

findAllRatingsForPerson :: Id Person -> Flow [Storage.Rating]
findAllRatingsForPerson personId = do
  ratingTable <- getDbTable
  productInstanceTable <- PI.getDbTable
  DB.findAllByJoinWithoutLimits
    orderBy
    (joinPredicate ratingTable productInstanceTable)
    >>= checkDBError
  where
    orderBy Storage.Rating {..} = B.desc_ _createdAt
    joinPredicate ratingTable productInstanceTable = do
      productInstance <-
        B.filter_
          (\PI.ProductInstance {..} -> _personId ==. B.val_ (Just personId))
          $ B.all_ productInstanceTable
      rating <- B.all_ ratingTable
      B.guard_ $ PI.ProductInstancePrimaryKey (Storage._productInstanceId rating) `B.references_` productInstance
      pure rating
