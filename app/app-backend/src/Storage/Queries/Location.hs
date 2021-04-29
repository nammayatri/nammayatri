module Storage.Queries.Location where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Location as Storage
import Database.Beam ((&&.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import Utils.Common

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.LocationT))
getDbTable =
  DB._location . DB.appDb <$> getSchemaName

createFlow :: Storage.Location -> Flow ()
createFlow = do
  DB.runSqlDB . create
    >=> checkDBError

create :: Storage.Location -> DB.SqlDB ()
create Storage.Location {..} = do
  dbTable <- getDbTable
  void $ DB.createOne' dbTable (Storage.insertExpression Storage.Location {..})

findLocationById ::
  Id Storage.Location -> Flow (Maybe Storage.Location)
findLocationById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Storage.Location {..} = _id ==. B.val_ id

findAllWithLimitOffsetWhere :: [Text] -> [Text] -> [Text] -> [Text] -> [Text] -> Maybe Int -> Maybe Int -> Flow [Storage.Location]
findAllWithLimitOffsetWhere pins cities states districts wards mlimit moffset = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere
    dbTable
    predicate
    limit
    offset
    orderByDesc
    >>= checkDBError
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Location {..} = B.desc_ _createdAt
    predicate Storage.Location {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _pincode `B.in_` (B.val_ . Just <$> pins) ||. complementVal pins,
          _city `B.in_` (B.val_ . Just <$> cities) ||. complementVal cities,
          _state `B.in_` (B.val_ . Just <$> states) ||. complementVal states,
          _district `B.in_` (B.val_ . Just <$> districts) ||. complementVal districts,
          _ward `B.in_` (B.val_ . Just <$> wards) ||. complementVal wards
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findAllByIds :: [Text] -> Flow [Storage.Location]
findAllByIds locIds = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable (predicate (Id <$> locIds))
  where
    predicate locationIds Storage.Location {..} =
      B.in_ _id (B.val_ <$> locationIds)
