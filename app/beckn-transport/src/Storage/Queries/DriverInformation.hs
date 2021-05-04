{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.DriverInformation where

import App.Types
import Beckn.External.Encryption
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import Data.Bitraversable
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QPerson
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverInformation as DriverInformation

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity DriverInformation.DriverInformationT))
getDbTable = DB._driverInformation . DB.transporterDb <$> getSchemaName

create :: DriverInformation.DriverInformation -> Flow ()
create DriverInformation.DriverInformation {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression DriverInformation.DriverInformation {..})

findById :: Id Driver -> Flow (Maybe DriverInformation.DriverInformation)
findById driverId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    personId = cast driverId
    predicate DriverInformation.DriverInformation {..} = _driverId ==. B.val_ personId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

fetchAllAvailableByIds :: [Id Driver] -> Flow [DriverInformation.DriverInformation]
fetchAllAvailableByIds driversIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    personsIds = cast <$> driversIds
    predicate DriverInformation.DriverInformation {..} =
      foldr
        (&&.)
        (B.val_ True)
        [ _driverId `B.in_` (B.val_ <$> personsIds),
          _active ==. B.val_ True,
          _onRide ==. B.val_ False
        ]

updateActivity :: Id Driver -> Bool -> Flow ()
updateActivity driverId active = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause active now) (predicate personId)
  where
    personId = cast driverId
    setClause a now DriverInformation.DriverInformation {..} =
      mconcat
        [ _active <-. B.val_ a,
          _updatedAt <-. B.val_ now
        ]
    predicate id DriverInformation.DriverInformation {..} = _driverId ==. B.val_ id

updateOnRideFlow :: Id Driver -> Bool -> Flow ()
updateOnRideFlow driverId onRide =
  DB.runSqlDB (updateOnRide driverId onRide)

updateOnRide ::
  Id Driver ->
  Bool ->
  DB.SqlDB ()
updateOnRide driverId onRide = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update' dbTable (setClause onRide now) (predicate personId)
  where
    personId = cast driverId
    setClause onR now' DriverInformation.DriverInformation {..} =
      mconcat
        [ _onRide <-. B.val_ onR,
          _updatedAt <-. B.val_ now'
        ]
    predicate id DriverInformation.DriverInformation {..} = _driverId ==. B.val_ id

deleteById :: Id Driver -> Flow ()
deleteById driverId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate personId)
  where
    personId = cast driverId
    predicate pid DriverInformation.DriverInformation {..} = _driverId ==. B.val_ pid

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Text] -> Flow [(Person.Person, DriverInformation.DriverInformation)]
findAllWithLimitOffsetByOrgIds mbLimit mbOffset orgIds = do
  personDbTable <- QPerson.getDbTable
  driverInfoDbTable <- getDbTable

  DB.findAllByJoin (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) (joinQuery personDbTable driverInfoDbTable)
    >>= traverse (bimapM decrypt return)
  where
    orderByDesc (Person.Person {..}, _) = B.desc_ _createdAt
    limit = fromMaybe 100 mbLimit
    offset = fromMaybe 0 mbOffset
    joinQuery personDbTable driverInfoDbTable = do
      person <- B.all_ personDbTable
      driverInfo <- B.join_ driverInfoDbTable $ \row -> do
        row ^. #_driverId B.==. person ^. #_id
      B.guard_ $ predicate person
      return (person, driverInfo)

    predicate Person.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role B.==. B.val_ Person.DRIVER
            B.&&. _organizationId `B.in_` (B.val_ . Just <$> orgIds) B.||. complementVal orgIds
        ]
