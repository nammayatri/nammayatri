{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.LocationBlacklist where

import           Database.Beam                         ((&&.), (<-.), (==.))
import           EulerHS.Prelude                       hiding (id)

import qualified Beckn.Storage.Queries                 as DB
import qualified Beckn.Storage.Queries                 as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB                as DB
import qualified Beckn.Types.Storage.DB                as DB
import qualified Beckn.Types.Storage.LocationBlacklist as Storage
import           Beckn.Utils.Common
import           Data.Time
import           Data.Time.LocalTime
import qualified Database.Beam                         as B
import qualified Database.Beam                         as B
import qualified EulerHS.Language                      as L
import qualified EulerHS.Language                      as L
import qualified EulerHS.Types                         as T
import qualified EulerHS.Types                         as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.LocationBlacklistT)
dbTable = DB._locationBlacklist DB.becknDb

create :: Storage.LocationBlacklist -> L.Flow ()
create Storage.LocationBlacklist {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.LocationBlacklist {..}) >>=
  either DB.throwDBError pure

findById :: LocationBlacklistId -> L.Flow (T.DBResult (Maybe Storage.LocationBlacklist))
findById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.LocationBlacklist {..} = (_id ==. B.val_ id)

-- findAllWithLimitOffset :: Maybe Int -> Maybe Int -> EntityType -> Text ->  L.Flow (T.DBResult [Storage.LocationBlacklist])
-- findAllWithLimitOffset mlimit moffset entityType entityId =
--   DB.findAllWithLimitOffsetWhere dbTable (pred entityType entityId) limit offset orderByDesc
--   where
--     limit = (toInteger $ fromMaybe 10 mlimit)
--     offset = (toInteger $ fromMaybe 0 moffset)
--     orderByDesc Storage.LocationBlacklist {..} = B.desc_ _createdAt
--     pred entityType entityId Storage.LocationBlacklist {..} = (_entityType ==. (B.val_ entityType)
--                                                   &&. _EntityId ==. (B.val_ entityId))

-- update ::
--   LocationBlacklistId
--   -> Maybe Int
--   -> Maybe LocalTime
--   -> Maybe LocalTime
--   -> L.Flow (T.DBResult ())
-- update id maxAllowedM  startTimeM endTimeM = do
--   (currTime :: LocalTime) <- getCurrTime
--   DB.update dbTable
--     (setClause maxAllowedM startTimeM endTimeM currTime)
--     (predicate id)
--   where
--     predicate id Storage.LocationBlacklist {..} = _id ==. B.val_ id
--     setClause maxAllowedM startTimeM endTimeM currTime Storage.LocationBlacklist {..} =
--       mconcat ([_updatedAt <-. B.val_ currTime]
--       <> maybe ([]) (return . (_startTime <-.) . B.val_) startTimeM
--       <> maybe ([]) (return . (_endTime <-.) . B.val_) endTimeM
--       <> maybe ([]) (return . (_maxAllowed <-.) . B.val_) maxAllowedM)
