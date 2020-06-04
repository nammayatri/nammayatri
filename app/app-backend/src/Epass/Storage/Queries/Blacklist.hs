{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.Blacklist where

import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Time
import Data.Time.LocalTime
import Database.Beam ((&&.), (<-.), (==.), in_)
import qualified Database.Beam as B
import qualified Database.Beam as B
import qualified Epass.Types.API.Blacklist as API
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Blacklist as Storage
import qualified Epass.Types.Storage.Blacklist as Storage
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.DB as DB
import qualified EulerHS.Language as L
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.BlacklistT)
dbTable = DB._Blacklist DB.becknDb

create :: Storage.Blacklist -> L.Flow ()
create Storage.Blacklist {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Blacklist {..})
    >>= either DB.throwDBError pure

findById :: BlacklistId -> L.Flow (T.DBResult (Maybe Storage.Blacklist))
findById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.Blacklist {..} = (_id ==. B.val_ id)

update ::
  BlacklistId ->
  API.UpdateReq ->
  L.Flow (T.DBResult ())
update id API.UpdateReq {..} = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause _remarks _TenantOrganizationId _info _entityType _EntityId _startTime _endTime currTime)
    (predicate id)
  where
    predicate id Storage.Blacklist {..} = _id ==. B.val_ id
    setClause remarksM tenantOrganizationIdM infoM entityType entityId startTimeM endTimeM currTime Storage.Blacklist {..} =
      mconcat
        ( [_updatedAt <-. B.val_ currTime]
            <> maybe [] (\x -> [_remarks <-. B.val_ x]) remarksM
            <> maybe [] (return . (__TenantOrganizationId <-.) . B.val_ . Just) tenantOrganizationIdM
            <> maybe [] (return . (_info <-.) . B.val_ . Just) infoM
            <> maybe [] (\x -> [__EntityId <-. B.val_ x]) entityId
            <> maybe [] (\x -> [_entityType <-. B.val_ x]) entityType
            <> maybe [] (return . (_startTime <-.) . B.val_) startTimeM
            <> maybe [] (return . (_endTime <-.) . B.val_) endTimeM
        )

deleteById :: BlacklistId -> L.Flow (T.DBResult ())
deleteById id =
  DB.delete dbTable (predicate id)
  where
    predicate carrierId Storage.Blacklist {..} = _id ==. B.val_ carrierId

findAllWithLimitOffset :: Maybe Int -> Maybe Int -> EntityType -> Text -> L.Flow (T.DBResult [Storage.Blacklist])
findAllWithLimitOffset mlimit moffset entityType entityId =
  DB.findAllWithLimitOffsetWhere dbTable (pred entityType entityId) limit offset orderByDesc
  where
    limit = (toInteger $ fromMaybe 10 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    orderByDesc Storage.Blacklist {..} = B.desc_ _createdAt
    pred entityType entityId Storage.Blacklist {..} =
      ( _entityType ==. (B.val_ entityType)
          &&. __EntityId ==. (B.val_ entityId)
      )

findAllByEntityId :: EntityType -> [Text] -> L.Flow [Storage.Blacklist]
findAllByEntityId entityType entityIds =
  DB.findAllOrErr dbTable (pred entityType entityIds)
  where
    pred entityType entityIds Storage.Blacklist {..} =
      ( _entityType ==. (B.val_ entityType)
          &&. B.in_ __EntityId (B.val_ <$> entityIds)
      )

findByOrgId :: OrganizationId -> L.Flow (Maybe Storage.Blacklist)
findByOrgId (OrganizationId eId) =
  DB.findOne dbTable (pred eId)
    >>= either DB.throwDBError pure
  where
    pred eId Storage.Blacklist {..} =
      (__EntityId ==. B.val_ eId)
        &&. (_entityType ==. B.val_ ORG)

findByLocationId :: Text -> L.Flow (Maybe Storage.Blacklist)
findByLocationId eid =
  DB.findOne dbTable (pred eid)
    >>= either DB.throwDBError pure
  where
    pred eId Storage.Blacklist {..} =
      (__EntityId ==. B.val_ eId)
        &&. (_entityType ==. B.val_ LOCATION)
