module Epass.Models.Blacklist where

import App.Types
import Beckn.Types.Error
import Beckn.Utils.Common
import Epass.Storage.Queries.Blacklist as Q
import qualified Epass.Types.API.Blacklist as API
import Epass.Types.App
import Epass.Types.Common
import Epass.Types.Storage.Blacklist
import EulerHS.Prelude hiding (id)

-- | Create Blackist
create :: Blacklist -> Flow ()
create blacklist = do
  -- TODO add some validation checks
  -- and use throwDomainError if needed
  result <- Q.create blacklist
  checkDBError result

-- | Find Blackist by id
findById :: BlacklistId -> Flow (Maybe Blacklist)
findById id = do
  result <- Q.findById id
  checkDBError result

update ::
  BlacklistId ->
  API.UpdateReq ->
  Flow ()
update id updateReq = do
  result <- Q.update id updateReq
  checkDBError result

deleteById :: BlacklistId -> Flow ()
deleteById id = do
  result <- Q.deleteById id
  checkDBError result

findAllWithLimitOffset :: Maybe Int -> Maybe Int -> EntityType -> Text -> Flow [Blacklist]
findAllWithLimitOffset mlimit moffset entityType entityId = do
  result <- Q.findAllWithLimitOffset mlimit moffset entityType entityId
  checkDBError result

findAllByEntityId :: EntityType -> [Text] -> Flow [Blacklist]
findAllByEntityId entityType entityIds = do
  result <- Q.findAllByEntityId entityType entityIds
  checkDBError result

findByOrgId :: OrganizationId -> Flow (Maybe Blacklist)
findByOrgId orgId = do
  result <- Q.findByOrgId orgId
  checkDBError result

findByLocationId :: Text -> Flow (Maybe Blacklist)
findByLocationId eid = do
  result <- Q.findByLocationId eid
  checkDBError result
