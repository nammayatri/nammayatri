{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.EntityDocument where

import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.EntityDocument as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.EntityDocumentT)
dbTable = DB._entityDocument DB.becknDb

create :: Storage.EntityDocument -> L.Flow ()
create Storage.EntityDocument {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.EntityDocument {..})
    >>= either DB.throwDBError pure

findById :: EntityDocumentId -> L.Flow (T.DBResult (Maybe Storage.EntityDocument))
findById id =
  DB.findOne dbTable predicate
  where
    predicate Storage.EntityDocument {..} = _id ==. B.val_ id

findAllIds :: Text -> DocumentByType -> L.Flow [Storage.EntityDocument]
findAllIds entityId dt =
  DB.findAll dbTable (predicate entityId dt)
    >>= either DB.throwDBError pure
  where
    predicate e VERIFIER Storage.EntityDocument {..} = _VerifiedBy ==. B.val_ (Just e)
    predicate e CREATOR Storage.EntityDocument {..} = _CreatedBy ==. B.val_ e

findAllByCustomerId :: CustomerId -> L.Flow [Storage.EntityDocument]
findAllByCustomerId (CustomerId cId) =
  DB.findAll dbTable (predicate cId)
    >>= either DB.throwDBError pure
  where
    predicate cId Storage.EntityDocument {..} =
      (_EntityId ==. B.val_ cId)
        &&. (_entityType ==. B.val_ CUSTOMER)

findAllByPassApplicationId :: PassApplicationId -> L.Flow [Storage.EntityDocument]
findAllByPassApplicationId (PassApplicationId eId) =
  DB.findAll dbTable (predicate eId)
    >>= either DB.throwDBError pure
  where
    predicate eId Storage.EntityDocument {..} =
      (_EntityId ==. B.val_ eId)
        &&. (_entityType ==. B.val_ PASSAPPLICATION)

findAllByOrgId :: OrganizationId -> L.Flow [Storage.EntityDocument]
findAllByOrgId (OrganizationId eId) =
  DB.findAll dbTable (predicate eId)
    >>= either DB.throwDBError pure
  where
    predicate eId Storage.EntityDocument {..} =
      (_EntityId ==. B.val_ eId)
        &&. (_entityType ==. B.val_ ORGANIZATIONS)

findAllByCaseId :: CaseId -> L.Flow [Storage.EntityDocument]
findAllByCaseId (CaseId cId) =
  DB.findAll dbTable (predicate cId)
    >>= either DB.throwDBError pure
  where
    predicate eId Storage.EntityDocument {..} =
      (_EntityId ==. B.val_ cId)
        &&. (_entityType ==. B.val_ PASSAPPLICATION)
