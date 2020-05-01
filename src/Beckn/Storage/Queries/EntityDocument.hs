{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.EntityDocument where

import           Database.Beam                      ((&&.), (<-.), (==.))
import           EulerHS.Prelude                    hiding (id)

import qualified Beckn.Storage.Queries              as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB             as DB
import qualified Beckn.Types.Storage.EntityDocument as Storage

import           Beckn.Utils.Common
import qualified Database.Beam                      as B
import qualified EulerHS.Language                   as L
import qualified EulerHS.Types                      as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.EntityDocumentT)
dbTable = DB._entityDocument DB.becknDb

create :: Storage.EntityDocument -> L.Flow ()
create Storage.EntityDocument {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.EntityDocument {..}) >>=
  either DB.throwDBError pure

findById :: EntityDocumentId -> L.Flow (T.DBResult (Maybe Storage.EntityDocument))
findById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.EntityDocument {..} = (_id ==. B.val_ id)

findAllIds :: Text -> DocumentByType -> L.Flow [Storage.EntityDocument]
findAllIds entityId dt =
  DB.findAll dbTable (predicate entityId dt) >>=
    either DB.throwDBError pure
  where
    predicate e VERIFIER Storage.EntityDocument {..} = _VerifiedBy ==. B.val_ (Just e)
    predicate e CREATOR Storage.EntityDocument {..} = _CreatedBy ==. B.val_ e



findAllByCustomerId :: CustomerId ->  L.Flow [Storage.EntityDocument]
findAllByCustomerId (CustomerId cId) =
  DB.findAll dbTable (predicate cId) >>=
    either DB.throwDBError pure
  where
    predicate cId Storage.EntityDocument {..} = (_EntityId ==. B.val_ cId)
                                                &&. (_entityType ==. B.val_ CUSTOMER)


findAllByPassApplicationId :: PassApplicationId ->  L.Flow [Storage.EntityDocument]
findAllByPassApplicationId (PassApplicationId eId) =
  DB.findAll dbTable (predicate eId) >>=
    either DB.throwDBError pure
  where
    predicate eId Storage.EntityDocument {..} = (_EntityId ==. B.val_ eId)
                                                &&. (_entityType ==. B.val_ PASSAPPLICATION)
