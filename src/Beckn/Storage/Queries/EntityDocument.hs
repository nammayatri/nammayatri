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
