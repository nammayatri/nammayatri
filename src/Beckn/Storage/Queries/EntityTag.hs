{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.EntityTag where

import           Database.Beam                 ((&&.), (<-.), (==.))
import           EulerHS.Prelude               hiding (id)

import qualified Beckn.Storage.Queries         as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB        as DB
import qualified Beckn.Types.Storage.EntityTag as Storage

import           Beckn.Utils.Common
import qualified Database.Beam                 as B
import qualified EulerHS.Language              as L
import qualified EulerHS.Types                 as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.EntityTagT)
dbTable = DB._entityTag DB.becknDb

create :: Storage.EntityTag -> L.Flow ()
create Storage.EntityTag {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.EntityTag {..}) >>=
  either DB.throwDBError pure

findById :: EntityTagId -> L.Flow (Maybe Storage.EntityTag)
findById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.EntityTag {..} = (_id ==. B.val_ id)
