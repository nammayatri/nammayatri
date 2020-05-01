{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Tag where

import           Database.Beam           ((&&.), (<-.), (==.))
import           EulerHS.Prelude         hiding (id)

import qualified Beckn.Storage.Queries   as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB  as DB
import qualified Beckn.Types.Storage.Tag as Storage

import           Beckn.Utils.Common
import qualified Database.Beam           as B
import qualified EulerHS.Language        as L
import qualified EulerHS.Types           as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.TagT)
dbTable = DB._tag DB.becknDb

create :: Storage.Tag -> L.Flow ()
create Storage.Tag {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Tag {..}) >>=
  either DB.throwDBError pure

findById :: TagId -> L.Flow (Maybe Storage.Tag)
findById id = do
  DB.findOne dbTable predicate
  >>= either DB.throwDBError pure
  where
    predicate Storage.Tag {..} = (_id ==. B.val_ id)
