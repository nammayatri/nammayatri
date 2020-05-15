{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.EntityTag where

import Database.Beam ((&&.), (<-.), (==.), in_)
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.EntityTag as Storage
import Epass.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.EntityTagT)
dbTable = DB._entityTag DB.becknDb

create :: Storage.EntityTag -> L.Flow ()
create Storage.EntityTag {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.EntityTag {..})
    >>= either DB.throwDBError pure

findById :: EntityTagId -> L.Flow (Maybe Storage.EntityTag)
findById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.EntityTag {..} = (_id ==. B.val_ id)

findAllById :: [EntityTagId] -> L.Flow [Storage.EntityTag]
findAllById ids =
  DB.findAllOrErr dbTable (predicate ids)
  where
    predicate ids Storage.EntityTag {..} = (B.in_ _id (B.val_ <$> ids))

findAllByEntity :: Text -> Text -> L.Flow [Storage.EntityTag]
findAllByEntity entityType entityId = do
  DB.findAll dbTable (predicate entityId entityType)
    >>= either DB.throwDBError pure
  where
    predicate entityId entityType Storage.EntityTag {..} =
      _EntityId ==. B.val_ entityId
        &&. _entityType ==. B.val_ entityType
