{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.Tag where

import           Database.Beam                   ((&&.), (<-.), (==.))
import           EulerHS.Prelude                 hiding (id)

import qualified Epass.Storage.Queries           as DB
import qualified Epass.Storage.Queries.EntityTag as EntityTag
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Storage.DB          as DB
import qualified Epass.Types.Storage.EntityTag   as EntityTag
import qualified Epass.Types.Storage.Tag         as Storage

import           Epass.Utils.Common
import qualified Database.Beam                   as B
import qualified EulerHS.Language                as L
import qualified EulerHS.Types                   as T

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.TagT)
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

findAllById :: [TagId] -> L.Flow [Storage.Tag]
findAllById ids =
  DB.findAllOrErr dbTable (predicate ids)
  where
    predicate ids Storage.Tag {..} = (B.in_ _id (B.val_ <$> ids))

findAllByEntity :: Text -> Text -> L.Flow [Storage.Tag]
findAllByEntity entityType entityId = do
  etags <- EntityTag.findAllByEntity entityType entityId
  let tagIds = TagId <$> EntityTag._TagId <$> etags
  DB.findAll dbTable (predicate tagIds)
    >>= either DB.throwDBError pure
  where
    predicate tagIds Storage.Tag {..} =
      _id `B.in_` (B.val_ <$> tagIds)
