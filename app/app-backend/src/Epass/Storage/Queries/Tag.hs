{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.Tag where

import App.Types
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import qualified Epass.Storage.Queries.EntityTag as EntityTag
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.EntityTag as EntityTag
import qualified Epass.Types.Storage.Tag as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.TagT)
dbTable = DB._tag DB.becknDb

create :: Storage.Tag -> Flow ()
create Storage.Tag {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Tag {..})
    >>= either DB.throwDBError pure

findById :: TagId -> Flow (Maybe Storage.Tag)
findById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Tag {..} = _id ==. B.val_ id

findAllById :: [TagId] -> Flow [Storage.Tag]
findAllById ids =
  DB.findAllOrErr dbTable (predicate ids)
  where
    predicate ids Storage.Tag {..} = B.in_ _id (B.val_ <$> ids)

findAllByEntity :: Text -> Text -> Flow [Storage.Tag]
findAllByEntity entityType entityId = do
  etags <- EntityTag.findAllByEntity entityType entityId
  let tagIds = TagId . EntityTag._TagId <$> etags
  DB.findAll dbTable (predicate tagIds)
    >>= either DB.throwDBError pure
  where
    predicate tagIds Storage.Tag {..} =
      _id `B.in_` (B.val_ <$> tagIds)

findAllTagTypes :: Flow [Text]
findAllTagTypes =
  DB.aggregate dbTable aggregator predicate
    >>= either DB.throwDBError pure
  where
    predicate tag = B.val_ True
    aggregator Storage.Tag {..} =
      B.group_ _tagType

findAllTagWhereType :: Text -> Flow [Text]
findAllTagWhereType tagType =
  DB.aggregate dbTable aggregator (predicate tagType)
    >>= either DB.throwDBError pure
  where
    predicate tagType Storage.Tag {..} =
      _tagType ==. B.val_ tagType
    aggregator Storage.Tag {..} =
      B.group_ _tag

findAllByTag :: Text -> Text -> Flow [Storage.Tag]
findAllByTag tagType tag =
  DB.findAll dbTable (predicate tagType tag)
    >>= either DB.throwDBError pure
  where
    predicate tagType tag Storage.Tag {..} =
      _tagType ==. B.val_ tagType
        &&. _tag ==. B.val_ tag
