{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Tag where

import           Database.Beam                   ((&&.), (<-.), (==.))
import           EulerHS.Prelude                 hiding (id)

import qualified Beckn.Storage.Queries           as DB
import qualified Beckn.Storage.Queries.EntityTag as EntityTag
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB          as DB
import qualified Beckn.Types.Storage.EntityTag   as EntityTag
import qualified Beckn.Types.Storage.Tag         as Storage

import           Beckn.Utils.Common
import qualified Database.Beam                   as B
import qualified EulerHS.Language                as L
import qualified EulerHS.Types                   as T

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

findAllTagTypes :: L.Flow [Text]
findAllTagTypes = do
  DB.aggregate dbTable aggregator predicate
    >>= either DB.throwDBError pure
  where
    predicate tag = B.val_ True

    aggregator Storage.Tag{..} =
      B.group_ _tagType

findAllTagWhereType :: Text -> L.Flow [Text]
findAllTagWhereType tagType = do
  DB.aggregate dbTable aggregator (predicate tagType)
    >>= either DB.throwDBError pure
  where
    predicate tagType Storage.Tag{..} =
      _tagType ==. B.val_ tagType

    aggregator Storage.Tag{..} =
      B.group_ _tag

findAllByTag :: Text -> Text -> L.Flow [Storage.Tag]
findAllByTag tagType tag = do
  DB.findAll dbTable (predicate tagType tag)
    >>= either DB.throwDBError pure
  where
    predicate tagType tag Storage.Tag{..} =
      _tagType ==. B.val_ tagType &&.
      _tag ==. B.val_ tag
