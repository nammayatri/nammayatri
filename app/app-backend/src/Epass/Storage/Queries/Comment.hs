{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.Comment where

import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Comment as Storage
import qualified Epass.Types.Storage.DB as DB
import Epass.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.CommentT)
dbTable = DB._comment DB.becknDb

create :: Storage.Comment -> L.Flow ()
create Storage.Comment {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Comment {..})
    >>= either DB.throwDBError pure

findById :: CommentId -> L.Flow (Maybe Storage.Comment)
findById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Comment {..} = _id ==. B.val_ id

findAllByCommentedOnEntity :: Text -> Text -> L.Flow [Storage.Comment]
findAllByCommentedOnEntity commentedOnEntityType commentedOnEntityId =
  DB.findAll dbTable (predicate commentedOnEntityType commentedOnEntityId)
    >>= either DB.throwDBError pure
  where
    predicate commentedOnEntityType commentedOnEntityId Storage.Comment {..} =
      _commentedOnEntityType ==. B.val_ commentedOnEntityType
        &&. _CommentedOnEntityId ==. B.val_ commentedOnEntityId
