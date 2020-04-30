{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Document where

import           Database.Beam                ((&&.), (<-.), (==.))
import           EulerHS.Prelude              hiding (id)

import qualified Beckn.Storage.Queries        as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB       as DB
import qualified Beckn.Types.Storage.Document as Storage

import           Beckn.Utils.Common
import           Data.Time
import           Data.Time.LocalTime
import qualified Database.Beam                as B
import qualified EulerHS.Language             as L
import qualified EulerHS.Types                as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.DocumentT)
dbTable = DB._document DB.becknDb

create :: Storage.Document -> L.Flow ()
create Storage.Document {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Document {..}) >>=
  either DB.throwDBError pure

findById :: DocumentId -> L.Flow (T.DBResult (Maybe Storage.Document))
findById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.Document {..} = (_id ==. B.val_ id)
