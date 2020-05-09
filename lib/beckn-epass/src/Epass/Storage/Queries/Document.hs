module Epass.Storage.Queries.Document where

import           Database.Beam                ((&&.), (<-.), (==.))
import           EulerHS.Prelude              hiding (id)

import qualified Epass.Storage.Queries        as DB
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Storage.DB       as DB
import qualified Epass.Types.Storage.Document as Storage

import           Epass.Utils.Common
import           Data.Time
import           Data.Time.LocalTime
import qualified Database.Beam                as B
import qualified EulerHS.Language             as L
import qualified EulerHS.Types                as T

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.DocumentT)
dbTable = DB._document DB.becknDb

create :: Storage.Document -> L.Flow ()
create Storage.Document {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Document {..}) >>=
  either DB.throwDBError pure

findById :: DocumentId -> L.Flow (Maybe Storage.Document)
findById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Document {..} = (_id ==. B.val_ id)
