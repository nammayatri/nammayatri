module Beckn.Storage.Queries.PassApplication where

import           Database.Beam                       ((&&.), (<-.), (==.))
import           EulerHS.Prelude                     hiding (id)

import qualified Beckn.Storage.Queries               as DB
import           Beckn.Types.App
import qualified Beckn.Types.Domain.PassApplication  as Domain
import qualified Beckn.Types.Storage.DB              as DB
import qualified Beckn.Types.Storage.PassApplication as Storage
import qualified Database.Beam                       as B
import qualified EulerHS.Language                    as L
import qualified EulerHS.Types                       as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.PassApplicationT)
dbTable = DB._passApplication DB.becknDb

create :: Domain.PassApplication -> L.Flow ()
create Domain.PassApplication{..} =
  DB.createOne dbTable (Storage.insertExpression Storage.PassApplication{..}) >>=
  either DB.throwDBError pure

findPassApplicationById :: PassApplicationId -> L.Flow (T.DBResult (Maybe Domain.PassApplication))
findPassApplicationById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.PassApplication {..} = (_id ==. B.val_ id)
