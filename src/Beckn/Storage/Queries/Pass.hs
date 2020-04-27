module Beckn.Storage.Queries.Pass where

import           Database.Beam            ((&&.), (<-.), (==.))
import           EulerHS.Prelude          hiding (id)

import qualified Beckn.Storage.Queries    as DB
import           Beckn.Types.App
import qualified Beckn.Types.Domain.Pass  as Domain
import qualified Beckn.Types.Storage.DB   as DB
import qualified Beckn.Types.Storage.Pass as Storage
import qualified Database.Beam            as B
import qualified EulerHS.Language         as L
import qualified EulerHS.Types            as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.PassT)
dbTable = DB._pass DB.becknDb

create :: Domain.Pass -> L.Flow ()
create Domain.Pass{..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Pass{..}) >>=
  either DB.throwDBError pure

findPassById :: PassId -> L.Flow (T.DBResult (Maybe Domain.Pass))
findPassById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.Pass {..} = (_id ==. B.val_ id)
