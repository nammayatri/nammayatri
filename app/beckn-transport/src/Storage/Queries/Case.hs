module Storage.Queries.Case where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Beckn.Storage.Queries            as DB
import           Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Beckn.Types.Storage.Case         as Storage
import           Beckn.Utils.Common
import           Data.Time
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.transporterDb

create :: Storage.Case -> L.Flow ()
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..}) >>=
  either DB.throwDBError pure