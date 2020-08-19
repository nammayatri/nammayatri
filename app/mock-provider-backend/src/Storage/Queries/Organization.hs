module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Org
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Org.OrganizationT)
dbTable = DB._organization DB.appDb

findOrgByApiKey :: App.APIKey -> Flow (Maybe Org.Organization)
findOrgByApiKey apiKey =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Org.Organization {..} =
      _apiKey ==. B.val_ (Just apiKey)
