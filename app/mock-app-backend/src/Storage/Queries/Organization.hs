module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as App
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Org.OrganizationT))
getDbTable =
  DB._organization . DB.appDb <$> getSchemaName

findOrgByApiKey :: App.APIKey -> Flow (Maybe Org.Organization)
findOrgByApiKey apiKey = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
  where
    predicate Org.Organization {..} =
      _apiKey ==. B.val_ (Just apiKey)

findOrgByCallbackUrl :: BaseUrl -> Flow (Maybe Org.Organization)
findOrgByCallbackUrl cbUrl = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
  where
    predicate Org.Organization {..} =
      _callbackUrl ==. B.val_ (Just cbUrl)

findOrgByShortId :: ShortId Org.Organization -> Flow (Maybe Org.Organization)
findOrgByShortId shortOrgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
  where
    predicate Org.Organization {..} =
      _shortId ==. B.val_ shortOrgId
