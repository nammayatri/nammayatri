module Product.BecknProvider.BP where

import           Beckn.Types.API.Search
import           Beckn.Types.Storage.Case
import           Beckn.Utils.Common
import           Storage.Queries.Case                as Case
import           Storage.Queries.Person              as Person
import           Storage.Queries.Organization        as Org
import           Beckn.Types.Storage.Organization    as Org
import           Beckn.Types.Storage.Person          as Person
import           Data.Aeson
import qualified EulerHS.Language                     as L
import           EulerHS.Prelude
import           Servant
import           Types.App
import           Utils.Routes


-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using GCM
-- 3) Respond with Ack

-- createCase
-- getAllOrganization
-- sendGCMToOrgAdmin
-- sendResponse

search :: Text -> SearchReq -> FlowHandler SearchRes
search apiKey req = withFlowHandler $ do
  let c = mkCase req
  Case.create c
  transporters <- listOrganizations Nothing Nothing [Org.TRANSPORTER] [Org.APPROVED]
  admins       <- findAdmins transporters
  notifyTransporters c admins
  mkAckResponse

findAdmins :: [Organization] -> L.Flow [Person]
findAdmins orgs = do
  findAllByOrgIds [Person.ADMIN] []

notifyTransporters :: Case -> [Person] -> L.Flow ()
notifyTransporters c admins = undefined

mkCase :: SearchReq -> Case
mkCase req = undefined