module Product.BecknProvider.BP where

import           Beckn.Types.API.Search
import           Beckn.Types.Storage.Organization    as Org
import           Beckn.Types.Storage.Person          as Person
import           Beckn.Types.Storage.Case
import           Beckn.Utils.Common
import           Data.Aeson
import qualified EulerHS.Language                     as L
import           EulerHS.Prelude
import           Servant
import           Storage.Queries.Case                as Case
import           Storage.Queries.Person              as Person
import           Storage.Queries.Organization        as Org
import           Types.App
import           Types.Notification
import           Utils.Routes
import           Utils.FCM


-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using GCM
-- 3) Respond with Ack

search :: Text -> SearchReq -> FlowHandler SearchRes
search apiKey req = withFlowHandler $ do
  let c = mkCase req
  Case.create c
  transporters <- listOrganizations Nothing Nothing [Org.TRANSPORTER] [Org.APPROVED]
  -- TODO : Fix show
  admins       <- findAllByOrgIds
                  [Person.ADMIN]
                  ((\o -> show $ Org._id o) <$> transporters)
  -- notifyTransporters c admins TODO : Uncomment this once we start saving deviceToken
  uuid <- L.generateGUID
  mkAckResponse uuid "search"

notifyTransporters :: Case -> [Person] -> L.Flow ()
notifyTransporters c admins =
  -- TODO : Get Token from Person
  traverse_ (\p -> sendNotification mkCaseNotification "deviceToken") admins
  where mkCaseNotification =
          Notification
            { _type    = LEAD
            , _payload = c
            }

mkCase :: SearchReq -> Case
mkCase req = undefined