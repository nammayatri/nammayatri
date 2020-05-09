module Product.BecknProvider.BP where

import           Beckn.Types.App
import           Beckn.Types.API.Search
import           Beckn.Types.Storage.Organization    as Org
import           Beckn.Types.Storage.Person          as Person
import           Beckn.Types.Storage.Case
import           Beckn.Utils.Common
import           Data.Aeson
import           Data.Accessor as Lens
import           Data.Time.Clock
import           Data.Time.LocalTime
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
  uuid <- L.generateGUID
  -- get customer info and do findOrCreate Person?? or store customerInfo in requestor?
  currTime <- getCurrentTimeUTC
  validity <- getValidTime currTime --addLocalTime (60*30 :: NominalDiffTime) currTime 
  let c = mkCase req uuid currTime validity
  Case.create c
  transporters <- listOrganizations Nothing Nothing [Org.TRANSPORTER] [Org.APPROVED]
  -- TODO : Fix show
  admins       <- findAllByOrgIds
                  [Person.ADMIN]
                  ((\o -> show $ Org._id o) <$> transporters)
  -- notifyTransporters c admins TODO : Uncomment this once we start saving deviceToken
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

getValidTime :: LocalTime -> L.Flow LocalTime
getValidTime now = pure $ addLocalTime (60*30 :: NominalDiffTime) now 

mkCase :: SearchReq -> Text -> LocalTime -> LocalTime -> Case
mkCase req uuid now validity = Case
  { _id = CaseId { _getCaseId = uuid}
  , _name = Nothing
  , _description = Just "Case to create a Ride"
  , _shortId = uuid -- need to generate shortId
  , _industry = MOBILITY
  , _type = RIDEBOOK
  , _exchangeType = FULFILLMENT
  , _status = NEW
  , _startTime = now
  , _endTime = Nothing
  , _validTill = validity
-- adding 30 mins for request validation.
  , _provider = Nothing
  , _providerType = Nothing
  , _requestor = Nothing 
-- Need to extract from Req and add it here
  , _requestorType = Just CONSUMER
  , _parentCaseId = Nothing
  , _udf1 = Nothing
  , _udf2 = Nothing
  , _udf3 = Nothing
  , _udf4 = Nothing
  , _udf5 = Nothing
  , _info = Nothing -- need to store the req
  , _createdAt = now
  , _updatedAt = now 
  }