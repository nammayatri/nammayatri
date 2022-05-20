module API.Common where

import App.Types
import Beckn.Types.Id
import Beckn.Types.Registry.Subscriber (Subscriber)
import qualified Domain.DunzoCreds as DDunzoCreds
import qualified Domain.Organization as DOrg
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Dunzo.Flow as DzAPI
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.Dunzo as QDz
import qualified Storage.Queries.DunzoCreds as QDunzoCreds
import qualified Storage.Queries.Organization as QOrg
import Types.Beckn.Context
import qualified Types.Beckn.Domain as Domain
import qualified Types.Beckn.Order as Order
import Types.Common
import Types.Error
import Types.Wrapper
import Utils.Common
import Utils.Context

findOrg :: EsqDBFlow m r => Subscriber -> m DOrg.Organization
findOrg subscriber =
  QOrg.findOrgByShortId (ShortId subscriber.subscriber_id)
    >>= fromMaybeM (OrgDoesNotExist subscriber.subscriber_id)

getStatus ::
  DDunzoCreds.DunzoCreds ->
  DunzoConfig ->
  Dz.TaskId ->
  Flow Dz.TaskStatus
getStatus dzBACreds@DDunzoCreds.DunzoCreds {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  DzAPI.taskStatus clientId token dzUrl dzTestMode taskId

getCreds :: Id DDunzoCreds.DunzoCreds -> Flow DDunzoCreds.DunzoCreds
getCreds orgCredsId = do
  dzConfig <- asks (.dzConfig)
  let credsId = maybe orgCredsId Id dzConfig.dzCredsId
  QDunzoCreds.findById credsId >>= fromMaybeErr "DUNZO_CREDS_NOT_FOUND" (Just CORE003)

fetchToken ::
  DDunzoCreds.DunzoCreds ->
  DunzoConfig ->
  Flow Token
fetchToken DDunzoCreds.DunzoCreds {..} DunzoConfig {..} = do
  mToken <- QDz.getToken clientId
  case mToken of
    Nothing -> do
      Dz.TokenRes token <- DzAPI.getToken dzTokenUrl (Dz.TokenReq clientId clientSecret)
      QDz.insertToken clientId token
      return token
    Just token -> return token

validateContext :: HasFlowEnv m r '["coreVersion" ::: Text] => Action -> Context -> m ()
validateContext action context = do
  validateDomainMig Domain.LOGISTICS context
  validateContextCommonsMig action context

validateBapUrl :: MonadFlow m => Subscriber -> Context -> m ()
validateBapUrl subscriber context =
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap URL.")

updateBppIdAndUri :: Context -> Text -> BaseUrl -> Context
updateBppIdAndUri Context {..} bppId bpNwAddress =
  Context
    { bpp_uri = Just bpNwAddress,
      bpp_id = Just bppId,
      ..
    }

mapTaskStateToOrderState :: Dz.TaskState -> Order.Status
mapTaskStateToOrderState s =
  case s of
    Dz.CREATED -> Order.ACTIVE
    Dz.QUEUED -> Order.ACTIVE
    Dz.RUNNER_ACCEPTED -> Order.ACTIVE
    Dz.REACHED_FOR_PICKUP -> Order.ACTIVE
    Dz.PICKUP_COMPLETE -> Order.ACTIVE
    Dz.STARTED_FOR_DELIVERY -> Order.ACTIVE
    Dz.REACHED_FOR_DELIVERY -> Order.ACTIVE
    Dz.DELIVERED -> Order.COMPLETED
    Dz.CANCELLED -> Order.CANCELLED
    Dz.RUNNER_CANCELLED -> Order.CANCELLED
