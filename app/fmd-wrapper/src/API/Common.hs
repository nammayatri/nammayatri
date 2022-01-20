module API.Common where

import App.Types
import Beckn.Types.Id
import Beckn.Types.Registry.Subscriber (Subscriber)
import EulerHS.Prelude
import qualified ExternalAPI.Dunzo.Flow as DzAPI
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.Dunzo as QDz
import qualified Storage.Queries.Organization as QOrg
import Types.Beckn.Context
import qualified Types.Beckn.DecimalValue as DecimalValue
import qualified Types.Beckn.Domain as Domain
import qualified Types.Beckn.Payment as Payment
import Types.Common
import Types.Error
import qualified Types.Storage.Organization as SOrg
import Types.Wrapper
import Utils.Common
import Utils.Context

findOrg :: DBFlow m r => Subscriber -> m SOrg.Organization
findOrg subscriber =
  QOrg.findOrgByShortId (ShortId subscriber.subscriber_id)
    >>= fromMaybeM OrgDoesNotExist

getStatus ::
  DzBAConfig ->
  DunzoConfig ->
  Dz.TaskId ->
  Flow Dz.TaskStatus
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  DzAPI.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken ::
  DzBAConfig ->
  DunzoConfig ->
  Flow Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- QDz.getToken dzClientId
  case mToken of
    Nothing -> do
      Dz.TokenRes token <- DzAPI.getToken dzTokenUrl (Dz.TokenReq dzClientId dzClientSecret)
      QDz.insertToken dzClientId token
      return token
    Just token -> return token

validateContext :: HasFlowEnv m r '["coreVersion" ::: Text] => Action -> Context -> m ()
validateContext action context = do
  validateDomainMig Domain.FINAL_MILE_DELIVERY context
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

getDzBAPCreds :: MonadFlow m => SOrg.Organization -> m DzBAConfig
getDzBAPCreds = getClientConfig

mapTaskStateToOrderState :: Dz.TaskState -> Text
mapTaskStateToOrderState s =
  case s of
    Dz.CREATED -> "ACTIVE"
    Dz.QUEUED -> "ACTIVE"
    Dz.RUNNER_ACCEPTED -> "ACTIVE"
    Dz.REACHED_FOR_PICKUP -> "ACTIVE"
    Dz.PICKUP_COMPLETE -> "ACTIVE"
    Dz.STARTED_FOR_DELIVERY -> "ACTIVE"
    Dz.REACHED_FOR_DELIVERY -> "ACTIVE"
    Dz.DELIVERED -> "COMPLETED"
    Dz.CANCELLED -> "CANCELLED"
    Dz.RUNNER_CANCELLED -> "CANCELLED"

mkPayment :: Maybe Float -> Payment.Payment
mkPayment estimated_price =
  Payment.Payment
    { params =
        Payment.Params
          { amount = DecimalValue.DecimalValue . show <$> estimated_price,
            currency = "INR"
          },
      _type = Payment.POST_FULFILLMENT
    }
