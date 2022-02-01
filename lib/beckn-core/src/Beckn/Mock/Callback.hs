{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Beckn.Mock.Callback (withBecknCallbackMock, withBecknCallbackBapMock, WithBecknCallbackMock) where

import Beckn.Mock.App
import Beckn.Mock.ExternalAPI
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context as M.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Utils.Common hiding (callAPI, show)
import qualified Control.Monad.Catch as C
import Relude
import Servant.Client

someExceptionToCallbackReqMig :: M.Context.Context -> C.SomeException -> BecknCallbackReq a
someExceptionToCallbackReqMig context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in BecknCallbackReq
        { contents = Left err,
          context
        }

type WithBecknCallbackMock api callback_success e =
  ( HasClient ClientM api,
    Client ClientM api
      ~ (BecknCallbackReq callback_success -> ClientM AckResponse),
    Show callback_success,
    HasField "selfId" e Text,
    HasField "uniqueKeyId" e Text
  ) =>
  M.Context.Action ->
  M.Context.Context ->
  MockM e callback_success ->
  MockM e AckResponse

withBecknCallbackBapMock ::
  forall api e callback_success.
  () =>
  WithBecknCallbackMock api callback_success e
withBecknCallbackBapMock actionName context = withBecknCallbackMock @api (context.bap_uri) actionName context

withBecknCallbackMock ::
  forall api e callback_success.
  () =>
  BaseUrl ->
  WithBecknCallbackMock api callback_success e
withBecknCallbackMock cbUrl actionName context action = do
  now <- getCurrentTime
  cbAction <-
    M.Context.mapToCbAction actionName
      & fromMaybeM (InternalError $ "Beckn " <> show actionName <> " action doesn't have callback")
  let cbContext =
        context
          { action = cbAction,
            timestamp = now
          }
  forkBecknCallback
    (someExceptionToCallbackReqMig cbContext)
    (BecknCallbackReq cbContext . Right)
    (void . callAPI @api cbUrl)
    (show actionName)
    action
  return Ack

forkBecknCallback ::
  (C.SomeException -> result) ->
  (success -> result) ->
  (result -> MockM e ()) ->
  Text ->
  MockM e success ->
  MockM e ()
forkBecknCallback fromError fromSuccess doWithResult actionName action =
  fork actionName $
    C.try action >>= \case
      Right success -> doWithResult $ fromSuccess success
      Left err -> do
        logError $ "Error executing callback action " <> actionName <> ": " <> show err
        doWithResult $ fromError err
