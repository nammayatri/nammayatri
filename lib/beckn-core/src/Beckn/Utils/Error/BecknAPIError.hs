{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.BecknAPIError where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Error.BecknAPIError
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Servant.Client
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant.Client (Client, HasClient)

data BecknAPICallError = BecknAPICallError Text Error
  deriving (Show)

instanceExceptionWithParent 'APIException ''BecknAPICallError

instance IsAPIError BecknAPICallError where
  toErrorCode (BecknAPICallError _ _) = "BECKN_API_CALL_ERROR"
  toMessage (BecknAPICallError action Error {..}) =
    Just $
      "Beckn " <> action <> " request returned error code " <> code
        <> maybe "" ("with message: " <>) message

type IsBecknAPI api req =
  ( HasClient ET.EulerClient api,
    Client ET.EulerClient api ~ (req -> ET.EulerClient AckResponse)
  )

callBecknAPI ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req
  ) =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPI mbManagerSelector errorCodeMb action api baseUrl req =
  callBecknAPI' mbManagerSelector errorCodeMb baseUrl (ET.client api req) action

callBecknAPI' ::
  MonadFlow m =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI' m AckResponse ()
callBecknAPI' mbManagerSelector errorCodeMb baseUrl eulerClient name =
  void $
    callApiUnwrappingApiError
      (becknAPIErrorToException name)
      mbManagerSelector
      errorCodeMb
      baseUrl
      eulerClient
      name

becknAPIErrorToException :: Text -> BecknAPIError -> BecknAPICallError
becknAPIErrorToException name (BecknAPIError becknErr) = BecknAPICallError name becknErr
