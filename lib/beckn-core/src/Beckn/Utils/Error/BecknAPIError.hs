{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.BecknAPIError where

import Beckn.Types.Core.Ack
import Beckn.Types.Error.BecknAPIError
import Beckn.Types.Flow
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Servant.Client
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant.Client (BaseUrl, Client, HasClient)

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
  ( HasCoreMetrics env,
    IsBecknAPI api req
  ) =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  FlowR env ()
callBecknAPI mbManagerSelector errorCodeMb action api baseUrl req =
  callBecknAPI' mbManagerSelector errorCodeMb baseUrl (ET.client api req) action

callBecknAPI' ::
  HasCoreMetrics env =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI' env AckResponse ()
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
