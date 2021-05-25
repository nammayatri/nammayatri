{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError where

import Beckn.Types.Common
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Servant.Client
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant.Client (Client, HasClient)

data BecknAPICallError = BecknAPICallError Text Error
  deriving (Show, IsAPIError, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''BecknAPICallError

instance IsBaseError BecknAPICallError where
  toMessage (BecknAPICallError action Error {..}) =
    Just $
      "Beckn " <> action <> " request returned error code " <> code
        <> maybe "" ("with message: " <>) message

instance IsHTTPError BecknAPICallError where
  toErrorCode (BecknAPICallError _ _) = "BECKN_API_CALL_ERROR"

type IsBecknAPI api req res =
  ( HasClient ET.EulerClient api,
    Client ET.EulerClient api ~ (req -> ET.EulerClient res),
    ET.JSONEx res,
    ToJSON res
  )

callBecknAPI ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res
  ) =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m res
callBecknAPI mbManagerSelector errorCodeMb action api baseUrl req =
  callBecknAPI' mbManagerSelector errorCodeMb baseUrl (ET.client api req) action

callBecknAPI' ::
  MonadFlow m =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI m res
callBecknAPI' mbManagerSelector errorCodeMb baseUrl eulerClient name =
  callApiUnwrappingApiError
    (becknAPIErrorToException name)
    mbManagerSelector
    errorCodeMb
    baseUrl
    eulerClient
    name

callPseudoBecknAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callPseudoBecknAPI mbManagerSelector errorCodeMb baseUrl eulerClient name =
  callApiUnwrappingApiError
    (becknAPIErrorToException name)
    mbManagerSelector
    errorCodeMb
    baseUrl
    eulerClient
    name

becknAPIErrorToException :: Text -> BecknAPIError -> BecknAPICallError
becknAPIErrorToException name (BecknAPIError becknErr) = BecknAPICallError name becknErr

toBecknAPIError :: (IsHTTPError e, IsBecknAPIError e) => e -> BecknAPIError
toBecknAPIError e =
  BecknAPIError
    Error
      { _type = toType e,
        code = toErrorCode e,
        path = toPath e,
        message = toMessageIfNotInternal e
      }
