{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.BecknAPIError where

import Beckn.Types.Core.Ack
import Beckn.Types.Error.BecknAPIError
import Beckn.Utils.Servant.Client
import EulerHS.Prelude
import qualified EulerHS.Types as ET

data BecknAPICallError = BecknAPICallError Text Error
  deriving (Show)

instanceExceptionWithParent 'APIException ''BecknAPICallError

instance IsAPIError BecknAPICallError where
  toErrorCode (BecknAPICallError _ _) = "BECKN_API_CALL_ERROR"
  toMessage (BecknAPICallError action Error {..}) =
    Just $
      "Beckn " <> action <> " request returned error code " <> code
        <> maybe "" ("with message: " <>) message

callBecknAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env AckResponse ()
callBecknAPI mbManagerSelector errorCodeMb baseUrl eulerClient name =
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
