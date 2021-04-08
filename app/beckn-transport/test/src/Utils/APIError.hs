module Utils.APIError where

import qualified Beckn.TypeClass.IsAPIError as APIError
import qualified Beckn.Types.APISuccess as APISuccess
import Data.Aeson (decode)
import EulerHS.Prelude
import qualified Servant.Server.Internal as Servant
import Test.Tasty.HUnit

errorCodeWhenLeft :: Either Servant.ServerError APISuccess.APISuccess -> Either Text APISuccess.APISuccess
errorCodeWhenLeft = first (APIError.errorCode . fromJust . mbApiError)
  where
    mbApiError err = decode (Servant.errBody err) :: Maybe APIError.APIError

mustBeErrorCode ::
  APIError.IsAPIError e =>
  e ->
  Either Servant.ServerError APISuccess.APISuccess ->
  Assertion
mustBeErrorCode err result =
  errorCodeWhenLeft result
    @?= Left (APIError.errorCode $ APIError.toAPIError err)
