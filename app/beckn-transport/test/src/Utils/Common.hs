module Utils.Common where

import qualified Beckn.TypeClass.IsAPIError as APIError
import qualified Beckn.Types.APISuccess as APISuccess
import Data.Aeson (decode)
import EulerHS.Prelude
import qualified Servant.Server.Internal as Servant

errorCodeWhenLeft :: Either Servant.ServerError APISuccess.APISuccess -> Either Text APISuccess.APISuccess
errorCodeWhenLeft = first (APIError.errorCode . fromJust . mbApiError)
  where
    mbApiError err = decode (Servant.errBody err) :: Maybe APIError.APIError
