module Utils.Common where

import qualified Beckn.Types.APIResult as APIResult
import qualified Beckn.Types.Error as APIError
import Data.Aeson (decode)
import EulerHS.Prelude
import qualified Servant.Server.Internal as Servant

errorCodeWhenLeft :: Either Servant.ServerError APIResult.APIResult -> Either Text APIResult.APIResult
errorCodeWhenLeft = first (APIError.errorCode . fromJust . mbApiError)
  where
    mbApiError err = decode (Servant.errBody err) :: Maybe APIError.APIError
