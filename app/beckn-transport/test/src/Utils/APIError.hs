module Utils.APIError where

import Beckn.TypeClass.IsAPIError
import qualified Beckn.Types.APISuccess as APISuccess
import Data.Aeson (decode)
import EulerHS.Prelude
import qualified Servant.Server.Internal as Servant
import Test.Tasty.HUnit

mustBeErrorCode ::
  IsAPIError e =>
  e ->
  Either Servant.ServerError a ->
  Assertion
mustBeErrorCode expectedError result =
  case result of
    Left err -> resultErrorCode err @?= expectedErrorCode
    Right _ -> assertFailure "An error result expected"
  where
    resultErrorCode err = errorCode $ fromJust $ decode $ Servant.errBody err
    expectedErrorCode = errorCode $ toAPIError expectedError
