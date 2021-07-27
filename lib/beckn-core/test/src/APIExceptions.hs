{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module APIExceptions (httpExceptionTests) where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Monitoring.Prometheus.Metrics
import qualified Beckn.Types.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Error.FlowHandling
import Control.Arrow (left)
import qualified Data.Aeson as A
import EulerHS.Prelude
import qualified Servant as S
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import TestSilentIOLogger ()

data SomeAPIError = SomeAPIError deriving (Show)

instance IsBaseError SomeAPIError

instance IsHTTPError SomeAPIError where
  toErrorCode SomeAPIError = "SOME_API_ERROR"

instance IsAPIError SomeAPIError

instance IsBecknAPIError SomeAPIError

instanceExceptionWithParent 'HTTPException ''SomeAPIError

data SomeBecknAPIError = SomeBecknAPIError deriving (Show)

instance IsBaseError SomeBecknAPIError

instance IsHTTPError SomeBecknAPIError where
  toErrorCode SomeBecknAPIError = "SOME_BECKN_API_ERROR"

instance IsAPIError SomeBecknAPIError

instance IsBecknAPIError SomeBecknAPIError where
  toType SomeBecknAPIError = INTERNAL_ERROR

instanceExceptionWithParent 'HTTPException ''SomeBecknAPIError

instance Metrics.CoreMetrics IO where
  addRequestLatency _ _ _ _ = return ()
  incrementErrorCounter _ = return ()
  addUrlCallRetries _ _ _ = return ()

httpExceptionTests :: TestTree
httpExceptionTests =
  testGroup
    "Endpoint exception catchers tests"
    [ testGroup
        "Throwing any error in our endpoints must return HTTPError"
        [ apiErrorInEndpoint,
          becknApiErrorInEndpoint,
          someErrorInEndpoint
        ],
      testGroup
        "Throwing any error in Beckn endpoints must return BecknAPIError"
        [ apiErrorInBecknEndpoint,
          becknApiErrorInBecknEndpoint,
          someErrorInBecknEndpoint
        ]
    ]

apiErrorInEndpoint :: TestTree
apiErrorInEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @APIError $ apiHandler (throwM SomeAPIError)

becknApiErrorInEndpoint :: TestTree
becknApiErrorInEndpoint =
  testCase "Throwing some Beckn API error" $
    mustThrow @APIError $ apiHandler (throwM SomeBecknAPIError)

someErrorInEndpoint :: TestTree
someErrorInEndpoint =
  testCase "Throwing SomeException" $
    mustThrow @APIError $ apiHandler (error "Some error")

apiErrorInBecknEndpoint :: TestTree
apiErrorInBecknEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @BecknAPIError $ becknApiHandler (throwM SomeAPIError)

becknApiErrorInBecknEndpoint :: TestTree
becknApiErrorInBecknEndpoint =
  testCase "Throwing some Beckn API error" $
    mustThrow @BecknAPIError $ becknApiHandler (throwM SomeBecknAPIError)

someErrorInBecknEndpoint :: TestTree
someErrorInBecknEndpoint =
  testCase "Throwing SomeException" $
    mustThrow @BecknAPIError $ becknApiHandler (error "Some error")

mustThrow :: forall (e :: Type). (Show e, FromJSON e) => IO () -> IO ()
mustThrow flow = try flow >>= (`shouldSatisfy` isLeft) . serverErrorTo @e

serverErrorTo :: FromJSON a => Either S.ServerError () -> Either a ()
serverErrorTo = left (fromJust . A.decode . S.errBody)
