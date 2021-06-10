{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module APIExceptions (apiExceptionTests) where

import Beckn.Types.Common
import Beckn.Types.Error.APIError
import Beckn.Types.Error.BecknAPIError
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Utils.Error.FlowHandling
import Control.Arrow (left)
import qualified Data.Aeson as A
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified Servant as S
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import TestSilentIOLogger ()

data SomeAPIError = SomeAPIError deriving (Show)

instance IsAPIError SomeAPIError where
  toErrorCode SomeAPIError = "SOME_API_ERROR"

instanceExceptionWithParent 'APIException ''SomeAPIError

data SomeBecknAPIError = SomeBecknAPIError deriving (Show)

instance IsAPIError SomeBecknAPIError where
  toErrorCode SomeBecknAPIError = "SOME_BECKN_API_ERROR"

instance IsBecknAPIError SomeBecknAPIError where
  toType SomeBecknAPIError = INTERNAL_ERROR

instanceExceptionWithParent 'BecknAPIException ''SomeBecknAPIError

data TestEnv = TestEnv
  { metricsRequestLatency :: RequestLatencyMetric,
    metricsErrorCounter :: ErrorCounterMetric
  }

buildTestEnv :: IO TestEnv
buildTestEnv = do
  metricsRequestLatency <- registerRequestLatencyMetric
  metricsErrorCounter <- registerErrorCounterMetric
  return $ TestEnv {..}

apiExceptionTests :: TestTree
apiExceptionTests =
  testGroup
    "Endpoint exception catchers tests"
    [ testGroup
        "Throwing any error in our endpoints must return APIError"
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
  testCase "Throwing some API error" $ do
    testEnv <- buildTestEnv
    mustThrow @APIError $
      R.withFlowRuntime Nothing $ \flowRt -> do
        runFlowR flowRt testEnv $ apiHandler (throwM SomeAPIError)

becknApiErrorInEndpoint :: TestTree
becknApiErrorInEndpoint =
  testCase "Throwing some Beckn API error" $ do
    testEnv <- buildTestEnv
    mustThrow @APIError $
      R.withFlowRuntime Nothing $ \flowRt -> do
        runFlowR flowRt testEnv $ apiHandler (throwM SomeBecknAPIError)

someErrorInEndpoint :: TestTree
someErrorInEndpoint =
  testCase "Throwing SomeException" $ do
    testEnv <- buildTestEnv
    mustThrow @APIError $
      R.withFlowRuntime Nothing $ \flowRt -> do
        runFlowR flowRt testEnv $ apiHandler (error "Some error")

apiErrorInBecknEndpoint :: TestTree
apiErrorInBecknEndpoint =
  testCase "Throwing some API error" $ do
    testEnv <- buildTestEnv
    mustThrow @BecknAPIError $
      R.withFlowRuntime Nothing $ \flowRt -> do
        runFlowR flowRt testEnv $ becknApiHandler (throwM SomeAPIError)

becknApiErrorInBecknEndpoint :: TestTree
becknApiErrorInBecknEndpoint =
  testCase "Throwing some Beckn API error" $ do
    testEnv <- buildTestEnv
    mustThrow @BecknAPIError $
      R.withFlowRuntime Nothing $ \flowRt -> do
        runFlowR flowRt testEnv $ becknApiHandler (throwM SomeBecknAPIError)

someErrorInBecknEndpoint :: TestTree
someErrorInBecknEndpoint =
  testCase "Throwing SomeException" $ do
    testEnv <- buildTestEnv
    mustThrow @BecknAPIError $
      R.withFlowRuntime Nothing $ \flowRt -> do
        runFlowR flowRt testEnv $ becknApiHandler (error "Some error")

mustThrow :: forall (e :: Type). (Show e, FromJSON e) => IO () -> IO ()
mustThrow flow = try flow >>= (`shouldSatisfy` isLeft) . serverErrorTo @e

serverErrorTo :: FromJSON a => Either S.ServerError () -> Either a ()
serverErrorTo = left (fromJust . A.decode . S.errBody)
