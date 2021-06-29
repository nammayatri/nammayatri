{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module APIExceptions (apiExceptionTests) where

import Beckn.Types.Error.BaseError.APIError
import Beckn.Types.Error.BaseError.APIError.BecknAPIError
import Beckn.Types.Error.BaseError.APIError.DomainError
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

data SomeDomainError = SomeDomainError deriving (Show)

instance IsBaseError SomeDomainError

instance IsAPIError SomeDomainError where
  toErrorCode SomeDomainError = "SOME_API_ERROR"

instance IsDomainError SomeDomainError

instanceExceptionWithParent 'DomainException ''SomeDomainError

data SomeBecknAPIError = SomeBecknAPIError deriving (Show)

instance IsBaseError SomeBecknAPIError

instance IsAPIError SomeBecknAPIError where
  toErrorCode SomeBecknAPIError = "SOME_BECKN_API_ERROR"

instance IsBecknAPIError SomeBecknAPIError where
  toType SomeBecknAPIError = INTERNAL_ERROR

instanceExceptionWithParent 'BecknAPIException ''SomeBecknAPIError

instance Metrics.CoreMetrics IO where
  addRequestLatency _ _ _ _ = return ()
  incrementErrorCounter _ = return ()

apiExceptionTests :: TestTree
apiExceptionTests =
  testGroup
    "Endpoint exception catchers tests"
    [ testGroup
        "Throwing any error in our endpoints must return APIError"
        [ domainErrorInEndpoint,
          becknApiErrorInEndpoint,
          someErrorInEndpoint
        ],
      testGroup
        "Throwing any error in Beckn endpoints must return BecknAPIError"
        [ domainErrorInBecknEndpoint,
          becknApiErrorInBecknEndpoint,
          someErrorInBecknEndpoint
        ]
    ]

domainErrorInEndpoint :: TestTree
domainErrorInEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @DomainError $ domainHandler (throwM SomeDomainError)

becknApiErrorInEndpoint :: TestTree
becknApiErrorInEndpoint =
  testCase "Throwing some Beckn API error" $
    mustThrow @DomainError $ domainHandler (throwM SomeBecknAPIError)

someErrorInEndpoint :: TestTree
someErrorInEndpoint =
  testCase "Throwing SomeException" $
    mustThrow @DomainError $ domainHandler (error "Some error")

domainErrorInBecknEndpoint :: TestTree
domainErrorInBecknEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @BecknAPIError $ becknApiHandler (throwM SomeDomainError)

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
