module HSpec
  ( module HSpec,
    module H,
  )
where

import EulerHS.Prelude
import Test.HUnit.Base (assertFailure)
import Test.Hspec as H hiding
  ( expectationFailure,
    shouldBe,
    shouldContain,
    shouldEndWith,
    shouldMatchList,
    shouldNotBe,
    shouldNotContain,
    shouldNotReturn,
    shouldNotSatisfy,
    shouldReturn,
    shouldSatisfy,
    shouldStartWith,
  )
import Test.Hspec.Expectations.Lifted as H hiding (expectationFailure)

expectationFailure :: (MonadIO m, HasCallStack) => String -> m a
expectationFailure = liftIO . assertFailure
