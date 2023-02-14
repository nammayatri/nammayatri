 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
