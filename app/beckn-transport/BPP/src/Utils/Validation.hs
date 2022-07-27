module Utils.Validation
  ( module Utils.Validation,
    module Beckn.Types.Validation,
  )
where

import Beckn.Types.Validation
import qualified Data.Either.Validation as V
import Data.Generics.Labels ()
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (pred)

validateDiscountDate ::
  Text ->
  UTCTime ->
  UTCTime ->
  Validation
validateDiscountDate fieldName toDate fromDate =
  unless (toDate >= fromDate) (V.Failure [validationDescription])
  where
    validationDescription =
      ValidationDescription
        { fieldName = [fieldName],
          expectation = "toDate should be greater or equal to fromDate."
        }
