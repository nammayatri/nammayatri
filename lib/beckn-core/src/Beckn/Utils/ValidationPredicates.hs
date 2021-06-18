{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Beckn.Utils.ValidationPredicates where

import Beckn.Types.Validation.Predicate
import qualified Beckn.Types.Validation.Regex as R

mobileNumber = ExactLength 10 `And` R.Many (R.Ch R.digit)

mobileCountryCode = LengthInRange 2 4 `And` R.Seq [R.Ch (R.ExactChar '+'), R.Many (R.Ch R.digit)]

name = R.Many (R.Any R.latinOrSpace)
