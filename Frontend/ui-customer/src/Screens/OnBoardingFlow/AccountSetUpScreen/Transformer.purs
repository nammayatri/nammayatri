module Screens.AccountSetUpScreen.Transformer where

import Prelude
import Data.Array (foldl)
import Screens.Types (DisabilityT)
import Services.API (Disability(..))

getDisabilityList :: Array Disability -> Array DisabilityT
getDisabilityList = foldl (\acc (Disability item) -> acc <> [ item ]) dummyDisability

dummyDisability :: Array DisabilityT
dummyDisability = []
