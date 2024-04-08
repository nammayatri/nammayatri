module Helpers.Accessor where

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)

_rideRequestPopUpScreen :: forall a b c. Newtype a { rideRequestPopUpScreen :: b | c } => Lens' a b
_rideRequestPopUpScreen = lens (unwrap >>> _.rideRequestPopUpScreen) (\oldRec newVal -> wrap ((unwrap oldRec) { rideRequestPopUpScreen = newVal }))
