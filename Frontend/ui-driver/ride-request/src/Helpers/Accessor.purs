module Helpers.Accessor where

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)

_rideRequestPopUpScreen :: forall a b c. Newtype a { rideRequestPopUpScreen :: b | c } => Lens' a b
_rideRequestPopUpScreen = lens (unwrap >>> _.rideRequestPopUpScreen) (\oldRec newVal -> wrap ((unwrap oldRec) { rideRequestPopUpScreen = newVal }))

_full_address :: forall a b c. Newtype a { full_address :: b | c } => Lens' a b
_full_address = lens (unwrap >>> _.full_address) (\oldRec newVal -> wrap ((unwrap oldRec) { full_address = newVal }))

_area :: forall a b c. Newtype a { area :: b | c } => Lens' a b
_area = lens (unwrap >>> _.area) (\oldRec newVal -> wrap ((unwrap oldRec) { area = newVal }))

_areaCode :: forall a b c. Newtype a { areaCode :: b | c } => Lens' a b
_areaCode = lens (unwrap >>> _.areaCode) (\oldRec newVal -> wrap ((unwrap oldRec) { areaCode = newVal }))