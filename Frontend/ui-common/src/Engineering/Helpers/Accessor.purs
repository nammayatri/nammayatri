module Engineering.Helpers.Accessor where

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Maybe (Maybe)

_title :: forall a b c. Newtype a {title :: b | c } => Lens' a b
_title = lens (unwrap >>> _.title) (\oldRec newVal -> wrap ((unwrap oldRec) { title = newVal }))

_priceWithCurrency :: forall a b c. Newtype a {priceWithCurrency :: b | c } => Lens' a b
_priceWithCurrency = lens (unwrap >>> _.priceWithCurrency) (\oldRec newVal -> wrap ((unwrap oldRec) { priceWithCurrency = newVal }))