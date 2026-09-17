module Engineering.Helpers.Accessor where

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Maybe (Maybe)

_title :: forall a b c. Newtype a {title :: b | c } => Lens' a b
_title = lens (unwrap >>> _.title) (\oldRec newVal -> wrap ((unwrap oldRec) { title = newVal }))

_priceWithCurrency :: forall a b c. Newtype a {priceWithCurrency :: b | c } => Lens' a b
_priceWithCurrency = lens (unwrap >>> _.priceWithCurrency) (\oldRec newVal -> wrap ((unwrap oldRec) { priceWithCurrency = newVal }))

_payload :: forall a b c. Newtype a { payload :: b | c } => Lens' a b
_payload = lens (unwrap >>> _.payload) (\oldRec newVal -> wrap ((unwrap oldRec) { payload = newVal }))

_fragmentViewGroups :: forall a b c. Newtype a { fragmentViewGroups :: b | c } => Lens' a b
_fragmentViewGroups = lens (unwrap >>> _.fragmentViewGroups) (\oldRec newVal -> wrap ((unwrap oldRec) { fragmentViewGroups = newVal }))

_main :: forall a b c. Newtype a { main :: b | c } => Lens' a b
_main = lens (unwrap >>> _.main) (\oldRec newVal -> wrap ((unwrap oldRec) { main = newVal }))

_view_param :: forall a b c. Newtype a { view_param :: b | c } => Lens' a b
_view_param = lens (unwrap >>> _.view_param) (\oldRec newVal -> wrap ((unwrap oldRec) { view_param = newVal }))

_deepLinkJSON :: forall a b c. Newtype a { deepLinkJSON :: b | c } => Lens' a b
_deepLinkJSON = lens (unwrap >>> _.deepLinkJSON) (\oldRec newVal -> wrap ((unwrap oldRec) { deepLinkJSON = newVal }))