module Resource.LocalizableV2.StringsV2Lazy where

import Prelude
import Prelude (($))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Language.Types
import Resource.Localizable.TypesV2

foreign import stringsV2Lazy :: Unit -> StringsV2

type StringsV2
  = { getStringV2 ::
        forall l r_ r.
        Cons l String r_ r =>
        IsSymbol l =>
        Lacks l r_ =>
        Newtype Keymap (Record r) =>
        Proxy l -> String
    , getString :: String -> STR -> String
    }
