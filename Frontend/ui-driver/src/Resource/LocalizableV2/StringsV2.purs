module Resource.Localizable.StringsV2 where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Locale.Utils
import Language.Types
import Resource.Localizable.TypesV2
import Resource.LocalizableV2.StringsV2Lazy as Lazy

getStringV2 ::
  forall l r_ r.
  Cons l String r_ r =>
  IsSymbol l =>
  Lacks l r_ =>
  Newtype Keymap (Record r) =>
  Proxy l -> String
getStringV2 key = (Lazy.stringsV2Lazy unit).getStringV2 key

getString :: String -> STR -> String
getString language str =  (Lazy.stringsV2Lazy unit).getString language str
