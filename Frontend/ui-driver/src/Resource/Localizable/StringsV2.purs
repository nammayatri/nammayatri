module Resource.Localizable.StringsV2 where


import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Resource.Localizable.ENv2
import Resource.Localizable.HIv2
import Resource.Localizable.TypesV2
import Locale.Utils


stringsMap :: Languages
stringsMap = Languages
  { english : getEn 
  , hindi : getHi
  }

infixl 1 readFromNT as @~

readFromNT :: forall l a r r_ t
  . Cons l a r_ r
  => IsSymbol l
  => Lacks l r_
  => Newtype t (Record r)
  => t -> (Proxy l) -> a
readFromNT a label = unsafeGet (reflectSymbol label) $ unwrap a

getStringV2 :: forall l r_ r
  . Cons l String r_ r
    => IsSymbol l 
    => Lacks l r_ 
    => Newtype Keymap (Record r)
    => Proxy l -> String
getStringV2 key = 
  let language = getLanguageLocale languageKey
  in  getString language key

getString :: forall l r_ r
  . Cons l String r_ r
  => IsSymbol l
  => Lacks l r_
  => Newtype Keymap (Record r)
  => String -> Proxy l -> String
getString "HI_IN" key = stringsMap @~ hindi @~ key
getString _ key = stringsMap @~ english @~ key