{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.Helpers.Suggestions where

import Data.Array (filter, head, concatMap)
import Prelude ((==))
import Data.Maybe (Maybe(..), fromMaybe)
import JBridge (getSuggestionfromKey, getSuggestionsfromLocal)

suggestionsDefinitions ∷ String -> SuggestionDefinitions
suggestionsDefinitions dummy = [
  {key : "cis1AP", value : getSuggestion "e4feea7ce0053771e08c9bd55c7169f7"},
  {key : "cis2AP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},
  {key : "cis1ds1AP", value :  getSuggestion "eecdfb21ec5e139f8f870f21c7fc7f65"},
  {key : "cis1ds2AP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},
  {key : "cis2ds1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis2ds2AP", value :  getSuggestion "c360639f51cb2625a9439974e865a683"},
  {key : "cis1ds1cs1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis1ds1cs2AP", value :  getSuggestion "c360639f51cb2625a9439974e865a683"},
  {key : "cis1ds2cs1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis1ds2cs2AP", value :  getSuggestion "c360639f51cb2625a9439974e865a683"},
  {key : "cis1BP", value :  getSuggestion "64b6698a5d66fb093bd5a5e7a0475b83"},
  {key : "cis2BP", value :  getSuggestion "58efb0b4f81490a656de99ab10cba628"},
  {key : "cis3BP", value :  getSuggestion "ddf581105f8a03708b2169c6ed526f57"},
  {key : "cis4BP", value :  getSuggestion "0e9fbeec2f63098db38830475f1f58e5"},
  {key : "cis5BP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},
  {key : "cis6BP", value :  getSuggestion "f06fcc3f8c955e54834ab089cebe02cc"},
  {key : "cis7BP", value :  getSuggestion "58efb0b4f81490a656de99ab10cba628"},
  {key : "cis8BP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},
  {key : "cis1ds1BP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "cis1ds2BP", value :  getSuggestion "fc75d7ce2e67cfc2ac9c990e5863b283"},
  {key : "cis1ds3BP", value :  getSuggestion "9659a7b3b6af7274b5968cbfaeda67ab"},
  {key : "cis1ds4BP", value :  getSuggestion "6ee79f64c45f33ce326fc2c7b2c15309"},
  {key : "cis2ds1BP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "cis2ds2BP", value :  getSuggestion "fc75d7ce2e67cfc2ac9c990e5863b283"},
  {key : "cis2ds3BP", value :  getSuggestion "9659a7b3b6af7274b5968cbfaeda67ab"},
  {key : "cis2ds4BP", value :  getSuggestion "6ee79f64c45f33ce326fc2c7b2c15309"},
  {key : "cis3ds1BP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "cis3ds2BP", value :  getSuggestion "fc75d7ce2e67cfc2ac9c990e5863b283"},
  {key : "cis3ds3BP", value :  getSuggestion "9659a7b3b6af7274b5968cbfaeda67ab"},
  {key : "cis3ds4BP", value :  getSuggestion "6ee79f64c45f33ce326fc2c7b2c15309"},
  {key : "cis4ds1BP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "cis4ds2BP", value :  getSuggestion "fc75d7ce2e67cfc2ac9c990e5863b283"},
  {key : "cis4ds3BP", value :  getSuggestion "9659a7b3b6af7274b5968cbfaeda67ab"},
  {key : "cis4ds4BP", value :  getSuggestion "6ee79f64c45f33ce326fc2c7b2c15309"},
  {key : "cis5ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis5ds2BP", value :  getSuggestion "2930acb2ae7ac25629025541fb0d2c72"},
  {key : "cis6ds1BP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "cis6ds2BP", value :  getSuggestion "fc75d7ce2e67cfc2ac9c990e5863b283"},
  {key : "cis6ds3BP", value :  getSuggestion "9659a7b3b6af7274b5968cbfaeda67ab"},
  {key : "cis6ds4BP", value :  getSuggestion "6ee79f64c45f33ce326fc2c7b2c15309"},
  {key : "cis7ds1BP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "cis7ds2BP", value :  getSuggestion "fc75d7ce2e67cfc2ac9c990e5863b283"},
  {key : "cis7ds3BP", value :  getSuggestion "9659a7b3b6af7274b5968cbfaeda67ab"},
  {key : "cis7ds4BP", value :  getSuggestion "6ee79f64c45f33ce326fc2c7b2c15309"},
  {key : "cis8ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis8ds2BP", value :  getSuggestion "c360639f51cb2625a9439974e865a683"},
  {key : "cis1ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis1ds1cs2BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis1ds2cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis1ds2cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis1ds2cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis1ds3cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis1ds3cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis1ds3cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis1ds4cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis1ds4cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis1ds4cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis2ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis2ds1cs2BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis2ds2cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis2ds2cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis2ds2cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis2ds3cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis2ds3cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis2ds3cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis2ds4cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis2ds4cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis2ds4cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis3ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis3ds1cs2BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis3ds2cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis3ds2cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis3ds2cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis3ds3cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis3ds3cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis3ds3cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis3ds4cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis3ds4cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis3ds4cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis4ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis4ds1cs2BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis4ds2cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis4ds2cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis4ds2cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis4ds3cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis4ds3cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis4ds3cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis4ds4cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis4ds4cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis4ds4cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis5ds2cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis5ds2cs2BP", value :  getSuggestion "e68fecabed0bc6a41d16a28b7239d355"},
  {key : "cis6ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis6ds1cs2BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis6ds2cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis6ds2cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis6ds2cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis6ds3cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis6ds3cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis6ds3cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis6ds4cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis6ds4cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis6ds4cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis7ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis7ds1cs2BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis7ds2cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis7ds2cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis7ds2cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis7ds3cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis7ds3cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis7ds3cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis7ds4cs1BP", value :   getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis7ds4cs2BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},
  {key : "cis7ds4cs3BP", value :  getSuggestion "0237462c58acd9b0b2d68af6ba595f36"},
  {key : "cis8ds2cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis8ds2cs2BP", value :  getSuggestion "e68fecabed0bc6a41d16a28b7239d355"},
  {key : "cis1ds2cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis1ds2cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis1ds2cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},
  {key : "cis1ds2cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cis1ds3cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis1ds3cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis1ds3cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis1ds3cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},


  {key : "cis1ds4cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis1ds4cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis1ds4cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis1ds4cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},


  {key : "cis2ds2cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis2ds2cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis2ds2cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis2ds2cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},


  {key : "cis2ds3cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis2ds3cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis2ds3cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis2ds3cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},


  {key : "cis2ds4cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis2ds4cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis2ds4cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis2ds4cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},


  {key : "cis3ds2cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis3ds2cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis3ds2cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis3ds2cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},


  {key : "cis3ds3cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis3ds3cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis3ds3cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis3ds3cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},


  {key : "cis3ds4cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis3ds4cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis3ds4cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis3ds4cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis4ds2cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis4ds2cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis4ds2cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis4ds2cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis4ds3cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis4ds3cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis4ds3cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis4ds3cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis4ds4cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis4ds4cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis4ds4cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},
  
  {key : "cis4ds4cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis5ds2cs2ds1BP", value :  getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis5ds2cs2ds2BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis6ds2cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis6ds2cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis6ds2cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis6ds2cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis6ds3cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis6ds3cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis6ds3cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis6ds3cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis6ds4cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis6ds4cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis6ds4cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis6ds4cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis7ds2cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis7ds2cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis7ds2cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis7ds2cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis7ds3cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis7ds3cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis7ds3cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis7ds3cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis7ds4cs2ds1BP", value :   getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cis7ds4cs2ds2BP", value :   getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis7ds4cs2ds3BP", value :   getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "cis7ds4cs3ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},

  {key : "cis8ds2cs2ds1BP", value :  getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "cis8ds2cs2ds2BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  
  {key : "cds1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cds2AP", value :  getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "cds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "cds2BP", value :  getSuggestion "347953dc18d3e190f655ade6388a7ad2"},
  {key : "cds3BP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},
  
  {key : "dis1AP", value :  getSuggestion "e485f3d2667273e342a65077e7538338"},
  {key : "dis2AP", value :  getSuggestion "4ecdd92ebb79b82cc3b50f4e2e427edf"},
  {key : "dis3AP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},

  {key : "dis1cs1AP", value :  getSuggestion "4fd6f9397027c30a999bec7f5db3373c"},
  {key : "dis1cs2AP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "dis1cs3AP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},

  {key : "dis2cs1AP", value :  getSuggestion "4fd6f9397027c30a999bec7f5db3373c"},
  {key : "dis2cs2AP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "dis2cs3AP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},
  
  {key : "dis3cs1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis3cs2AP", value :  getSuggestion "c360639f51cb2625a9439974e865a683"},

  {key : "dis1cs1ds1AP", value :  getSuggestion "95e946dc1a56fce830c31471435ad95c"},
  {key : "dis1cs1ds2AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs2ds1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs2ds2AP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},
  {key : "dis1cs3ds1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs3ds2AP", value :  getSuggestion "c360639f51cb2625a9439974e865a683"},

  {key : "dis2cs1ds1AP", value :  getSuggestion "95e946dc1a56fce830c31471435ad95c"},
  {key : "dis2cs1ds2AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs2ds1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs2ds2AP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},
  {key : "dis2cs3ds1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs3ds2AP", value :  getSuggestion "c360639f51cb2625a9439974e865a683"},

  {key : "dis1BP", value :  getSuggestion "0b295e1ff354fe09337a4fa1670dd0f5"},
  {key : "dis2BP", value :  getSuggestion "9659a7b3b6af7274b5968cbfaeda67ab"},
  
  {key : "dis1cs1BP", value :  getSuggestion "c1e1159b7e54849e5a625f83dbb34dd8"},
  {key : "dis1cs2BP", value :  getSuggestion "58efb0b4f81490a656de99ab10cba628"},
  {key : "dis1cs3BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},

  {key : "dis2cs1BP", value :  getSuggestion "c1e1159b7e54849e5a625f83dbb34dd8"},
  {key : "dis2cs2BP", value :  getSuggestion "58efb0b4f81490a656de99ab10cba628"},
  {key : "dis2cs3BP", value :  getSuggestion "4602c2fcd1828f1e8f09e89c24fff83b"},

  {key : "dis1cs1ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs2ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs3ds1BP", value :  getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "dis1cs3ds2BP", value :  getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "dis1cs3ds3BP", value :  getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "dis2cs1ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs2ds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs3ds1BP", value :  getSuggestion "b27d2dcad03aafc5ef4ff3488db459fe"},
  {key : "dis2cs3ds2BP", value :  getSuggestion "db9a9c07acc22ba7cfae634454876643"},
  {key : "dis2cs3ds3BP", value :  getSuggestion "08ebb69f0aff8e031b574d73f3bdf5a2"},

  {key : "dis1cs3ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs3ds1cs2BP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},
  {key : "dis1cs3ds2cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs3ds2cs2BP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},
  {key : "dis1cs3ds3cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis1cs3ds3cs2BP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},

  {key : "dis2cs3ds1cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs3ds1cs2BP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},

  {key : "dis2cs3ds2cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs3ds2cs2BP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"}, 

  {key : "dis2cs3ds3cs1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dis2cs3ds3cs2BP", value :  getSuggestion "ee7973cd011e973db10e9d720c91d73d"},
  
  {key : "dds1AP", value :  getSuggestion "4ecdd92ebb79b82cc3b50f4e2e427edf"},
  {key : "dds2AP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},
  
  {key : "dds1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dds2BP", value :  getSuggestion "4c047babe705b8118adde2befb24a3d2"},
  {key : "dds3BP", value :  getSuggestion "728899cf1708c26fec21542cd164fe19"},
  
  {key : "dols1AP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dols2AP", value :  getSuggestion "93cba07454f06a4a960172bbd6e2a435"},
  {key : "dols3AP", value :  getSuggestion "db9a9c07acc22ba7cfae634454876643"},

  {key : "dols1BP", value :  getSuggestion "a60852f204ed8028c1c58808b746d115"},
  {key : "dols2BP", value :  getSuggestion "93cba07454f06a4a960172bbd6e2a435"},
  {key : "dols3BP", value :  getSuggestion "db9a9c07acc22ba7cfae634454876643"}
]

getSuggestions ∷ String -> Suggestions
getSuggestions dummy = [
   {key : "customerInitialAP", value : ["cis1AP", "cis2AP"]},
   {key : "customerInitialBP1", value : ["cis1BP", "cis2BP"]},
   {key : "customerInitialBP2", value : ["cis3BP", "cis4BP", "cis5BP"]},
   {key : "customerInitialBP3", value : ["cis6BP", "cis7BP", "cis8BP"]},
   {key : "cis1AP", value : ["cis1ds1AP", "cis1ds2AP"]},
   {key : "cis2AP", value : ["cis2ds1AP", "cis2ds2AP"]},
   {key : "cis1ds1AP", value : ["cis1ds1cs1AP", "cis1ds1cs2AP"]},
   {key : "cis1ds2AP", value : ["cis1ds2cs1AP", "cis1ds2cs2AP"]},
   {key : "cis1BP", value : ["cis1ds1BP", "cis1ds2BP", "cis1ds3BP", "cis1ds4BP"]},
   {key : "cis2BP", value : ["cis2ds1BP", "cis2ds2BP", "cis2ds3BP", "cis2ds4BP"]},
   {key : "cis3BP", value : ["cis3ds1BP", "cis3ds2BP", "cis3ds3BP", "cis3ds4BP"]},
   {key : "cis4BP", value : ["cis4ds1BP", "cis4ds2BP", "cis4ds3BP", "cis4ds4BP"]},
   
   {key : "cis5BP", value : ["cis5ds1BP", "cis5ds2BP"]},
   {key : "cis6BP", value : ["cis6ds1BP", "cis6ds2BP", "cis6ds3BP", "cis6ds4BP"]},
   {key : "cis7BP", value : ["cis7ds1BP", "cis7ds2BP", "cis7ds3BP", "cis7ds4BP"]},
   {key : "cis8BP", value : ["cis8ds1BP", "cis8ds2BP"]},


   {key : "cis1ds1BP", value : ["cis1ds1cs1BP", "cis1ds1cs2BP"]},
   {key : "cis1ds2BP", value : ["cis1ds2cs1BP", "cis1ds2cs2BP", "cis1ds2cs3BP"]},
   {key : "cis1ds3BP", value : ["cis1ds3cs1BP", "cis1ds3cs2BP", "cis1ds3cs3BP"]},
   {key : "cis1ds4BP", value : ["cis1ds4cs1BP", "cis1ds4cs2BP", "cis1ds4cs3BP"]},
   
   {key : "cis2ds1BP", value : ["cis2ds1cs1BP", "cis2ds1cs2BP"]},
   {key : "cis2ds2BP", value : ["cis2ds2cs1BP", "cis2ds2cs2BP", "cis2ds2cs3BP"]},
   {key : "cis2ds3BP", value : ["cis2ds3cs1BP", "cis2ds3cs2BP", "cis2ds3cs3BP"]},
   {key : "cis2ds4BP", value : ["cis2ds4cs1BP", "cis2ds4cs2BP", "cis2ds4cs3BP"]},
   
   
   {key : "cis3ds1BP", value : ["cis3ds1cs1BP", "cis3ds1cs2BP"]},
   {key : "cis3ds2BP", value : ["cis3ds2cs1BP", "cis3ds2cs2BP", "cis3ds2cs3BP"]},
   {key : "cis3ds3BP", value : ["cis3ds3cs1BP", "cis3ds3cs2BP", "cis3ds3cs3BP"]},
   {key : "cis3ds4BP", value : ["cis3ds4cs1BP", "cis3ds4cs2BP", "cis3ds4cs3BP"]},

   {key : "cis4ds1BP", value : ["cis4ds1cs1BP", "cis4ds1cs2BP"]},
   {key : "cis4ds2BP", value : ["cis4ds2cs1BP", "cis4ds2cs2BP", "cis4ds2cs3BP"]},
   {key : "cis4ds3BP", value : ["cis4ds3cs1BP", "cis4ds3cs2BP", "cis4ds3cs3BP"]},
   {key : "cis4ds4BP", value : ["cis4ds4cs1BP", "cis4ds4cs2BP", "cis4ds4cs3BP"]},
   
   {key : "cis5ds2BP", value : ["cis5ds2cs1BP", "cis5ds2cs2BP"]},

   {key : "cis6ds1BP", value : ["cis6ds1cs1BP", "cis6ds1cs2BP"]},
   {key : "cis6ds2BP", value : ["cis6ds2cs1BP", "cis6ds2cs2BP", "cis6ds2cs3BP"]},
   {key : "cis6ds3BP", value : ["cis6ds3cs1BP", "cis6ds3cs2BP", "cis6ds3cs3BP"]},
   {key : "cis6ds4BP", value : ["cis6ds4cs1BP", "cis6ds4cs2BP", "cis6ds4cs3BP"]},

   {key : "cis7ds1BP", value : ["cis7ds1cs1BP", "cis7ds1cs2BP"]},
   {key : "cis7ds2BP", value : ["cis7ds2cs1BP", "cis7ds2cs2BP", "cis7ds2cs3BP"]},
   {key : "cis7ds3BP", value : ["cis7ds3cs1BP", "cis7ds3cs2BP", "cis7ds3cs3BP"]},
   {key : "cis7ds4BP", value : ["cis7ds4cs1BP", "cis7ds4cs2BP", "cis7ds4cs3BP"]},   

   {key : "cis8ds2BP", value : ["cis8ds2cs1BP", "cis8ds2cs2BP"]},

   {key : "cis1ds2cs2BP", value : ["cis1ds2cs2ds1BP", "cis1ds2cs2ds2BP", "cis1ds2cs2ds3BP"]},
   {key : "cis1ds3cs2BP", value : ["cis1ds3cs2ds1BP", "cis1ds3cs2ds2BP", "cis1ds3cs2ds3BP"]},
   {key : "cis1ds4cs2BP", value : ["cis1ds4cs2ds1BP", "cis1ds4cs2ds2BP", "cis1ds4cs2ds3BP"]},

   {key : "cis2ds2cs2BP", value : ["cis2ds2cs2ds1BP", "cis2ds2cs2ds2BP", "cis2ds2cs2ds3BP"]},
   {key : "cis2ds3cs2BP", value : ["cis2ds3cs2ds1BP", "cis2ds3cs2ds2BP", "cis2ds3cs2ds3BP"]},
   {key : "cis2ds4cs2BP", value : ["cis2ds4cs2ds1BP", "cis2ds4cs2ds2BP", "cis2ds4cs2ds3BP"]},

   {key : "cis3ds2cs2BP", value : ["cis3ds2cs2ds1BP", "cis3ds2cs2ds2BP", "cis3ds2cs2ds3BP"]},
   {key : "cis3ds3cs2BP", value : ["cis3ds3cs2ds1BP", "cis3ds3cs2ds2BP", "cis3ds3cs2ds3BP"]},
   {key : "cis3ds4cs2BP", value : ["cis3ds4cs2ds1BP", "cis3ds4cs2ds2BP", "cis3ds4cs2ds3BP"]},

   {key : "cis4ds2cs2BP", value : ["cis4ds2cs2ds1BP", "cis4ds2cs2ds2BP", "cis4ds2cs2ds3BP"]},
   {key : "cis4ds3cs2BP", value : ["cis4ds3cs2ds1BP", "cis4ds3cs2ds2BP", "cis4ds3cs2ds3BP"]},
   {key : "cis4ds4cs2BP", value : ["cis4ds4cs2ds1BP", "cis4ds4cs2ds2BP", "cis4ds4cs2ds3BP"]},

   {key : "cis5ds2cs2BP", value : ["cis5ds2cs2ds1BP", "cis5ds2cs2ds2BP"]},

   {key : "cis6ds2cs2BP", value : ["cis6ds2cs2ds1BP", "cis6ds2cs2ds2BP", "cis6ds2cs2ds3BP"]},
   {key : "cis6ds3cs2BP", value : ["cis6ds3cs2ds1BP", "cis6ds3cs2ds2BP", "cis6ds3cs2ds3BP"]},
   {key : "cis6ds4cs2BP", value : ["cis6ds4cs2ds1BP", "cis6ds4cs2ds2BP", "cis6ds4cs2ds3BP"]},

   {key : "cis7ds2cs2BP", value : ["cis7ds2cs2ds1BP", "cis7ds2cs2ds2BP", "cis7ds2cs2ds3BP"]},
   {key : "cis7ds3cs2BP", value : ["cis7ds3cs2ds1BP", "cis7ds3cs2ds2BP", "cis7ds3cs2ds3BP"]},
   {key : "cis7ds4cs2BP", value : ["cis7ds4cs2ds1BP", "cis7ds4cs2ds2BP", "cis7ds4cs2ds3BP"]},

   {key : "cis8ds2cs2BP", value : ["cis8ds2cs2ds1BP", "cis8ds2cs2ds2BP"]},
   
   {key : "customerDefaultAP", value : ["cds1AP", "cds2AP"]},
   {key : "customerDefaultBP", value : ["cds1BP", "cds2BP", "cds3BP"]},
   {key : "driverInitialAP", value : ["dis1AP", "dis2AP", "dis3AP"]},
   {key : "driverInitialBP", value : ["dis1BP", "dis2BP"]},
   
   {key : "dis1AP", value : ["dis1cs1AP", "dis1cs2AP", "dis1cs3AP"]},
   {key : "dis2AP", value : ["dis2cs1AP", "dis2cs2AP", "dis2cs3AP"]},
   {key : "dis3AP", value : ["dis3cs1AP", "dis3cs2AP"]},

   {key : "dis1cs1AP", value : ["dis1cs1ds1AP", "dis1cs1ds2AP"]},
   {key : "dis1cs2AP", value : ["dis1cs2ds1AP", "dis1cs2ds2AP"]},
   {key : "dis1cs3AP", value : ["dis1cs3ds1AP", "dis1cs3ds2AP"]},

   {key : "dis2cs1AP", value : ["dis2cs1ds1AP", "dis2cs1ds2AP"]},
   {key : "dis2cs2AP", value : ["dis2cs2ds1AP", "dis2cs2ds2AP"]},
   {key : "dis2cs3AP", value : ["dis2cs3ds1AP", "dis2cs3ds2AP"]},

   {key : "dis1BP", value : ["dis1cs1BP", "dis1cs2BP", "dis1cs3BP"]},
   {key : "dis2BP", value : ["dis2cs1BP", "dis2cs2BP", "dis2cs3BP"]},

   {key : "dis1cs1BP", value : ["dis1cs1ds1BP"]},
   {key : "dis1cs2BP", value : ["dis1cs2ds1BP"]},
   {key : "dis1cs3BP", value : ["dis1cs3ds1BP", "dis1cs3ds2BP", "dis1cs3ds3BP"]},
   {key : "dis2cs1BP", value : ["dis2cs1ds1BP"]},
   {key : "dis2cs2BP", value : ["dis2cs2ds1BP"]},
   {key : "dis2cs3BP", value : ["dis2cs3ds1BP", "dis2cs3ds2BP", "dis2cs3ds3BP"]},
   {key : "driverOverlayDefaultAP", value : ["dols1AP", "dols2AP", "dols3AP"]},
   {key : "driverOverlayDefaultBP", value : ["dols1BP", "dols2BP", "dols3BP"]},
   {key : "driverDefaultAP", value : ["dds1AP", "dds2AP"]},
   {key : "driverDefaultBP", value : ["dds1BP", "dds2BP", "dds3BP"]}
]

type Suggestions = Array
  {
    key :: String,
    value :: Array String
  }

type SuggestionDefinitions = Array
  {
    key :: String,
    value :: {en_us :: String, ta_in :: String, kn_in :: String, hi_in :: String, ml_in :: String, bn_in :: String}
  }

suggestionsList :: SuggestionDefinitions
suggestionsList = [
  {key : "a60852f204ed8028c1c58808b746d115", value :  { en_us : "Ok", ta_in : "சரி", kn_in : "ಸರಿ", hi_in : "ठीक है", ml_in : "ഓക്കേ", bn_in : "ঠিক আছে" }},
  {key : "4c047babe705b8118adde2befb24a3d2", value :  { en_us : "On my way", ta_in : "வந்துகொண்டிருக்கிறேன்", kn_in : "ಬರುತ್ತಿದ್ದೇನೆ", hi_in : "मैं आ रहा हूँ ", ml_in : "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ് ", bn_in : "আমি আসছি" }},
  {key : "728899cf1708c26fec21542cd164fe19", value :  { en_us : "Call me, You're unreachable", ta_in : "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in : "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in : "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in : "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in : "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" }},
  {key : "c360639f51cb2625a9439974e865a683", value :  { en_us : "Please wait", ta_in : "தயவுசெய்து காத்திருக்கவும்", kn_in : "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in : "प्लीज थोड़ी देर रुकिए", ml_in : "ദയവായി കാത്തിരിക്കുക", bn_in : "দয়া করে অপেক্ষা করুন" }},
  {key : "b27d2dcad03aafc5ef4ff3488db459fe", value :  { en_us : "2 mins", ta_in : "2 mins", kn_in : "2 mins", hi_in : "2 mins", ml_in : "2 mins", bn_in : "2 mins" }},
  {key : "db9a9c07acc22ba7cfae634454876643", value :  { en_us : "5 mins", ta_in : "5 mins", kn_in : "5 mins", hi_in : "5 mins", ml_in : "5 mins", bn_in : "5 mins" }},
  {key : "08ebb69f0aff8e031b574d73f3bdf5a2", value :  { en_us : "10 mins", ta_in : "10 mins", kn_in : "10 mins", hi_in : "10 mins", ml_in : "10 mins", bn_in : "10 mins" }},
  {key : "4602c2fcd1828f1e8f09e89c24fff83b", value :  { en_us : "How long?", ta_in : "எவ்ளோ நேரம்?", kn_in : "ಎಷ್ಟು ಹೊತ್ತು?", hi_in : "आप कितना समय लेंगे?", ml_in : "എത്ര സമയം എടുക്കും", bn_in : "কত সময় লাগবে ?" }},
  {key : "0237462c58acd9b0b2d68af6ba595f36", value :  { en_us : "Waiting, Don't cancel", ta_in : "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in : "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in : "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in : "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in : "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" }},
  {key : "fc75d7ce2e67cfc2ac9c990e5863b283", value :  { en_us : "Starting shortly, Please wait", ta_in : "விரைவில் துவங்குவேன். வருகிறேன்", kn_in : "ಪ್ರಾರಂಭಿಸಲಾಗುತ್ತಿದೆ,ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in : "थोड़ी देर में आ रहा हूँ ", ml_in : "ഉടനെ പുറപ്പെടും, ദയവായി കാത്തിരിക്കുക", bn_in : "আমি শীঘ্রই আসছি, দয়া করে অপেক্ষা করুন" }},
  {key : "9659a7b3b6af7274b5968cbfaeda67ab", value :  { en_us : "Road block/Traffic, Please wait", ta_in : "சாலை தடுப்பு/டிராபிக். வருகிறேன்", kn_in : "ರಸ್ತೆ ತಡೆ, ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in : "रास्ता बंद है / ट्रैफिक में फसा हूँ, प्लीज रुकिए", ml_in : "റോഡ് ബ്ലോക്ക്/ട്രാഫിക്, ദയവായി കാത്തിരിക്കുക", bn_in : "রাস্তা ব্লক/ট্রাফিক, দয়া করে অপেক্ষা করুন" }},
  {key : "6ee79f64c45f33ce326fc2c7b2c15309", value :  { en_us : "Finishing a trip, coming soon", ta_in : "மற்றொரு சவாரி முடிக்கிறேன். வருகிறேன்", kn_in : "ಟ್ರಿಪ್ ಮುಗಿಸಲಾಗುತ್ತಿದೆ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in : "एक ट्रिप ख़त्म कर के आ रहा हूँ ", ml_in : "ഒരു ട്രിപ്പ് പൂർത്തിയാക്കുന്നു, ഉടൻ വരുന്നു", bn_in : "একটা ট্রিপ শেষ করে শীঘ্রই আসছি" }},
  {key : "58efb0b4f81490a656de99ab10cba628", value :  { en_us : "Urgent, come soon", ta_in : "அவசரம், சீக்கிரம் வாருங்கள்", kn_in : "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ ", hi_in : "अर्जेंट है प्लीज जल्दी आइये", ml_in : "അത്യാവശ്യമാണ്, വേഗം വരൂ", bn_in : "জরুরী আছে, দয়া করে তাড়াতাড়ি আসুন" }},
  {key : "c1e1159b7e54849e5a625f83dbb34dd8", value :  { en_us : "Ok, I will wait", ta_in : "சரி, நான் காத்திருக்கிறேன்", kn_in : "ಸರಿ ನಾನು ಕಾಯುತ್ತೇನೆ", hi_in : "ठीक है मैं प्रतीक्षा करूंगा", ml_in : "ഓക്കേ, ഞാൻ കാത്തിരിക്കാം", bn_in : "আচ্ছা, আমি অপেক্ষা করবো" }},
  {key : "e4feea7ce0053771e08c9bd55c7169f7", value :  { en_us : "At pick-up, Where are you ?", ta_in : "பிக்-அப்பில் உள்ளேன், நீங்க?", kn_in : "ಪಿಕ್-ಅಪ್‌ನಲ್ಲಿದ್ದೇನೆ  ನೀವು ಎಲ್ಲಿದ್ದೀರಿ?", hi_in : "मैं लोकेशन पे हूं , आप कहा हो ?", ml_in : "ഞാൻ പിക്ക്-അപ്പിൽ ആണ്, താങ്കൾ എവിടെ?", bn_in : "আমি পিক-আপ স্থানে আছি, আপনি কোথায়?"}},
  {key : "4fd6f9397027c30a999bec7f5db3373c", value :  { en_us : "At pick-up", ta_in : "பிக்-அப்பில் உள்ளேன்", kn_in : "ಪಿಕ್ ಅಪ್ ನಲ್ಲಿ ", hi_in : "मैं लोकेशन पे आ गया हूँ ", ml_in : "ഞാൻ പിക്ക്-അപ്പിൽ ആണ്", bn_in : "আমি পিক-আপ স্থানে আছি" }},
  {key : "347953dc18d3e190f655ade6388a7ad2", value :  { en_us : "Don't cancel", ta_in : "ரத்து செய்யாதீர்கள்", kn_in : "ರದ್ದುಗೊಳಿಸಬೇಡಿ", hi_in : "कृपया रद्द न करें", ml_in : "ക്യാൻസൽ ചെയ്യരുത്", bn_in : "ক্যানসেল করবেন না" }},
  {key : "ee7973cd011e973db10e9d720c91d73d", value :  { en_us : "Will wait", ta_in : "காத்திருப்பேன்", kn_in : "ಕಾಯುವೆ", hi_in : "इंतजार करेंगे", ml_in : "കാത്തിരിക്കാം", bn_in : "আমি অপেক্ষা করছি" }},
  {key : "93cba07454f06a4a960172bbd6e2a435", value :  { en_us : "Yes", ta_in : "ஆம்", kn_in : "ಹೌದು", hi_in : "हाँ", ml_in : "അതെ", bn_in : "হ্যাঁ" }},
  {key : "64b6698a5d66fb093bd5a5e7a0475b83", value :  { en_us : "Are you starting ?", ta_in : "நீங்கள் தொடங்குகிறீர்களா?", kn_in : "ನೀವು ಪ್ರಾರಂಭಿಸುತ್ತಿದ್ದೀರಾ?", hi_in : "आप आ रहे है क्या? ", ml_in : "താങ്കൾ പുറപ്പെടുകയാണോ?", bn_in : "আপনি কি আসছেন ?" }},
  {key : "ddf581105f8a03708b2169c6ed526f57", value :  { en_us : "Waiting, will you come?", ta_in : "காத்திருக்கிறேன், வருவீர்களா?", kn_in : "ಕಾಯುತ್ತಿರುವೆ, ನೀವು ಬರುವಿರಾ?", hi_in : "इंतज़ार कर रहा हूँ, आप आयेंगे?", ml_in : "കാത്തിരിക്കുകയാണ്, താങ്കൾ വരുന്നുണ്ടോ", bn_in : "আমি অপেক্ষা করছি, আপনি কি আসছেন?" }},
  {key : "eecdfb21ec5e139f8f870f21c7fc7f65", value :  { en_us : "Waiting for you", ta_in : "உங்களுக்காக காத்திருக்கிறேன்", kn_in : "ನಿಮಗಾಗಿ ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in : "मैं आपका इंतजार कर रहा हूँ ", ml_in : "താങ്കൾക്കായി കാത്തിരിക്കുകയാണ്", bn_in : "আপনার জন্য অপেক্ষা করছি" }},
  {key : "0e9fbeec2f63098db38830475f1f58e5", value :  { en_us : "won't cancel, please come", ta_in : "ரத்து செய்ய மாட்டேன், தயவுசெய்து வாருங்கள்", kn_in : "ರದ್ದು ಮಾಡುವುದಿಲ್ಲ, ದಯವಿಟ್ಟು ಬನ್ನಿ", hi_in : "मैं रद्द नहीं करूँगा, कृपया आएँ", ml_in : "ക്യാൻസൽ ചെയ്യില്ല, ദയവായി വരുക", bn_in : "আমি ক্যানসেল করছি না, আপনি দয়া করে আসুন" }},
  {key : "f06fcc3f8c955e54834ab089cebe02cc", value :  { en_us : "Are you on the way?", ta_in : "நீங்கள் வந்துகொண்டுஇருக்கீர்களா?", kn_in : "ನೀವು ದಾರಿಯಲ್ಲಿದ್ದೀರಾ?", hi_in : "क्या आप रास्ते में हैं?", ml_in : "താങ്കൾ വന്നുകൊണ്ടിരിക്കുകയാണോ", bn_in : "আপনি কি আসছেন?" }},
  {key : "2930acb2ae7ac25629025541fb0d2c72", value :  { en_us : "In traffic, please chat", ta_in : "டிராபிக்-இல் உள்ளேன், தயவுற்று மெசேஜ் செய்யவும்", kn_in : "ಟ್ರಾಫಿಕ್‌, ದಯವಿಟ್ಟು ಚಾಟ್ ಮಾಡಿ", hi_in : "मैं ट्रैफिक में हूं, कृपया चैट करें", ml_in : "ട്രാഫിക്കിലാണ്, ദയവായി ചാറ്റ് ചെയ്യുക", bn_in : "ট্রাফিক আটকে আছি, দয়া করে চ্যাট করুন" }},
  {key : "e68fecabed0bc6a41d16a28b7239d355", value :  { en_us : "Can't chat, please call", ta_in : "மெசேஜ் செய்யமுடியவில்லை தொலைபேசியில் அழைக்கவும்", kn_in : "ಚಾಟ್ ಮಾಡಲು ಸಾಧ್ಯವಿಲ್ಲ, ದಯವಿಟ್ಟು ಕರೆ ಮಾಡಿ", hi_in : "चैट नहीं कर सकते, कृपया कॉल करें", ml_in : "ചാറ്റ് ചെയ്യാനാവില്ല, ദയവായി കാൾചെയ്യൂ", bn_in : "চ্যাট করা যাবে না, কল করুন" }},
  {key : "95e946dc1a56fce830c31471435ad95c", value :  { en_us : "Looking for you", ta_in : "உங்களை தேடுகிறேன்", kn_in : "ನಿಮ್ಮನ್ನೆ  ಹುಡುಕುತ್ತಿರುವೆ", hi_in : "मैं आपको ढूंड रहा हूँ, आप कहा हो ?", ml_in : "താങ്കളെ തിരയുകയാണ്", bn_in : "আপনাকে খুঁজছি, আপনি কোথায় ?" }},
  {key : "e485f3d2667273e342a65077e7538338", value :  { en_us : "I've arrived", ta_in : "நான் வந்துவிட்டேன்", kn_in : "ನಾನು ತಲುಪಿದೆ", hi_in : "मैं लोकेशन पे आ गया हूँ ", ml_in : "ഞാൻ എത്തി", bn_in : "আমি পৌঁছে গেছি" }},
  {key : "4ecdd92ebb79b82cc3b50f4e2e427edf", value :  { en_us : "Waiting, please come", ta_in : "காத்திருக்கிறேன், தயவுசெய்து வாருங்கள்", kn_in : "ಕಾಯುತ್ತಿರುವೆ, ದಯವಿಟ್ಟು ಬನ್ನಿ", hi_in : "इंतजार कर रहे हैं, कृपया आएं", ml_in : "കാത്തിരിക്കുകയാണ്, ദയവായി വരുക", bn_in : "আমি অপেক্ষা করছি, দয়া করে আসুন" }},
  {key : "0b295e1ff354fe09337a4fa1670dd0f5", value :  { en_us : "Starting, don't cancel", ta_in : "நான் தொடங்குகிறேன், ரத்து செய்ய வேண்டாம்", kn_in : "ಪ್ರಾರಂಭಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದುಗೊಳಿಸಬೇಡಿ", hi_in : "मैं शुरू कर रहा हूं, रद्द न करें", ml_in : "തുടങ്ങുകയാണ്, ക്യാന്സലചെയ്യരുത്", bn_in : "আমি শুরু করছি, ক্যানসেল করবেন না" }}
]

getSuggestion :: String -> {en_us :: String, ta_in :: String, kn_in :: String, hi_in :: String, ml_in :: String, bn_in :: String}
getSuggestion key = do
  let message = filter (\item -> item.key == key) suggestionsList
  case (head message) of
    Just record -> record.value
    Nothing ->  {en_us : "", ta_in : "", kn_in : "", hi_in : "", ml_in : "", bn_in : "" }

getMessageFromKey :: String -> String -> String
getMessageFromKey key language = do
  let decodedMessage = (getSuggestionfromKey key language)
  if decodedMessage == "" then do
    let suggestions = (suggestionsDefinitions "")
    let message = filter(\item -> item.key == key) suggestions
    case head message of
      Just value -> case language of
                      "EN_US" -> value.value.en_us
                      "HI_IN" -> value.value.hi_in
                      "KN_IN" -> value.value.kn_in
                      "BN_IN" -> value.value.bn_in
                      "ML_IN" -> value.value.ml_in
                      "TA_IN" -> value.value.ta_in
                      _ -> value.value.en_us
      Nothing -> key
  else decodedMessage

getSuggestionsfromKey :: String -> Array String
getSuggestionsfromKey key = do
  let decodedSuggestions = getSuggestionsfromLocal key
  case (head decodedSuggestions) of
    Just value -> if value == "error" then do
                                            let suggestions = (getSuggestions "")
                                            let replies = filter(\item -> item.key == key) suggestions
                                            concatMap (\item -> item.value) replies
                  else decodedSuggestions
    Nothing -> []
