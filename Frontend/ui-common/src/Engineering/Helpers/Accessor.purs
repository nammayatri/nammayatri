{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Common.Accessor where

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Maybe (Maybe)

_major :: forall a b c. Newtype a { major :: b | c } => Lens' a b
_major = lens (unwrap >>> _.major) (\oldRec newVal -> wrap ((unwrap oldRec) { major = newVal }))

_minor :: forall a b c. Newtype a { minor :: b | c } => Lens' a b
_minor = lens (unwrap >>> _.minor) (\oldRec newVal -> wrap ((unwrap oldRec) { minor = newVal }))

_maintenance :: forall a b c. Newtype a { maintenance :: b | c } => Lens' a b
_maintenance = lens (unwrap >>> _.maintenance) (\oldRec newVal -> wrap ((unwrap oldRec) { maintenance = newVal }))
