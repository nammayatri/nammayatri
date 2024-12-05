{-
	Copyright 2022-23, Juspay India Pvt Ltd

	This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

	as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

	is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

	or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Common.Cac.KeyNameConstants where

import Kernel.Prelude
import Kernel.Types.Id

data CacKeyType = Transaction | Driver deriving (Eq)

data CacKey
  = TransactionId (Id 'Transaction)
  | DriverId (Id 'Driver)
  deriving (Eq, Show)

getKeyName :: CacKey -> Text
getKeyName (TransactionId _) = "transactionId"
getKeyName (DriverId _) = "driverId"

getKeyValue :: CacKey -> Text
getKeyValue (TransactionId x) = x.getId
getKeyValue (DriverId x) = x.getId
