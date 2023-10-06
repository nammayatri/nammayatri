{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.FarePolicy.FareProductType where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Tools.Beam.UtilsTH as TH

data FareProductType = ONE_WAY | RENTAL | DRIVER_OFFER | ONE_WAY_SPECIAL_ZONE deriving (Generic, Show, Read, Eq, Ord, FromJSON, ToJSON, ToSchema)

derivePersistField "FareProductType"

$(TH.mkBeamInstancesForEnum ''FareProductType)
