{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy.DriverExtraFeeBounds where

import Data.Text as Text
import qualified Domain.Types.FarePolicy as DFP
import Kernel.Beam.Functions (FromCacType (..))
import qualified Kernel.Beam.Functions as KBF
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as BeamDEFB
import Storage.Queries.FarePolicy.DriverExtraFeeBounds ()
import Utils.Common.CacUtils

getDriverExtraFeeBoundsFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id DFP.FarePolicy -> Int -> m [DFP.FullDriverExtraFeeBounds]
getDriverExtraFeeBoundsFromCAC context tenant id toss = do
  res :: (Maybe [BeamDEFB.DriverExtraFeeBounds]) <- getConfigListFromCac context tenant toss FarePolicyDriverExtraFeeBounds (Text.unpack id.getId)
  config <- mapM KBF.fromCacType (fromMaybe [] res)
  pure $ catMaybes config

instance FromCacType BeamDEFB.DriverExtraFeeBounds DFP.FullDriverExtraFeeBounds where
  fromCacType = KBF.fromTType'
