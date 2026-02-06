{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Yudhishthira.Storage.Beam.BeamFlow where

import Kernel.Beam.Lib.UtilsTH as Reexport
import Kernel.Types.Common as Reexport hiding (id)
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement as BeamADLE
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as BeamADLR
import qualified Lib.Yudhishthira.Storage.Beam.ChakraQueries as BeamCQ
import qualified Lib.Yudhishthira.Storage.Beam.NammaTag as BeamNT
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagTrigger as BeamNTT
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagTriggerV2 as BeamNTTV2
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagV2 as BeamNTV2
import qualified Lib.Yudhishthira.Storage.Beam.TagActionNotificationConfig as BeamTANC
import qualified Lib.Yudhishthira.Storage.Beam.TimeBoundConfig as BeamTMC
import qualified Lib.Yudhishthira.Storage.Beam.UserData as BeamUD

type BeamFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema
  )

type HasYudhishthiraTablesSchema =
  ( HasSchemaName BeamADLR.AppDynamicLogicRolloutT,
    HasSchemaName BeamADLE.AppDynamicLogicElementT,
    HasSchemaName BeamCQ.ChakraQueriesT,
    HasSchemaName BeamNT.NammaTagT,
    HasSchemaName BeamNTT.NammaTagTriggerT,
    HasSchemaName BeamNTTV2.NammaTagTriggerV2T,
    HasSchemaName BeamNTV2.NammaTagV2T,
    HasSchemaName BeamUD.UserDataT,
    HasSchemaName BeamTANC.TagActionNotificationConfigT,
    HasSchemaName BeamTMC.TimeBoundConfigT
  )
