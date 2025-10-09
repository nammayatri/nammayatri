{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.Storage.Beam.BeamFlow where

import Kernel.Beam.Lib.UtilsTH as Reexport
import Kernel.Types.Common as Reexport hiding (id)
import Kernel.Utils.Common
import qualified Lib.Payment.Storage.Beam.PaymentOrder as BeamPO
import qualified Lib.Payment.Storage.Beam.PaymentOrderOffer as BeamOffer
import qualified Lib.Payment.Storage.Beam.PaymentOrderSplit as BeamPOS
import qualified Lib.Payment.Storage.Beam.PaymentTransaction as BeamPT
import qualified Lib.Payment.Storage.Beam.PayoutOrder as BeamPOO
import qualified Lib.Payment.Storage.Beam.PayoutTransaction as BeamPOT
import qualified Lib.Payment.Storage.Beam.Refunds as BeamRF

type BeamFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasSchemaName BeamPO.PaymentOrderT,
    HasSchemaName BeamPT.PaymentTransactionT,
    HasSchemaName BeamRF.RefundsT,
    HasSchemaName BeamPOO.PayoutOrderT,
    HasSchemaName BeamPOT.PayoutTransactionT,
    HasSchemaName BeamPOS.PaymentOrderSplitT,
    HasSchemaName BeamOffer.PaymentOrderOfferT
  )
