{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Payment where

import qualified Data.Text as T
import qualified Lib.Payment.Storage.Beam.PaymentOrder as BeamPO
import qualified Lib.Payment.Storage.Beam.PaymentOrderOffer as BeamOffer
import qualified Lib.Payment.Storage.Beam.PaymentOrderSplit as BeamSplit
import qualified Lib.Payment.Storage.Beam.PaymentTransaction as BeamPT
import qualified Lib.Payment.Storage.Beam.PayoutOrder as BeamP
import qualified Lib.Payment.Storage.Beam.PayoutStatusHistory as BeamPSH
import qualified Lib.Payment.Storage.Beam.PayoutTransaction as BeamT
import qualified Lib.Payment.Storage.Beam.PersonWallet as BeamPW
import qualified Lib.Payment.Storage.Beam.Refunds as BeamRF
import qualified Lib.Payment.Storage.Beam.WalletRewardPosting as BeamWRP
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName BeamPO.PaymentOrderT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPT.PaymentTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamRF.RefundsT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamP.PayoutOrderT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPSH.PayoutStatusHistoryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamT.PayoutTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamSplit.PaymentOrderSplitT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamOffer.PaymentOrderOfferT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamWRP.WalletRewardPostingT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPW.PersonWalletT where
  schemaName _ = T.pack currentSchemaName
