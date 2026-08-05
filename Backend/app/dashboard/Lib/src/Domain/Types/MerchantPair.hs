{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.MerchantPair where

import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Types.Id

-- One logical merchant as the user understands it (e.g. NAMMA_YATRI), mapped
-- to its per-platform merchant rows (BAP: NAMMA_YATRI, BPP:
-- NAMMA_YATRI_PARTNER). Lets one dashboard token serve both the "bap" and
-- "bpp" route trees on the unified server: when the token's merchant cannot
-- serve the required platform, Tools.Auth.Api resolves the paired partner
-- via this table. A NULL side means the merchant exists on one platform only.
-- Seeded by the Phase 1 merge (dashboard-unification/0006-merchants.sql);
-- empty on the pre-merge per-side schemas, which keeps legacy behavior there.
data MerchantPair = MerchantPair
  { logicalShortId :: Text,
    bapMerchantId :: Maybe (Id DM.Merchant),
    bppMerchantId :: Maybe (Id DM.Merchant),
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
