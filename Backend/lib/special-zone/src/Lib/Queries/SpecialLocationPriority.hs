{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.SpecialLocationPriority where

import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Lib.Tabular.SpecialLocationPriority
import qualified Lib.Types.SpecialLocationPriority as SpecialLocationPriorityD

findByMerchantOpCityIdAndCategory ::
  Transactionable m =>
  Text ->
  Text ->
  m (Maybe SpecialLocationPriorityD.SpecialLocationPriority)
findByMerchantOpCityIdAndCategory merchantOpCityId category = do
  Esq.findOne $ do
    specialLocationPriority <- from $ table @SpecialLocationPriorityT
    where_ $ specialLocationPriority ^. SpecialLocationPriorityMerchantOperatingCityId ==. val merchantOpCityId
    where_ $ specialLocationPriority ^. SpecialLocationPriorityCategory ==. val category
    return specialLocationPriority
