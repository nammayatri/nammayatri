{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RatingCategories where

import Beckn.Types.Core.Taxi.Rating.Category
import qualified Domain.Types.RatingCategories as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.RatingCategories

findAll :: Transactionable m => m [Domain.RatingCategory]
findAll = Esq.findAll $ from $ table @RatingCategoryT

findByCategoryName :: Transactionable m => CategoryName -> m (Maybe (Id Domain.RatingCategory))
findByCategoryName name = Esq.findOne $ do
  categoryT <- from $ table @RatingCategoryT
  where_ $
    categoryT ^. RatingCategoryCategory ==. val name
  return $ categoryT ^. RatingCategoryTId
