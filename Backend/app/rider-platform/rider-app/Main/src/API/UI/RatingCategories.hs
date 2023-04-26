{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.RatingCategories
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.GetRatingCategories as Domain
import qualified Domain.Types.Person as Person
import qualified Environment as App
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.GetRatingCategories as GRC
import Tools.Auth

-------- Rating Categories Flow ----------
type API =
  "rating"
    :> ( "categories"
           :> TokenAuth
           :> Get '[JSON] GRC.RatingCategoriesResp
       )

handler :: App.FlowServer API
handler = ratingCategories

ratingCategories :: Id Person.Person -> App.FlowHandler GRC.RatingCategoriesResp
ratingCategories personId = withFlowHandlerAPI . withPersonIdLogTag personId $ Domain.getRatingCategories personId
