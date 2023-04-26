{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.GetRatingCategories (API, handler) where

import qualified Beckn.ACL.GetRatingCategories as ACL
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  SignatureAuth "Authorization"
    :> Rating.GetRatingCategoriesAPI

handler :: FlowServer API
handler = getRatingCategories

getRatingCategories ::
  SignatureAuthResult ->
  Rating.GetRatingCategoriesReq ->
  FlowHandler Rating.GetRatingCategoriesResp
getRatingCategories (SignatureAuthResult _ subscriber) req = withFlowHandlerAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "getRatingCategoriesAPI" "Received get_rating_categories API call."
    ACL.buildRatingCategoriesResp subscriber req
