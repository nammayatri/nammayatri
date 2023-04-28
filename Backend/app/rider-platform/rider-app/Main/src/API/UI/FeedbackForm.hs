{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.FeedbackForm
  ( API,
    handler,
  )
where

import Beckn.Types.Core.Taxi.Rating.Category (CategoryName)
import qualified Domain.Action.UI.GetFeedbackForm as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Environment as App
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.GetFeedbackForm as GFF
import Tools.Auth

-------- Feedback form Flow ----------
type API =
  "feedback"
    :> ( "form"
           :> TokenAuth
           :> MandatoryQueryParam "ratingValue" Int
           :> MandatoryQueryParam "categoryName" CategoryName
           :> Get '[JSON] GFF.FeedbackFormResp
       )

handler :: App.FlowServer API
handler = feedbackForm

feedbackForm :: (Id Person.Person, Id DM.Merchant) -> Int -> CategoryName -> App.FlowHandler GFF.FeedbackFormResp
feedbackForm (personId, _) ratingValue categoryName = withFlowHandlerAPI . withPersonIdLogTag personId $ Domain.getFeedbackForm personId ratingValue categoryName
