{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.TrainingFeedback
  ( API,
    handler,
  )
where

import qualified Domain.Action.Dashboard.Driver.TrainingFeedback as DTF
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant

-- ============================================
-- API Type
-- ============================================

type API =
  "training-feedback"
    :> ( ListModulesAPI
           :<|> ModuleFeedbackDetailAPI
           :<|> SummaryAPI
       )

-- GET /training-feedback/modules
type ListModulesAPI =
  "modules"
    :> QueryParam "from" Text
    :> QueryParam "to" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] DTF.TrainingModuleListResponse

-- GET /training-feedback/modules/:moduleId/feedback
type ModuleFeedbackDetailAPI =
  "modules"
    :> Capture "moduleId" Text
    :> "feedback"
    :> QueryParam "from" Text
    :> QueryParam "to" Text
    :> QueryParam "rating" Int
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] DTF.TrainingModuleFeedbackDetailResponse

-- GET /training-feedback/summary
type SummaryAPI =
  "summary"
    :> QueryParam "from" Text
    :> QueryParam "to" Text
    :> Get '[JSON] DTF.TrainingFeedbackSummaryResponse

-- ============================================
-- Handler
-- ============================================

handler :: ShortId merchant -> Id city -> FlowServer API
handler merchantShortId cityId =
  listModules merchantShortId cityId
    :<|> moduleFeedbackDetail merchantShortId cityId
    :<|> feedbackSummary merchantShortId cityId

listModules ::
  ShortId merchant ->
  Id city ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler DTF.TrainingModuleListResponse
listModules merchantShortId cityId mbFrom mbTo mbSortBy mbSortOrder mbLimit mbOffset =
  withFlowHandlerAPI' $
    DTF.listTrainingModuleFeedback
      (cast merchantShortId)
      (cast cityId)
      mbFrom
      mbTo
      mbSortBy
      mbSortOrder
      mbLimit
      mbOffset

moduleFeedbackDetail ::
  ShortId merchant ->
  Id city ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler DTF.TrainingModuleFeedbackDetailResponse
moduleFeedbackDetail merchantShortId cityId moduleId mbFrom mbTo mbRating mbLimit mbOffset =
  withFlowHandlerAPI' $
    DTF.getTrainingModuleFeedbackDetail
      (cast merchantShortId)
      (cast cityId)
      moduleId
      mbFrom
      mbTo
      mbRating
      mbLimit
      mbOffset

feedbackSummary ::
  ShortId merchant ->
  Id city ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler DTF.TrainingFeedbackSummaryResponse
feedbackSummary merchantShortId cityId mbFrom mbTo =
  withFlowHandlerAPI' $
    DTF.getTrainingFeedbackSummary
      (cast merchantShortId)
      (cast cityId)
      mbFrom
      mbTo
