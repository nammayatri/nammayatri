{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.LmsModule where

import qualified API.Types.UI.LmsModule
import qualified Control.Lens
import qualified Domain.Action.UI.LmsModule as Domain.Action.UI.LmsModule
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle.Variant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth :> "lms" :> "listAllModules" :> QueryParam "language" (Kernel.External.Types.Language) :> QueryParam "limit" (Kernel.Prelude.Int) :> QueryParam "offset" (Kernel.Prelude.Int) :> QueryParam "variant" (Domain.Types.Vehicle.Variant.Variant) :> Get '[JSON] API.Types.UI.LmsModule.LmsGetModuleRes
    :<|> TokenAuth :> "lms" :> Capture "moduleId" (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule) :> "listAllVideos" :> QueryParam "language" (Kernel.External.Types.Language) :> Get '[JSON] API.Types.UI.LmsModule.LmsGetVideosRes
    :<|> TokenAuth :> "lms" :> Capture "moduleId" (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule) :> "listAllQuiz" :> QueryParam "language" (Kernel.External.Types.Language) :> Get '[JSON] [API.Types.UI.LmsModule.LmsQuestionRes]
    :<|> TokenAuth :> "lms" :> "markVideoAsStarted" :> ReqBody '[JSON] API.Types.UI.LmsModule.VideoUpdateAPIReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
    :<|> TokenAuth :> "lms" :> "markVideoAsCompleted" :> ReqBody '[JSON] API.Types.UI.LmsModule.VideoUpdateAPIReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
    :<|> TokenAuth :> "lms" :> "question" :> "confirm" :> ReqBody '[JSON] API.Types.UI.LmsModule.QuestionConfirmReq :> Post '[JSON] API.Types.UI.LmsModule.QuestionConfirmRes

handler :: Environment.FlowServer API
handler =
  getLmsListAllModules
    :<|> getLmsListAllVideos
    :<|> getLmsListAllQuiz
    :<|> postLmsMarkVideoAsStarted
    :<|> postLmsMarkVideoAsCompleted
    :<|> postLmsQuestionConfirm

getLmsListAllModules :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.Vehicle.Variant.Variant) -> Environment.FlowHandler API.Types.UI.LmsModule.LmsGetModuleRes
getLmsListAllModules a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsListAllModules (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getLmsListAllVideos :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.FlowHandler API.Types.UI.LmsModule.LmsGetVideosRes
getLmsListAllVideos a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsListAllVideos (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getLmsListAllQuiz :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.FlowHandler [API.Types.UI.LmsModule.LmsQuestionRes]
getLmsListAllQuiz a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsListAllQuiz (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postLmsMarkVideoAsStarted :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postLmsMarkVideoAsStarted a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.postLmsMarkVideoAsStarted (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postLmsMarkVideoAsCompleted :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.LmsModule.VideoUpdateAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postLmsMarkVideoAsCompleted a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.postLmsMarkVideoAsCompleted (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postLmsQuestionConfirm :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.LmsModule.QuestionConfirmReq -> Environment.FlowHandler API.Types.UI.LmsModule.QuestionConfirmRes
postLmsQuestionConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.postLmsQuestionConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
