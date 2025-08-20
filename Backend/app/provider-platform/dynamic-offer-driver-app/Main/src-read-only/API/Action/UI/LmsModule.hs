{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.LmsModule
  ( API,
    handler,
  )
where

import qualified API.Types.UI.LmsModule
import qualified Control.Lens
import qualified Domain.Action.UI.LmsModule
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleVariant
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
  ( TokenAuth :> "lms" :> "listAllModules" :> QueryParam "language" Kernel.External.Types.Language :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "moduleSection"
           Domain.Types.LmsModule.ModuleSection
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "variant"
           Domain.Types.VehicleVariant.VehicleVariant
      :> Get
           '[JSON]
           API.Types.UI.LmsModule.LmsGetModuleRes
      :<|> TokenAuth
      :> "lms"
      :> Capture
           "moduleId"
           (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule)
      :> "listAllVideos"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get
           '[JSON]
           API.Types.UI.LmsModule.LmsGetVideosRes
      :<|> TokenAuth
      :> "lms"
      :> Capture
           "moduleId"
           (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule)
      :> "listAllQuiz"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get
           '[JSON]
           API.Types.UI.LmsModule.LmsGetQuizRes
      :<|> TokenAuth
      :> "lms"
      :> Capture
           "moduleId"
           (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule)
      :> "getCertificate"
      :> Get
           '[JSON]
           API.Types.UI.LmsModule.LmsCertificateRes
      :<|> TokenAuth
      :> "lms"
      :> "getAllCertificates"
      :> Get
           '[JSON]
           [API.Types.UI.LmsModule.CertificateInfo]
      :<|> TokenAuth
      :> "lms"
      :> Capture
           "moduleId"
           (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule)
      :> "getBonusCoins"
      :> Get
           '[JSON]
           API.Types.UI.LmsModule.BonusRes
      :<|> TokenAuth
      :> "lms"
      :> "markVideoAsStarted"
      :> ReqBody
           '[JSON]
           API.Types.UI.LmsModule.VideoUpdateAPIReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "lms"
      :> "markVideoAsCompleted"
      :> ReqBody
           '[JSON]
           API.Types.UI.LmsModule.VideoUpdateAPIReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "lms"
      :> "question"
      :> "confirm"
      :> ReqBody
           '[JSON]
           API.Types.UI.LmsModule.QuestionConfirmReq
      :> Post
           '[JSON]
           API.Types.UI.LmsModule.QuestionConfirmRes
  )

handler :: Environment.FlowServer API
handler = getLmsListAllModules :<|> getLmsListAllVideos :<|> getLmsListAllQuiz :<|> getLmsGetCertificate :<|> getLmsGetAllCertificates :<|> getLmsGetBonusCoins :<|> postLmsMarkVideoAsStarted :<|> postLmsMarkVideoAsCompleted :<|> postLmsQuestionConfirm

getLmsListAllModules ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.External.Types.Language ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Domain.Types.LmsModule.ModuleSection ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant ->
    Environment.FlowHandler API.Types.UI.LmsModule.LmsGetModuleRes
  )
getLmsListAllModules a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsListAllModules (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1

getLmsListAllVideos ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule ->
    Kernel.Prelude.Maybe Kernel.External.Types.Language ->
    Environment.FlowHandler API.Types.UI.LmsModule.LmsGetVideosRes
  )
getLmsListAllVideos a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsListAllVideos (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getLmsListAllQuiz ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule ->
    Kernel.Prelude.Maybe Kernel.External.Types.Language ->
    Environment.FlowHandler API.Types.UI.LmsModule.LmsGetQuizRes
  )
getLmsListAllQuiz a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsListAllQuiz (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getLmsGetCertificate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule ->
    Environment.FlowHandler API.Types.UI.LmsModule.LmsCertificateRes
  )
getLmsGetCertificate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsGetCertificate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getLmsGetAllCertificates ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler [API.Types.UI.LmsModule.CertificateInfo]
  )
getLmsGetAllCertificates a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsGetAllCertificates (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getLmsGetBonusCoins ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule ->
    Environment.FlowHandler API.Types.UI.LmsModule.BonusRes
  )
getLmsGetBonusCoins a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.getLmsGetBonusCoins (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postLmsMarkVideoAsStarted ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.LmsModule.VideoUpdateAPIReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postLmsMarkVideoAsStarted a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.postLmsMarkVideoAsStarted (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postLmsMarkVideoAsCompleted ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.LmsModule.VideoUpdateAPIReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postLmsMarkVideoAsCompleted a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.postLmsMarkVideoAsCompleted (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postLmsQuestionConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.LmsModule.QuestionConfirmReq ->
    Environment.FlowHandler API.Types.UI.LmsModule.QuestionConfirmRes
  )
postLmsQuestionConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.LmsModule.postLmsQuestionConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
