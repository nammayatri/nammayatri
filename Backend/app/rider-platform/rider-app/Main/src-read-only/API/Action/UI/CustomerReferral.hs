{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CustomerReferral
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CustomerReferral
import qualified Control.Lens
import Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi hiding (description)
import Data.Proxy
import qualified Data.Text as T
import qualified Domain.Action.UI.CustomerReferral as Domain.Action.UI.CustomerReferral
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Error (PersonError (PersonNotFound))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Network.HTTP.Types.Status (status404, status410)
import Servant
import Servant.Exception (Throws, ToServantErr (..))
import Servant.Exception.Server ()
import Servant.OpenApi
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.Error (PersonStatsError (..))

--import qualified Data.Text as T

type API =
  ( Throws MyCustomException :> TokenAuth :> "CustomerRefferal" :> "count" :> Get '[JSON] API.Types.UI.CustomerReferral.ReferredCustomers
      :<|> TokenAuth
      :> "person"
      :> "applyReferral"
      :> ReqBody
           '[JSON]
           API.Types.UI.CustomerReferral.ApplyCodeReq
      :> Post
           '[JSON]
           API.Types.UI.CustomerReferral.ReferrerInfo
      :<|> TokenAuth
      :> "referral"
      :> "verifyVpa"
      :> MandatoryQueryParam
           "vpa"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.CustomerReferral.VpaResp
      :<|> TokenAuth
      :> "referralPayout"
      :> "history"
      :> Get
           '[JSON]
           API.Types.UI.CustomerReferral.PayoutHistory
      :<|> TokenAuth
      :> "payoutVpa"
      :> "upsert"
      :> ReqBody
           '[JSON]
           API.Types.UI.CustomerReferral.UpdatePayoutVpaReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data MyCustomException = PersonError | PersonStatsError
  deriving (Show, Generic)

instance ToJSON MyCustomException

instance Exception MyCustomException

instance ToServantErr MyCustomException where
  status PersonError = status404
  status PersonStatsError = status410 -- FXME

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (Throws r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

data ErrorResponse = ErrorResponse
  { code :: Int,
    errorCode :: Text,
    description :: Text,
    errorMessage :: Text
  }
  deriving (Generic, ToJSON)

personNotFoundError :: ErrorResponse
personNotFoundError =
  ErrorResponse
    { code = 404,
      errorCode = "PERSON_NOT_FOUND",
      description = "PersonNotFound",
      errorMessage = "Person with personId {personId} not found."
    }

personStatsNotFoundError :: ErrorResponse
personStatsNotFoundError =
  ErrorResponse
    { code = 410,
      errorCode = "PERSON_STATS_NOT_FOUND",
      description = "PersonStatsNotFound",
      errorMessage = "Person stats with personId {personId} not found."
    }

instance ToSchema ErrorResponse where
  declareNamedSchema _ = do
    let schem =
          mempty
            & type_ Control.Lens..~ Just OpenApiObject
            & properties
              Control.Lens..~ InsOrd.fromList
                [ ("code", Inline $ toSchema (Proxy :: Proxy Int)),
                  ("errorCode", Inline $ toSchema (Proxy :: Proxy Text)),
                  ("description", Inline $ toSchema (Proxy :: Proxy Text)),
                  ("errorMessage", Inline $ toSchema (Proxy :: Proxy Text))
                ]
            & required Control.Lens..~ ["code", "errorCode", "description", "errorMessage"]
    return $ NamedSchema (Just "ErrorResponse") schem

instance (HasOpenApi api) => HasOpenApi (Throws MyCustomException :> api) where
  toOpenApi _ =
    let apiOpenApi = toOpenApi (Proxy @api)
        errorResponses = [personNotFoundError, personStatsNotFoundError]
        updatedPaths = addErrorResponses (_openApiPaths apiOpenApi) errorResponses
        updatedComponents = addErrorDescriptions (_openApiComponents apiOpenApi) errorResponses
        updatedSchemas = addErrorSchemas updatedComponents
     in apiOpenApi {_openApiPaths = updatedPaths, _openApiComponents = updatedSchemas}

addErrorResponses :: InsOrd.InsOrdHashMap FilePath PathItem -> [ErrorResponse] -> InsOrd.InsOrdHashMap FilePath PathItem
addErrorResponses hashMapPaths resp =
  InsOrd.mapWithKey (addResponses resp) hashMapPaths

addResponses :: [ErrorResponse] -> FilePath -> PathItem -> PathItem
addResponses resp _pathKey pathItem =
  pathItem {_pathItemGet = updateOperation (_pathItemGet pathItem)}
  where
    updateOperation :: Maybe Operation -> Maybe Operation
    updateOperation Nothing = Nothing
    updateOperation (Just op) = Just $ op {_operationResponses = updatedResponses}
      where
        updatedResponses = foldr addResponse (_operationResponses op) resp

addResponse :: ErrorResponse -> Responses -> Responses
addResponse ErrorResponse {..} resp =
  let mediaTypeObject =
        MediaTypeObject
          { _mediaTypeObjectSchema = Just . Ref . Reference $ description,
            _mediaTypeObjectExample = Nothing,
            _mediaTypeObjectExamples = InsOrd.empty,
            _mediaTypeObjectEncoding = InsOrd.empty
          }
      response =
        Inline $
          Response
            { _responseDescription = errorMessage,
              _responseContent =
                InsOrd.fromList
                  [ ( "application/json",
                      mediaTypeObject
                    )
                  ],
              _responseHeaders = InsOrd.empty,
              _responseLinks = InsOrd.empty -- InsOrd.fromList [(description, Ref (Reference description))]
            }
   in resp {_responsesResponses = InsOrd.insert code response (_responsesResponses resp)}

addErrorDescriptions :: Components -> [ErrorResponse] -> Components
addErrorDescriptions comp resp =
  foldr addErrorDescription comp resp

addErrorDescription :: ErrorResponse -> Components -> Components
addErrorDescription ErrorResponse {..} comp =
  let resp = Response description mempty mempty mempty
   in comp {_componentsResponses = InsOrd.insert description resp (_componentsResponses comp)}

errorResponsesLs :: [ErrorResponse]
errorResponsesLs = [personNotFoundError, personStatsNotFoundError]

errorSchemasLs :: [ErrorResponse] -> [(Text, Schema)]
errorSchemasLs = map ((,toSchema (Proxy :: Proxy ErrorResponse)) . description)

addErrorSchemas :: Components -> Components
addErrorSchemas comp =
  let newSchemas = InsOrd.fromList $ errorSchemasLs errorResponsesLs
   in comp {_componentsSchemas = InsOrd.union newSchemas (_componentsSchemas comp)}

handler :: Environment.FlowServer API
handler = getCustomerRefferalCount :<|> postPersonApplyReferral :<|> getReferralVerifyVpa :<|> getReferralPayoutHistory :<|> postPayoutVpaUpsert

getCustomerRefferalCount ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.CustomerReferral.ReferredCustomers
  )
getCustomerRefferalCount a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.getCustomerRefferalCount (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPersonApplyReferral ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CustomerReferral.ApplyCodeReq ->
    Environment.FlowHandler API.Types.UI.CustomerReferral.ReferrerInfo
  )
postPersonApplyReferral a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.postPersonApplyReferral (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getReferralVerifyVpa ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.CustomerReferral.VpaResp
  )
getReferralVerifyVpa a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.getReferralVerifyVpa (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getReferralPayoutHistory :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.CustomerReferral.PayoutHistory)
getReferralPayoutHistory a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.getReferralPayoutHistory (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPayoutVpaUpsert ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CustomerReferral.UpdatePayoutVpaReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPayoutVpaUpsert a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.postPayoutVpaUpsert (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
