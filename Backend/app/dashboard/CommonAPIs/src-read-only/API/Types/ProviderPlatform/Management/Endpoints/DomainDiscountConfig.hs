{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.ProviderPlatform.Management.Endpoints.DomainDiscountConfig where
import Kernel.Utils.TH
import Data.Aeson
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Dashboard.Common
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Kernel.Types.HideSecrets
import qualified Data.Singletons.TH



data BillingCategory
    = PERSONAL | BUSINESS
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)
instance Kernel.Types.HideSecrets.HideSecrets BillingCategory
    where hideSecrets = Kernel.Prelude.identity
data CreateDomainDiscountConfigReq
    = CreateDomainDiscountConfigReq {domain :: Kernel.Prelude.Text,
                                     billingCategory :: BillingCategory,
                                     vehicleServiceTier :: Dashboard.Common.ServiceTierType,
                                     discountPercentage :: Kernel.Prelude.Double,
                                     enabled :: Kernel.Prelude.Bool}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets CreateDomainDiscountConfigReq
    where hideSecrets = Kernel.Prelude.identity
data DeleteDomainDiscountConfigReq
    = DeleteDomainDiscountConfigReq {domain :: Kernel.Prelude.Text, billingCategory :: BillingCategory, vehicleServiceTier :: Dashboard.Common.ServiceTierType}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets DeleteDomainDiscountConfigReq
    where hideSecrets = Kernel.Prelude.identity
data DomainDiscountConfigRes
    = DomainDiscountConfigRes {domain :: Kernel.Prelude.Text,
                               billingCategory :: BillingCategory,
                               vehicleServiceTier :: Dashboard.Common.ServiceTierType,
                               discountPercentage :: Kernel.Prelude.Double,
                               enabled :: Kernel.Prelude.Bool,
                               createdAt :: Kernel.Prelude.UTCTime,
                               updatedAt :: Kernel.Prelude.UTCTime}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = ("domainDiscountConfig" :> (PostDomainDiscountConfigCreate :<|> GetDomainDiscountConfigList :<|> DeleteDomainDiscountConfigDelete))
type PostDomainDiscountConfigCreate = ("create" :> ReqBody ('[JSON]) CreateDomainDiscountConfigReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
type GetDomainDiscountConfigList = ("list" :> MandatoryQueryParam "billingCategory" BillingCategory :> Get ('[JSON]) [DomainDiscountConfigRes])
type DeleteDomainDiscountConfigDelete = ("delete" :> ReqBody ('[JSON]) DeleteDomainDiscountConfigReq :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess)
data DomainDiscountConfigAPIs
    = DomainDiscountConfigAPIs {postDomainDiscountConfigCreate :: (CreateDomainDiscountConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
                                getDomainDiscountConfigList :: (BillingCategory -> EulerHS.Types.EulerClient [DomainDiscountConfigRes]),
                                deleteDomainDiscountConfigDelete :: (DeleteDomainDiscountConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}
mkDomainDiscountConfigAPIs :: (Client EulerHS.Types.EulerClient API -> DomainDiscountConfigAPIs)
mkDomainDiscountConfigAPIs domainDiscountConfigClient = (DomainDiscountConfigAPIs {..})
                               where postDomainDiscountConfigCreate :<|> getDomainDiscountConfigList :<|> deleteDomainDiscountConfigDelete = domainDiscountConfigClient
data DomainDiscountConfigUserActionType
    = POST_DOMAIN_DISCOUNT_CONFIG_CREATE | GET_DOMAIN_DISCOUNT_CONFIG_LIST | DELETE_DOMAIN_DISCOUNT_CONFIG_DELETE
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''BillingCategory))

$(Data.Singletons.TH.genSingletons [(''DomainDiscountConfigUserActionType)])

