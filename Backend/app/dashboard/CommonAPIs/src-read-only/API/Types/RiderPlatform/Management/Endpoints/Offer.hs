{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Offer where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Offer
import Servant
import Servant.Client

data CreateOfferReq = CreateOfferReq
  { offerCode :: Kernel.Prelude.Text,
    offerType :: Lib.Payment.Domain.Types.Offer.OfferType,
    discountType :: Lib.Payment.Domain.Types.Offer.DiscountType,
    discountValue :: Kernel.Types.Common.HighPrecMoney,
    maxDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsoredBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tnc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    offerEligibilityJsonLogic :: Kernel.Prelude.Maybe Data.Aeson.Value,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateOfferReq where
  hideSecrets = Kernel.Prelude.identity

data OfferEligibilitySchemaResp = OfferEligibilitySchemaResp {defaultValue :: Data.Aeson.Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OfferResp = OfferResp
  { id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer,
    offerCode :: Kernel.Prelude.Text,
    offerType :: Lib.Payment.Domain.Types.Offer.OfferType,
    discountType :: Lib.Payment.Domain.Types.Offer.DiscountType,
    discountValue :: Kernel.Types.Common.HighPrecMoney,
    maxDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsoredBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tnc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    offerEligibilityJsonLogic :: Kernel.Prelude.Maybe Data.Aeson.Value,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    isActive :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateOfferReq = UpdateOfferReq
  { discountValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsoredBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tnc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    offerEligibilityJsonLogic :: Kernel.Prelude.Maybe Data.Aeson.Value,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateOfferReq where
  hideSecrets = Kernel.Prelude.identity

data ValidateOfferEligibilityReq = ValidateOfferEligibilityReq {jsonLogic :: Data.Aeson.Value, inputData :: Data.Aeson.Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ValidateOfferEligibilityReq where
  hideSecrets = Kernel.Prelude.identity

data ValidateOfferEligibilityResp = ValidateOfferEligibilityResp {eligible :: Kernel.Prelude.Bool, errors :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("offer" :> (PostOfferCreate :<|> PostOfferUpdate :<|> GetOfferList :<|> PostOfferToggle :<|> PostOfferValidateEligibility :<|> GetOfferEligibilitySchema))

type PostOfferCreate = ("create" :> ReqBody ('[JSON]) CreateOfferReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostOfferUpdate =
  ( Capture "offerId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer) :> "update" :> ReqBody ('[JSON]) UpdateOfferReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetOfferList = ("list" :> Get ('[JSON]) [OfferResp])

type PostOfferToggle = (Capture "offerId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer) :> "toggle" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostOfferValidateEligibility = ("validateEligibility" :> ReqBody ('[JSON]) ValidateOfferEligibilityReq :> Post ('[JSON]) ValidateOfferEligibilityResp)

type GetOfferEligibilitySchema = ("eligibilitySchema" :> Get ('[JSON]) OfferEligibilitySchemaResp)

data OfferAPIs = OfferAPIs
  { postOfferCreate :: (CreateOfferReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postOfferUpdate :: (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> UpdateOfferReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getOfferList :: (EulerHS.Types.EulerClient [OfferResp]),
    postOfferToggle :: (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postOfferValidateEligibility :: (ValidateOfferEligibilityReq -> EulerHS.Types.EulerClient ValidateOfferEligibilityResp),
    getOfferEligibilitySchema :: (EulerHS.Types.EulerClient OfferEligibilitySchemaResp)
  }

mkOfferAPIs :: (Client EulerHS.Types.EulerClient API -> OfferAPIs)
mkOfferAPIs offerClient = (OfferAPIs {..})
  where
    postOfferCreate :<|> postOfferUpdate :<|> getOfferList :<|> postOfferToggle :<|> postOfferValidateEligibility :<|> getOfferEligibilitySchema = offerClient

data OfferUserActionType
  = POST_OFFER_CREATE
  | POST_OFFER_UPDATE
  | GET_OFFER_LIST
  | POST_OFFER_TOGGLE
  | POST_OFFER_VALIDATE_ELIGIBILITY
  | GET_OFFER_ELIGIBILITY_SCHEMA
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''OfferUserActionType)])
