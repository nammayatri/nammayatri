{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.MerchantOnboarding where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.MerchantOnboarding
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

type API = (MerchantOnboardingInfo :<|> MerchantOnboardingStart :<|> MerchantOnboardingList)

type MerchantOnboardingInfo =
  ( "onboarding" :> Capture "onboardingType" Domain.Types.MerchantOnboarding.OnboardingType :> "info" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
  )

type MerchantOnboardingStart =
  ( "onboarding" :> Capture "onboardingType" Domain.Types.MerchantOnboarding.OnboardingType :> "start" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
  )

type MerchantOnboardingList = ("onboarding" :> "list" :> QueryParam "requestorId" Kernel.Prelude.Text :> Get ('[JSON]) [Domain.Types.MerchantOnboarding.MerchantOnboarding])

data MerchantOnboardingAPIs = MerchantOnboardingAPIs
  { merchantOnboardingInfo :: (Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient Domain.Types.MerchantOnboarding.MerchantOnboardingAPI),
    merchantOnboardingStart :: (Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient Domain.Types.MerchantOnboarding.MerchantOnboardingAPI),
    merchantOnboardingList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient [Domain.Types.MerchantOnboarding.MerchantOnboarding])
  }

mkMerchantOnboardingAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantOnboardingAPIs)
mkMerchantOnboardingAPIs merchantOnboardingClient = (MerchantOnboardingAPIs {..})
  where
    merchantOnboardingInfo :<|> merchantOnboardingStart :<|> merchantOnboardingList = merchantOnboardingClient

data MerchantOnboardingUserActionType
  = MERCHANT_ONBOARDING_INFO
  | MERCHANT_ONBOARDING_START
  | MERCHANT_ONBOARDING_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''MerchantOnboardingUserActionType)])
