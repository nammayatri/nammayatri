{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Translation where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data TranslationDimension = TranslationDimension {name :: Kernel.Prelude.Text, values :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationDomainSummary = TranslationDomainSummary
  { domain :: Kernel.Prelude.Text,
    fields :: [Kernel.Prelude.Text],
    dimensions :: [TranslationDimension],
    keyCount :: Kernel.Prelude.Int,
    textCount :: Kernel.Prelude.Int,
    filledTexts :: [TranslationLanguageCount]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationDomainsResp = TranslationDomainsResp {domains :: [TranslationDomainSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationKeyEntry = TranslationKeyEntry {key :: Kernel.Prelude.Text, legacyKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text, texts :: [TranslationText]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationKeysResp = TranslationKeysResp {domain :: Kernel.Prelude.Text, fields :: [Kernel.Prelude.Text], dimensions :: [TranslationDimension], entries :: [TranslationKeyEntry]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationLanguageCount = TranslationLanguageCount {language :: Kernel.External.Types.Language, count :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationText = TranslationText
  { language :: Kernel.External.Types.Language,
    field :: Kernel.Prelude.Text,
    variant :: [(Kernel.Prelude.Text, Kernel.Prelude.Text)],
    text :: Kernel.Prelude.Text,
    source :: TranslationTextSource
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationTextSource
  = City
  | Shared
  | Legacy
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationUpsertEntry = TranslationUpsertEntry
  { key :: Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    field :: Kernel.Prelude.Text,
    variant :: Kernel.Prelude.Maybe [(Kernel.Prelude.Text, Kernel.Prelude.Text)],
    text :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationUpsertFailure = TranslationUpsertFailure
  { key :: Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    field :: Kernel.Prelude.Text,
    variant :: Kernel.Prelude.Maybe [(Kernel.Prelude.Text, Kernel.Prelude.Text)],
    reason :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TranslationUpsertReq = TranslationUpsertReq {entries :: [TranslationUpsertEntry]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TranslationUpsertReq where
  hideSecrets = Kernel.Prelude.identity

data TranslationUpsertRes = TranslationUpsertRes {saved :: Kernel.Prelude.Int, failed :: [TranslationUpsertFailure]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = "translation" :> (GetTranslationDomains :<|> (GetTranslationKeys :<|> PostTranslationUpsert))

type GetTranslationDomains = "domains" :> Get '[JSON] TranslationDomainsResp

type GetTranslationKeys = Capture "domain" Kernel.Prelude.Text :> ("keys" :> Get '[JSON] TranslationKeysResp)

type PostTranslationUpsert = Capture "domain" Kernel.Prelude.Text :> ("upsert" :> (ReqBody '[JSON] TranslationUpsertReq :> Post '[JSON] TranslationUpsertRes))

data TranslationAPIs = TranslationAPIs
  { getTranslationDomains :: EulerHS.Types.EulerClient TranslationDomainsResp,
    getTranslationKeys :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient TranslationKeysResp,
    postTranslationUpsert :: Kernel.Prelude.Text -> (TranslationUpsertReq -> EulerHS.Types.EulerClient TranslationUpsertRes)
  }

mkTranslationAPIs :: Client EulerHS.Types.EulerClient API -> TranslationAPIs
mkTranslationAPIs translationClient = (TranslationAPIs {..})
  where
    getTranslationDomains :<|> getTranslationKeys :<|> postTranslationUpsert = translationClient

data TranslationUserActionType
  = GET_TRANSLATION_DOMAINS
  | GET_TRANSLATION_KEYS
  | POST_TRANSLATION_UPSERT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''TranslationUserActionType])
