{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.SearchTry where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data RecentSearchTriesReq = RecentSearchTriesReq {phoneNumbers :: [Kernel.Prelude.Text], limit :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RecentSearchTriesReq where
  hideSecrets = Kernel.Prelude.identity

data RecentSearchTriesRes = RecentSearchTriesRes {searchTries :: [RecentSearchTryItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecentSearchTryItem = RecentSearchTryItem
  { phoneNumber :: Kernel.Prelude.Text,
    riderId :: Kernel.Prelude.Text,
    searchRequestId :: Kernel.Prelude.Text,
    searchTryId :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    vehicleServiceTier :: Kernel.Prelude.Text,
    searchRepeatCounter :: Kernel.Prelude.Int,
    searchRepeatType :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("searchTry" :> PostSearchTryRecent)

type PostSearchTryRecent = ("recentSearchTries" :> ReqBody ('[JSON]) RecentSearchTriesReq :> Post ('[JSON]) RecentSearchTriesRes)

newtype SearchTryAPIs = SearchTryAPIs {postSearchTryRecent :: (RecentSearchTriesReq -> EulerHS.Types.EulerClient RecentSearchTriesRes)}

mkSearchTryAPIs :: (Client EulerHS.Types.EulerClient API -> SearchTryAPIs)
mkSearchTryAPIs searchTryClient = (SearchTryAPIs {..})
  where
    postSearchTryRecent = searchTryClient

data SearchTryUserActionType
  = POST_SEARCH_TRY_RECENT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON SearchTryUserActionType where
  toJSON (POST_SEARCH_TRY_RECENT) = Data.Aeson.String "POST_SEARCH_TRY_RECENT"

instance FromJSON SearchTryUserActionType where
  parseJSON (Data.Aeson.String "POST_SEARCH_TRY_RECENT") = pure POST_SEARCH_TRY_RECENT
  parseJSON _ = fail "POST_SEARCH_TRY_RECENT expected"

$(Data.Singletons.TH.genSingletons [(''SearchTryUserActionType)])
