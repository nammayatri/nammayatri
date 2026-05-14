{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MerchantDocument where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified MerchantDocuments.Domain.Types.MerchantDocument
import Servant
import Tools.Auth

data MerchantDocumentListRes = MerchantDocumentListRes {documents :: [MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
