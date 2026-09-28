{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.FRFSTicketService where

import qualified Data.Aeson
import qualified Data.Bifunctor
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import Kernel.Prelude
import Web.HttpApiData (FromHttpApiData (..))

instance FromHttpApiData [Data.Text.Text] where
  parseUrlPiece = parseHeader . Data.Text.Encoding.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = Data.Bifunctor.first Data.Text.pack . Data.Aeson.eitherDecode . Data.ByteString.Lazy.fromStrict
