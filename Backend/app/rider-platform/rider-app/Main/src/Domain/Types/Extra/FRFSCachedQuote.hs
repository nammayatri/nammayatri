{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.FRFSCachedQuote where

import Data.Aeson
import qualified Domain.Types.FRFSQuote as Quote
import Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

data FRFSCachedQuote = FRFSCachedQuote
  { price :: Price,
    stationsJson :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data FRFSCachedQuoteKey = FRFSCachedQuoteKey
  { fromStationId :: Id Station,
    toStationId :: Id Station,
    providerId :: Text,
    quoteType :: Quote.FRFSQuoteType
  }

makeFromToProviderIdAndQuoteTypeKey :: FRFSCachedQuoteKey -> Text
makeFromToProviderIdAndQuoteTypeKey frfsCachedQuoteKey = "FRFSCachedQuote:FromStationId:" <> frfsCachedQuoteKey.fromStationId.getId <> ":ToStationId:" <> frfsCachedQuoteKey.toStationId.getId <> ":ProviderId:" <> frfsCachedQuoteKey.providerId <> ":QuoteType:" <> show frfsCachedQuoteKey.quoteType

findCachedQuoteByFromToProviderIdAndQuoteType :: (CacheFlow m r, EsqDBFlow m r) => FRFSCachedQuoteKey -> m (Maybe FRFSCachedQuote)
findCachedQuoteByFromToProviderIdAndQuoteType frfsCachedQuoteKey = do
  let key = makeFromToProviderIdAndQuoteTypeKey frfsCachedQuoteKey
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> return Nothing

cacheByFromToProviderIdAndQuoteType :: (CacheFlow m r) => FRFSCachedQuoteKey -> FRFSCachedQuote -> m ()
cacheByFromToProviderIdAndQuoteType frfsCachedQuoteKey cachedQuote = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeFromToProviderIdAndQuoteTypeKey frfsCachedQuoteKey
  Hedis.setExp key cachedQuote expTime