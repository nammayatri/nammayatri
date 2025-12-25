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
  { fromStationId :: Text,
    toStationId :: Text,
    providerId :: Text,
    quoteType :: Quote.FRFSQuoteType
  }

makeFRFSCachedQuoteKey :: FRFSCachedQuoteKey -> Text
makeFRFSCachedQuoteKey frfsCachedQuoteKey = "FRFSCachedQuote:FromStationId:" <> frfsCachedQuoteKey.fromStationId <> ":ToStationId:" <> frfsCachedQuoteKey.toStationId <> ":ProviderId:" <> frfsCachedQuoteKey.providerId <> ":QuoteType:" <> show frfsCachedQuoteKey.quoteType

findByFRFSCachedQuoteKey :: (CacheFlow m r, EsqDBFlow m r) => FRFSCachedQuoteKey -> m (Maybe FRFSCachedQuote)
findByFRFSCachedQuoteKey frfsCachedQuoteKey = do
  let key = makeFRFSCachedQuoteKey frfsCachedQuoteKey
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> return Nothing

cacheByFRFSCachedQuoteKey :: (CacheFlow m r) => FRFSCachedQuoteKey -> FRFSCachedQuote -> m ()
cacheByFRFSCachedQuoteKey frfsCachedQuoteKey cachedQuote = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeFRFSCachedQuoteKey frfsCachedQuoteKey
  Hedis.setExp key cachedQuote expTime
