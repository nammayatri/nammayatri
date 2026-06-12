{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.InvoiceTemplate
  ( findByMerchantOpCityIdInvoiceTypeAndLanguage,
    clearCache,
    clearCacheById,
  )
where

import qualified "beckn-spec" Domain.Types.Invoice
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.InvoiceTemplate as FInvoiceTemplate
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Queries.InvoiceTemplate as Queries
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Types as LYT
import qualified Tools.DynamicLogic as DynamicLogic

makeCacheKey :: Id DMOC.MerchantOperatingCity -> LYT.InvoiceTemplateScope -> Kernel.External.Types.Language -> Text
makeCacheKey mocId scope lang =
  "CachedQueries:InvoiceTemplate:MOCID-" <> mocId.getId <> ":Scope-" <> show scope <> ":Lang-" <> show lang

-- | Lookup with Specific → Generic fallback, then an ENGLISH retry for
-- unseeded languages. Each step is cached under its own key.
findByMerchantOpCityIdInvoiceTypeAndLanguage ::
  (CacheFlow m r, EsqDBFlow m r, Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r, Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Domain.Types.Invoice.InvoiceType ->
  Kernel.External.Types.Language ->
  Maybe [LYT.ConfigVersionMap] ->
  m (Maybe FInvoiceTemplate.InvoiceTemplate)
findByMerchantOpCityIdInvoiceTypeAndLanguage mocId mbInvoiceType lang mbConfigVersionMap = do
  result <- lookupForLang lang
  case result of
    Just a -> return $ Just a
    Nothing
      | lang /= Kernel.External.Types.ENGLISH -> lookupForLang Kernel.External.Types.ENGLISH
      | otherwise -> return Nothing
  where
    lookupForLang lang' = do
      let scope = maybe LYT.InvoiceTypeGeneric LYT.InvoiceTypeSpecific mbInvoiceType
      res <-
        DynamicLogic.findOneConfigWithCacheKey
          (cast mocId)
          (LYT.INVOICE_TEMPLATE scope)
          mbConfigVersionMap
          Nothing
          (Queries.findByMerchantOpCityIdInvoiceTypeAndLanguage mocId.getId mbInvoiceType lang')
          (makeCacheKey mocId scope lang')
      case res of
        Just a -> return $ Just a
        Nothing -> case mbInvoiceType of
          Nothing -> return Nothing
          Just _ ->
            DynamicLogic.findOneConfigWithCacheKey
              (cast mocId)
              (LYT.INVOICE_TEMPLATE LYT.InvoiceTypeGeneric)
              mbConfigVersionMap
              Nothing
              (Queries.findByMerchantOpCityIdInvoiceTypeAndLanguage mocId.getId Nothing lang')
              (makeCacheKey mocId LYT.InvoiceTypeGeneric lang')

clearCache :: (CacheFlow m r, EsqDBFlow m r, Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r, Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Domain.Types.Invoice.InvoiceType -> Kernel.External.Types.Language -> m ()
clearCache mocId mbInvoiceType lang = do
  let scope = maybe LYT.InvoiceTypeGeneric LYT.InvoiceTypeSpecific mbInvoiceType
  DynamicLogic.clearConfigCacheWithPrefix
    (makeCacheKey mocId scope lang)
    (cast mocId)
    (LYT.INVOICE_TEMPLATE scope)
    Nothing

clearCacheById :: (CacheFlow m r, EsqDBFlow m r, Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r, Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
clearCacheById mocId =
  DynamicLogic.clearConfigCache
    (cast mocId)
    (LYT.INVOICE_TEMPLATE LYT.InvoiceTypeGeneric)
    Nothing
