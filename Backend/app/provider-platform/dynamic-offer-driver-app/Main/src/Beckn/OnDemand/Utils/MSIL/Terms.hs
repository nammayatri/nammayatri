-- | MSIL pilot: common helpers for the BAP_TERMS/BPP_TERMS tag groups
-- (STATIC_TERMS, OFFLINE_CONTRACT), shared across every MSIL transformer that
-- touches them (Search, OnSearch, OnConfirm, ...) -- this module is not
-- itself a Layer 2 transformer for any one API, just the common
-- parse/build/patch logic they all call into. Layer 1 never reads or builds
-- either group -- the only thing it does with STATIC_TERMS is emit it under
-- the legacy SETTLEMENT_TERMS group
-- (BecknV2.OnDemand.Utils.Payment.mkSettlementTagGroup, untouched here).
--
-- Verifying (parse-time, STATIC_TERMS only): extracts the BAP's declared
-- BAP_TERMS.STATIC_TERMS off an incoming wire message and stores it on that
-- BAP's BapMetadata row (Storage.CachedQueries.BapMetadata.updateStaticTermsUrlIfChanged),
-- so it's available to echo back later. OFFLINE_CONTRACT is never parsed --
-- only ever built, from config, per the task this module was written for.
--
-- Building: BPP_TERMS (STATIC_TERMS from beckn_config.staticTermsUrl,
-- OFFLINE_CONTRACT from beckn_config.offlineContract) is additive, alongside
-- whatever Layer 1 already put in order.tags -- order-level tags Layer 1 puts
-- there (if any) are left exactly as they are. BAP_TERMS (both tags, echoing
-- back what's on record for this BAP) is only built where the call site
-- explicitly asks for it (on_confirm).
--
-- Every patch/fix operation on an order's tag list goes through
-- 'patchOrderTags' (on_select/on_init/on_confirm); on_search has no Order at
-- all (Catalog -> Provider, no order wrapper), so its BPP_TERMS goes on
-- message.catalog.tags instead, via 'patchCatalogTags'.
--
-- Handler call sites never parse or store STATIC_TERMS themselves -- they
-- (or the MSIL transformer they delegate to, e.g.
-- Beckn.OnDemand.Transformer.MSIL.Search.msilParser) just extract the
-- relevant tag list off the wire message and hand it to
-- 'verifyIncomingStaticTerms', which does the whole parse-and-store pipeline
-- in one call.
module Beckn.OnDemand.Utils.MSIL.Terms
  ( verifyIncomingStaticTerms,
    patchOrderTags,
    patchCatalogTags,
  )
where

import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import qualified Control.Exception as E
import qualified Domain.Types.BapMetadata as DBapMetadata
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData

-- | Extract BAP_TERMS.STATIC_TERMS off an incoming wire message's tag list,
-- parse it as a URL, and -- if it parsed and differs from what's on record --
-- store it on that BAP's BapMetadata row
-- (Storage.CachedQueries.BapMetadata.updateStaticTermsUrlIfChanged), so it's
-- available to echo back later (e.g. at on_confirm). Never throws -- a
-- malformed or absent value from the BAP just means we don't learn a
-- static-terms URL from this request, not a reason to fail the whole
-- search/init/confirm.
verifyIncomingStaticTerms :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DBapMetadata.BapMetadata -> Domain.Domain -> Maybe [Spec.TagGroup] -> m ()
verifyIncomingStaticTerms bapSubscriberId domain tagGroups =
  case Utils.getTagV2 Tag.BAP_TERMS Tag.STATIC_TERMS tagGroups of
    Nothing -> pure ()
    Just rawUrl -> do
      result <- liftIO $ E.try @E.SomeException $ parseBaseUrl rawUrl
      either (const (pure ())) (CQBapMetaData.updateStaticTermsUrlIfChanged bapSubscriberId (show domain)) result

boolTagValue :: Bool -> Text
boolTagValue True = "true"
boolTagValue False = "false"

mkBppTermsTagGroup :: DBC.BecknConfig -> Maybe Spec.TagGroup
mkBppTermsTagGroup bppConfig =
  case catMaybes [staticTermsTag, offlineContractTag] of
    [] -> Nothing
    tags -> Just $ Tag.getFullTagGroup Tag.BPP_TERMS tags
  where
    staticTermsTag = Tag.mkTag Tag.STATIC_TERMS . Just . showBaseUrl <$> bppConfig.staticTermsUrl
    offlineContractTag = Tag.mkTag Tag.OFFLINE_CONTRACT . Just . boolTagValue <$> bppConfig.offlineContract

mkBapTermsTagGroup :: Maybe BaseUrl -> DBC.BecknConfig -> Maybe Spec.TagGroup
mkBapTermsTagGroup mbBapStaticTermsUrl bppConfig =
  case catMaybes [staticTermsTag, offlineContractTag] of
    [] -> Nothing
    tags -> Just $ Tag.getFullTagGroup Tag.BAP_TERMS tags
  where
    staticTermsTag = Tag.mkTag Tag.STATIC_TERMS . Just . showBaseUrl <$> mbBapStaticTermsUrl
    -- OFFLINE_CONTRACT is one fact about the deal, not a separate claim per
    -- side -- BAP_TERMS and BPP_TERMS echo the same config-sourced value.
    offlineContractTag = Tag.mkTag Tag.OFFLINE_CONTRACT . Just . boolTagValue <$> bppConfig.offlineContract

addTagGroup :: Maybe Spec.TagGroup -> Maybe [Spec.TagGroup] -> Maybe [Spec.TagGroup]
addTagGroup Nothing existingGroups = existingGroups
addTagGroup (Just newGroup) existingGroups = Just (fromMaybe [] existingGroups <> [newGroup])

-- | The single patch/fix operation for the order's top-level tag list
-- (message.order.tags -- a sibling of order.payments, per ONDC 2.1.0, NOT
-- nested inside any payment). Always adds BPP_TERMS (our own, from
-- beckn_config); when 'includeBapTerms' is True, also adds BAP_TERMS
-- (echoing back the BAP's own declared terms, if known) -- used only on
-- on_confirm, per this task's scope. on_search/on_select/on_init pass
-- 'includeBapTerms = False' and 'mbBapStaticTermsUrl = Nothing'.
patchOrderTags :: Bool -> Maybe BaseUrl -> DBC.BecknConfig -> Spec.Order -> Spec.Order
patchOrderTags includeBapTerms mbBapStaticTermsUrl bppConfig order =
  order {Spec.orderTags = withBapTerms . withBppTerms $ order.orderTags}
  where
    withBppTerms = addTagGroup (mkBppTermsTagGroup bppConfig)
    withBapTerms
      | includeBapTerms = addTagGroup (mkBapTermsTagGroup mbBapStaticTermsUrl bppConfig)
      | otherwise = \tagGroups -> tagGroups

-- | The single patch/fix operation for on_search's catalog-level tag list
-- (message.catalog.tags -- on_search has no Order to hang order.tags off of).
-- Only ever BPP_TERMS -- on_search has no notion of echoing the BAP's own
-- terms back.
patchCatalogTags :: DBC.BecknConfig -> Spec.Catalog -> Spec.Catalog
patchCatalogTags bppConfig catalog =
  catalog {Spec.catalogTags = addTagGroup (mkBppTermsTagGroup bppConfig) catalog.catalogTags}
