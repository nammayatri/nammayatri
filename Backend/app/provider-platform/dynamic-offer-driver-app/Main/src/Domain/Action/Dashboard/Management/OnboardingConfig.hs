module Domain.Action.Dashboard.Management.OnboardingConfig
  ( getOnboardingConfigGet,
    postOnboardingConfigClone,
    postOnboardingConfigApply,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Onboarding as CommonOnboarding
import qualified API.Types.ProviderPlatform.Management.DriverRegistration as CommonDR
import qualified API.Types.ProviderPlatform.Management.OnboardingConfig as Common
import qualified API.Types.UI.DriverOnboardingV2 as DOVT
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Types as AT
import Data.List (groupBy, nub, nubBy, sortOn)
import qualified Domain.Action.Dashboard.Fleet.Onboarding as DFO
import Domain.Action.Dashboard.Management.NammaTag ()
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DocumentVerificationStagesConfig as DVSC
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as DFODVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Role (..))
import qualified Domain.Types.VehicleCategory as DVeh
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.TypesTH as YTH
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.DocumentVerificationStagesConfig as CQDVSC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CQFODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (projectDriverConfigs)
import qualified Storage.Queries.DocumentVerificationConfig as QDVC
import qualified Storage.Queries.DocumentVerificationStagesConfig as QDVSC
import qualified Storage.Queries.FleetOwnerDocumentVerificationConfig as QFODVC

getOnboardingConfigGet ::
  ShortId DM.Merchant ->
  Context.City ->
  Environment.Flow Common.OnboardingConfigRes
getOnboardingConfigGet merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  cityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- readOnboardingConfig cityId
  pure
    Common.OnboardingConfigRes
      { merchantOperatingCityId = cityId.getId,
        cityName = show opCity,
        config = config,
        enums =
          [ Common.EnumCatalog {name = "ConfigSection", values = map show ([minBound .. maxBound] :: [Common.ConfigSection])},
            Common.EnumCatalog {name = "DocumentApplicableType", values = map show ([minBound .. maxBound] :: [CommonOnboarding.DocumentApplicableType])},
            Common.EnumCatalog {name = "DocumentCategory", values = map show ([minBound .. maxBound] :: [CommonOnboarding.DocumentCategory])},
            Common.EnumCatalog {name = "VehicleCategory", values = map show ([minBound .. maxBound] :: [DVeh.VehicleCategory])}
          ],
        fieldPolicies =
          [ Common.FieldPolicy
              { section = section,
                field = keyField,
                editable = False,
                reason = Just "Part of the primary key. Remove and re-add to change it."
              }
            | section <- [minBound .. maxBound],
              keyField <- if section == Common.STAGES then ["stage", "vehicleCategory", "applicableTo"] else ["documentType"]
          ]
      }

postOnboardingConfigClone ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CloneConfigReq ->
  Environment.Flow Common.CloneConfigRes
postOnboardingConfigClone _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId (ShortId req.sourceMerchantShortId)
  cityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just $ Context.City req.sourceCity)
  cfg <- readOnboardingConfig cityId
  pure
    Common.CloneConfigRes
      { sourceMerchantShortId = req.sourceMerchantShortId,
        sourceCity = req.sourceCity,
        sections = wanted,
        config =
          Common.OnboardingConfig
            { driverDocuments = pick Common.DRIVER_DOCUMENTS [g {Common.documents = byType g.documents} | g <- cfg.driverDocuments],
              fleetOwnerDocuments = pick Common.FLEET_OWNER_DOCUMENTS (byType cfg.fleetOwnerDocuments),
              fleetBusinessDocuments = pick Common.FLEET_BUSINESS_DOCUMENTS (byType cfg.fleetBusinessDocuments),
              stages = pick Common.STAGES cfg.stages
            }
      }
  where
    wanted = if null req.sections then [minBound .. maxBound] else nub req.sections

    pick :: Common.ConfigSection -> [a] -> [a]
    pick s xs = if s `elem` wanted then xs else []

    byType xs = if null req.documentTypes then xs else filter ((`elem` req.documentTypes) . (.documentType)) xs

postOnboardingConfigApply ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.ApplyConfigReq ->
  Environment.Flow Common.ApplyConfigRes
postOnboardingConfigApply merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  cityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let mId = merchant.id
  driverChanges <- writeDriverDocs now mId cityId req.config.driverDocuments
  legacyFleet <- QFODVC.findAllByMerchantOpCityId Nothing Nothing cityId
  let tagged = map (FLEET_OWNER,) req.config.fleetOwnerDocuments <> map (FLEET_BUSINESS,) req.config.fleetBusinessDocuments
  fleetChanges <-
    if null legacyFleet
      then writeFleetIntoDvc now mId cityId tagged
      else concat <$> mapM (\(role, docs) -> writeLegacyFleet now mId cityId role docs) [(FLEET_OWNER, req.config.fleetOwnerDocuments), (FLEET_BUSINESS, req.config.fleetBusinessDocuments)]
  stageChanges <- writeStages now mId cityId req.config.stages
  CQDVC.clearCache cityId
  CQFODVC.clearCache cityId
  CQDVSC.clearCache cityId
  pure Common.ApplyConfigRes {appliedAt = now, changes = driverChanges <> fleetChanges <> stageChanges, issues = []}
  where
    writeDriverDocs now mId cityId groups =
      fmap concat . forM groups $ \group ->
        forM (zip [0 ..] group.documents) $ \(idx, doc) -> do
          docType <- resolveDocType doc.documentType
          existing <- QDVC.findByPrimaryKey docType cityId group.vehicleCategory
          row <- applyDoc now idx doc =<< maybe (newDvcRow now mId cityId docType group.vehicleCategory) pure existing
          maybe (QDVC.create row) (const (QDVC.updateByPrimaryKey row)) existing
          pure (change existing Common.DRIVER_DOCUMENTS (show doc.documentType))

    writeFleetIntoDvc now mId cityId tagged =
      fmap concat . forM (zip [0 ..] (nubBy (\a b -> (snd a).documentType == (snd b).documentType) tagged)) $ \(idx, (_, doc)) -> do
        docType <- resolveDocType doc.documentType
        rows <- QDVC.findByDimensions cityId (Just docType) Nothing
        let roles = nub [r | (r, d) <- tagged, d.documentType == doc.documentType]
            category = maybe DVeh.CAR (.vehicleCategory) (listToMaybe (filter ((== Just DVC.Fleet) . (.documentCategory)) rows))
            existing = listToMaybe (filter ((== category) . (.vehicleCategory)) rows)
        applied <- applyDoc now idx doc =<< maybe (newDvcRow now mId cityId docType category) pure existing
        let row = applied {DVC.documentCategory = Just DVC.Fleet, DVC.rolesAllowedToUploadDocument = Just roles}
        maybe (QDVC.create row) (const (QDVC.updateByPrimaryKey row)) existing
        pure [change existing (sectionFor r) (show doc.documentType) | r <- roles]

    writeLegacyFleet now mId cityId role docs =
      forM (zip [0 ..] docs) $ \(idx, doc) -> do
        docType <- resolveDocType doc.documentType
        existing <- QFODVC.findByPrimaryKey docType cityId [role]
        row <- applyFleetDoc now idx doc =<< maybe (newFvcRow now mId cityId docType role) pure existing
        maybe (QFODVC.create row) (const (QFODVC.updateByPrimaryKey row)) existing
        pure (change existing (sectionFor role) (show doc.documentType))

    writeStages now mId cityId groups = do
      existing :: [DVSC.DocumentVerificationStagesConfig] <- QDVSC.findAllByMerchantOpCityId Nothing Nothing cityId
      fmap concat . forM groups $ \g -> do
        category <- readEnum g.documentCategory
        forM g.stages $ \s -> do
          applicableTo <- readEnum s.applicableTo
          let stage = SDO.castDocumentOnboardingStageFromCommon s.stage
              match r =
                r.documentOnboardingStage == stage
                  && r.vehicleCategory == s.vehicleCategory
                  && r.applicableTo == applicableTo
                  && r.documentCategory == category
              found = find match existing
          row <- overlay [] s =<< maybe (newStageRow now mId cityId stage s.vehicleCategory applicableTo category) pure found
          maybe (QDVSC.create row {DVSC.updatedAt = now}) (const (QDVSC.updateByPrimaryKey row {DVSC.updatedAt = now})) found
          pure (change found Common.STAGES (show s.stage))

    sectionFor role = if role == FLEET_OWNER then Common.FLEET_OWNER_DOCUMENTS else Common.FLEET_BUSINESS_DOCUMENTS

    change existing section ident =
      Common.ConfigChange
        { kind = maybe Common.ADDED (const Common.MODIFIED) existing,
          section = section,
          identifier = ident,
          field = Nothing,
          before = Nothing,
          after = Nothing
        }

    applyDoc now idx d row = do
      merged <- overlay ["documentType", "rolesAllowedToUploadDocument"] d row
      pure merged {DVC.order = idx, DVC.updatedAt = now}

    applyFleetDoc now idx d row = do
      merged <- overlay ["documentType", "rolesAllowedToUploadDocument"] d row
      pure merged {DFODVC.order = idx, DFODVC.updatedAt = now}

    newDvcRow now mId cityId docType category = do
      base <- defaultRow (Proxy @DVC.DocumentVerificationConfig)
      pure
        base
          { DVC.merchantId = mId,
            DVC.merchantOperatingCityId = cityId,
            DVC.documentType = docType,
            DVC.vehicleCategory = category,
            DVC.maxRetryCount = 4,
            DVC.vehicleClassCheckType = DVC.Infix,
            DVC.supportedVehicleClasses = DVC.DLValidClasses [],
            DVC.createdAt = now,
            DVC.updatedAt = now
          }

    newFvcRow now mId cityId docType role = do
      base <- defaultRow (Proxy @DFODVC.FleetOwnerDocumentVerificationConfig)
      pure base {DFODVC.merchantId = mId, DFODVC.merchantOperatingCityId = cityId, DFODVC.documentType = docType, DFODVC.role = [role], DFODVC.maxRetryCount = 4, DFODVC.createdAt = now, DFODVC.updatedAt = now}

    newStageRow now mId cityId stage vehCategory applicableTo docCategory = do
      base <- defaultRow (Proxy @DVSC.DocumentVerificationStagesConfig)
      pure
        base
          { DVSC.merchantId = mId,
            DVSC.merchantOperatingCityId = cityId,
            DVSC.documentOnboardingStage = stage,
            DVSC.vehicleCategory = vehCategory,
            DVSC.applicableTo = applicableTo,
            DVSC.documentCategory = docCategory,
            DVSC.createdAt = now,
            DVSC.updatedAt = now
          }

    resolveDocType t =
      castDocumentTypeFromCommon t
        & fromMaybeM (InvalidRequest $ "Document type " <> show t <> " has no storable equivalent.")

readOnboardingConfig :: Id DMOC.MerchantOperatingCity -> Environment.Flow Common.OnboardingConfig
readOnboardingConfig cityId = do
  docRows <- QDVC.findAllByMerchantOpCityId Nothing Nothing cityId
  stageRows <- QDVSC.findAllByMerchantOpCityId Nothing Nothing cityId
  legacyFleet <- QFODVC.findAllByMerchantOpCityId Nothing Nothing cityId
  -- Same all-or-nothing fallback the read side uses: a city keeps FVC until it has none left.
  let fleetRows = if null legacyFleet then projectDriverConfigs docRows else legacyFleet
      driverRows = filter ((/= Just DVC.Fleet) . (.documentCategory)) docRows
  pure
    Common.OnboardingConfig
      { driverDocuments =
          [ Common.DriverDocumentGroup {vehicleCategory = r.vehicleCategory, documents = map docEntity grp}
            | grp@(r : _) <- groupOn (.vehicleCategory) driverRows
          ],
        fleetOwnerDocuments = map fleetEntity (withRole FLEET_OWNER fleetRows),
        fleetBusinessDocuments = map fleetEntity (withRole FLEET_BUSINESS fleetRows),
        stages =
          [ Common.StageGroup {documentCategory = SDO.castDocumentCategory r.documentCategory, stages = map stageEntity grp}
            | grp@(r : _) <- groupOn (.documentCategory) stageRows
          ]
      }
  where
    -- sortOn is stable, so the query's ORDER BY "order" survives inside each group.
    groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
    groupOn f = groupBy (\x y -> f x == f y) . sortOn f

    withRole role = filter ((role `elem`) . (.role))

    docEntity DVC.DocumentVerificationConfig {..} =
      DFO.castDocumentVerificationConfigAPIEntity
        DOVT.DocumentVerificationConfigAPIEntity
          { isMandatoryForEnabling = fromMaybe isMandatory isMandatoryForEnabling,
            documentFlowGrouping = fromMaybe DVC.STANDARD documentFlowGrouping,
            verificationProvidersPriorityList = Nothing,
            ..
          }

    fleetEntity DFODVC.FleetOwnerDocumentVerificationConfig {..} =
      DFO.castDocumentVerificationConfigAPIEntity
        DOVT.DocumentVerificationConfigAPIEntity
          { applicableTo = DVC.FLEET,
            documentFlowGrouping = DVC.STANDARD,
            isMandatoryForEnabling = fromMaybe isMandatory isMandatoryForEnabling,
            filterForOldApks = Nothing,
            isReminderSupported = Nothing,
            rcNumberPrefixList = [],
            verificationProvidersPriorityList = Nothing,
            ..
          }

    stageEntity DVSC.DocumentVerificationStagesConfig {..} =
      DFO.castOnboardingStageAPIEntity DOVT.DocumentOnboardingStageAPIEntity {stage = documentOnboardingStage, ..}

castDocumentTypeFromCommon :: CommonDR.DocumentType -> Maybe DVC.DocumentType
castDocumentTypeFromCommon = \case
  CommonDR.ProfilePhotoImage -> Just DVC.ProfilePhoto
  CommonDR.VehiclePermitImage -> Just DVC.VehiclePermit
  CommonDR.VehicleFitnessCertificateImage -> Just DVC.VehicleFitnessCertificate
  CommonDR.VehicleInsuranceImage -> Just DVC.VehicleInsurance
  CommonDR.VehiclePUCImage -> Just DVC.VehiclePUC
  CommonDR.VehicleInspectionImage -> Just DVC.VehicleInspectionForm
  CommonDR.DriverInspectionFormImage -> Just DVC.DriverInspectionForm
  CommonDR.TrainingFormImage -> Just DVC.TrainingForm
  CommonDR.UploadProfileImage -> Just DVC.UploadProfile
  CommonDR.PanAadhaarLink -> Just DVC.PanAadhaarLinkage
  t -> readMaybe (show t)

readEnum :: (Show a, Read b, MonadThrow m, Log m) => a -> m b
readEnum a = readMaybe (show a) & fromMaybeM (InvalidRequest $ "Unsupported value: " <> show a)

defaultRow :: (YTH.GenericDefaults a, MonadThrow m, Log m) => Proxy a -> m a
defaultRow p = listToMaybe (YTH.genDef p) & fromMaybeM (InvalidRequest "Onboarding row has no generic default")

overlay :: (A.ToJSON e, A.ToJSON r, A.FromJSON r, MonadThrow m, Log m) => [Text] -> e -> r -> m r
overlay skip entity row = case (A.toJSON entity, A.toJSON row) of
  (A.Object e, A.Object r) ->
    AT.parseMaybe A.parseJSON (A.Object (AKM.union (AKM.filterWithKey keep e) r))
      & fromMaybeM (InvalidRequest "Onboarding config field types do not line up with storage")
  _ -> throwError (InvalidRequest "Onboarding config is not a JSON object")
  where
    keep k _ = AK.toText k `notElem` skip
