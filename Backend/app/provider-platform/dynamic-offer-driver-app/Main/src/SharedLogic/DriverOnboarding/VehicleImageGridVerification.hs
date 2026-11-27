module SharedLogic.DriverOnboarding.VehicleImageGridVerification
  ( VehicleImageVerdict (..),
    verifyVehicleImageBatchWithGrid,
  )
where

import qualified Control.Monad.Catch as Catch
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Grid.Types as Grid
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Grid.GridDecision (AiVerifyResult (..), aiVerifyDoc, describeGridFailure, isYes)
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC

data VehicleImageVerdict = VehicleImageVerdict
  { plateMatchesRc :: Bool,
    isClean :: Bool
  }
  deriving (Show, Eq)

rcNumberBounds :: (Int, Int)
rcNumberBounds = (5, 12)

-- | Question labels this flow acts on (rows in ai_verification_question).
-- plate_matches_rc must be a CHOICE question with the options "match" and "no_match";
-- is_clean must be a NOUL question. Other labels are still asked, but this flow ignores their answers.
plateMatchesRcKey, isCleanKey :: Grid.QuestionKey
plateMatchesRcKey = "plate_matches_rc"
isCleanKey = "is_clean"

-- | Placeholder for the RC number in question text, e.g. "Does the plate read {rc_number}?".
rcNumberPlaceholder :: Text
rcNumberPlaceholder = "rc_number"

verifyVehicleImageBatchWithGrid ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, CoreMetrics m, HasRequestId r, MonadReader r m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DVRC.VehicleRegistrationCertificate ->
  NonEmpty DVC.DocumentType ->
  NonEmpty Text ->
  m (Maybe VehicleImageVerdict)
verifyVehicleImageBatchWithGrid merchantId merchantOpCityId rcId docTypes rawImages = do
  result <- Catch.try @_ @SomeException lookupRcNumber
  case result of
    Left err
      | Just (asyncErr :: SomeAsyncException) <- fromException err -> Catch.throwM asyncErr
      | otherwise -> do
        logWarning $ "Vehicle image batch RC lookup failed, falling back to manual/provider flow: " <> describeGridFailure err
        pure Nothing
    Right Nothing -> pure Nothing
    Right (Just rcNumber) -> do
      mbResult <- aiVerifyDoc merchantId merchantOpCityId (show <$> docTypes) [(rcNumberPlaceholder, rcNumber)] rawImages
      pure (mbResult >>= judge)
  where
    lookupRcNumber =
      QRC.findById rcId >>= \case
        Nothing -> pure Nothing
        Just rc -> do
          rcNumber <- decrypt rc.certificateNumber
          pure (Grid.normalizeIdentifier rcNumberBounds rcNumber)
    judge result@AiVerifyResult {answers} = do
      plateAnswer <- Grid.getChoice plateMatchesRcKey answers
      let isClean = fromMaybe False (isYes isCleanKey result)
      case plateAnswer.choice of
        "match" -> Just VehicleImageVerdict {plateMatchesRc = True, isClean}
        "no_match" -> Just VehicleImageVerdict {plateMatchesRc = False, isClean}
        _ -> Nothing
