module SharedLogic.DriverOnboarding.VehicleImageGridVerification
  ( VehicleImageVerdict (..),
    verifyVehicleImageBatchWithGrid,
  )
where

import qualified Control.Monad.Catch as Catch
import Data.Aeson (decode, withObject, (.:), (.:?))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Grid.Types as Grid
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Grid.GridDecision (callGridDecision, describeGridFailure)
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC

data VehicleImageVerdict = VehicleImageVerdict
  { plateMatchesRc :: Bool,
    isClean :: Bool
  }
  deriving (Show, Eq)

rcNumberBounds :: (Int, Int)
rcNumberBounds = (5, 12)

batchStateText :: Text
batchStateText = "Vehicle photos (front/back/interior/odometer, same vehicle) uploaded by a driver during onboarding."

verifyVehicleImageBatchWithGrid ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, CoreMetrics m, HasRequestId r, MonadReader r m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DVRC.VehicleRegistrationCertificate ->
  NonEmpty Text ->
  Maybe Text ->
  m (Maybe VehicleImageVerdict)
verifyVehicleImageBatchWithGrid merchantId merchantOpCityId rcId rawImages mbQuestionsConfig =
  case buildQuestionsFromConfig mbQuestionsConfig of
    Nothing -> pure Nothing
    Just (isCleanThreshold, mkQuestions) -> do
      result <- Catch.try @_ @SomeException lookupPlate
      case result of
        Left err
          | Just (asyncErr :: SomeAsyncException) <- fromException err -> Catch.throwM asyncErr
          | otherwise -> do
            logWarning $ "Vehicle image batch RC lookup failed, falling back to manual/provider flow: " <> describeGridFailure err
            pure Nothing
        Right Nothing -> pure Nothing
        Right (Just plate) -> do
          let questions = mkQuestions plate
          mbResp <- callGridDecision merchantId merchantOpCityId batchStateText questions rawImages
          pure (mbResp >>= judge isCleanThreshold questions)
  where
    lookupPlate =
      QRC.findById rcId >>= \case
        Nothing -> pure Nothing
        Just rc -> do
          rcNumber <- decrypt rc.certificateNumber
          pure (Grid.normalizeIdentifier rcNumberBounds rcNumber)
    judge isCleanThreshold questions resp = do
      let answers = Grid.decodeAnswers questions resp
      plateAnswer <- Grid.getChoice "plate_matches_rc" answers
      let isClean = maybe False ((>= isCleanThreshold) . (.pYes)) (Grid.getNoul "is_clean" answers)
      case plateAnswer.choice of
        "match" -> Just VehicleImageVerdict {plateMatchesRc = True, isClean}
        "no_match" -> Just VehicleImageVerdict {plateMatchesRc = False, isClean}
        _ -> Nothing

data StoredGridQuestion = StoredGridQuestion
  { questionType :: Text,
    instructions :: Text,
    criteria :: Maybe [StoredChoice],
    threshold :: Maybe Double
  }
  deriving (Generic)

instance FromJSON StoredGridQuestion where
  parseJSON = withObject "StoredGridQuestion" $ \o ->
    StoredGridQuestion
      <$> o .: "type"
      <*> o .: "instructions"
      <*> o .:? "criteria"
      <*> o .:? "threshold"

data StoredChoice = StoredChoice
  { key :: Text,
    text :: Text
  }
  deriving (Generic, FromJSON)

substitutePlate :: Text -> Text -> Text
substitutePlate = T.replace "{{plate}}"

data ValidatedGridQuestion
  = ValidatedNoul Text Double
  | ValidatedChoice Text (NonEmpty (Text, Text))

validateStoredQuestion :: StoredGridQuestion -> Maybe ValidatedGridQuestion
validateStoredQuestion q = case q.questionType of
  "noul" -> do
    threshold <- case q.threshold of
      Nothing -> Just 0.5
      Just t | t >= 0 && t <= 1 -> Just t
      Just _ -> Nothing
    Just (ValidatedNoul q.instructions threshold)
  "choice" -> do
    options <- q.criteria
    optionsNE <- NE.nonEmpty [(c.key, c.text) | c <- options]
    Just (ValidatedChoice q.instructions optionsNE)
  _ -> Nothing

applyPlate :: Text -> ValidatedGridQuestion -> Grid.GridQuestion
applyPlate plate = \case
  ValidatedNoul instructions _ -> Grid.NoulQ (substitutePlate plate instructions)
  ValidatedChoice instructions options -> Grid.ChoiceQ (substitutePlate plate instructions) (fmap (substitutePlate plate) <$> options)

buildQuestionsFromConfig :: Maybe Text -> Maybe (Double, Text -> Grid.GridQuestions)
buildQuestionsFromConfig mbConfigJson = do
  configJson <- mbConfigJson
  configMap <- decode (LBS.fromStrict (TE.encodeUtf8 configJson)) :: Maybe (Map.Map Text StoredGridQuestion)
  isCleanQ@(ValidatedNoul _ isCleanThreshold) <- Map.lookup "is_clean" configMap >>= validateStoredQuestion >>= requireNoul
  plateQ <- Map.lookup "plate_matches_rc" configMap >>= validateStoredQuestion
  Just
    ( isCleanThreshold,
      \plate ->
        Grid.mkGridQuestions
          [ ("is_clean", applyPlate plate isCleanQ),
            ("plate_matches_rc", applyPlate plate plateQ)
          ]
    )
  where
    requireNoul q@(ValidatedNoul _ _) = Just q
    requireNoul _ = Nothing
