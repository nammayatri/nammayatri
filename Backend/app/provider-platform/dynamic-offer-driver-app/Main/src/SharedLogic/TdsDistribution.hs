-- | Helpers for TDS certificate (Form 16A) disbursement: certificate file names, financial years and
-- matching a deductee PAN to a driver or fleet owner of a city.
--
-- Certificate files are named PAN_Qx_FY.pdf, e.g. ABCPG1234K_Q2_2026-27.pdf.
module SharedLogic.TdsDistribution
  ( ParsedTdsFileName (..),
    PanMatch (..),
    parseTdsFileName,
    isPdfFile,
    isValidFinancialYear,
    financialYearStart,
    assessmentYearOf,
    matchPan,
    recipientTypeForRole,
    personDisplayName,
  )
where

import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Types.DriverPanCard as DPC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TDSDistributionPdfFile as DTF
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverPanCardExtra as QPanCard
import qualified Storage.Queries.Person as QPerson

data ParsedTdsFileName = ParsedTdsFileName
  { pan :: Text,
    -- | "Q1" .. "Q4"
    quarter :: Text,
    -- | e.g. "2026-27"
    financialYear :: Text
  }
  deriving (Show, Eq)

-- | Parse PAN_Qx_FY from a certificate file name. The extension is ignored here (see 'isPdfFile'), so a
-- correctly named non-PDF file still yields its PAN and can be matched for display.
parseTdsFileName :: Text -> Maybe ParsedTdsFileName
parseTdsFileName fileName =
  case T.splitOn "_" (baseName fileName) of
    [panPart, quarterPart, fyPart] -> do
      let pan = T.toUpper panPart
          quarter = T.toUpper quarterPart
      guard (isValidPan pan)
      guard (quarter `elem` ["Q1", "Q2", "Q3", "Q4"])
      guard (isValidFinancialYear fyPart)
      pure ParsedTdsFileName {pan, quarter, financialYear = fyPart}
    _ -> Nothing
  where
    baseName name = case T.breakOnEnd "." name of
      ("", _) -> name
      (withDot, _) -> T.dropEnd 1 withDot

isPdfFile :: Text -> Text -> Bool
isPdfFile fileName mimeType =
  T.toLower (snd $ T.breakOnEnd "." fileName) == "pdf"
    && T.toLower (T.strip mimeType) `elem` ["application/pdf", ""]

-- | PAN: 5 letters, 4 digits, 1 letter.
isValidPan :: Text -> Bool
isValidPan pan =
  T.length pan == 10
    && T.all Char.isAsciiUpper (T.take 5 pan)
    && T.all Char.isDigit (T.take 4 $ T.drop 5 pan)
    && T.all Char.isAsciiUpper (T.drop 9 pan)

-- | "YYYY-YY" where YY is the year after YYYY, e.g. "2026-27".
isValidFinancialYear :: Text -> Bool
isValidFinancialYear fy = isJust (financialYearStart fy)

financialYearStart :: Text -> Maybe Int
financialYearStart fy = case T.splitOn "-" fy of
  [startTxt, endTxt]
    | T.length startTxt == 4 && T.length endTxt == 2 -> do
      start <- readMaybe (T.unpack startTxt)
      end <- readMaybe (T.unpack endTxt)
      guard ((start + 1) `mod` 100 == end)
      pure start
  _ -> Nothing

-- | The assessment year of a financial year ("2026-27" -> "2027-28"). Only used to recognise legacy
-- tds_distribution_record rows, which carry an assessment year instead of a financial year.
assessmentYearOf :: Text -> Maybe Text
assessmentYearOf fy = do
  start <- financialYearStart fy
  let next = start + 1
  pure $ show next <> "-" <> T.justifyRight 2 '0' (show ((next + 1) `mod` 100))

data PanMatch
  = PanMatched DP.Person DTF.TDSRecipientType
  | PanNotFound
  | PanAmbiguous

-- | Find the driver or fleet owner of this city who owns the PAN. The PAN is looked up by its hash in
-- driver_pan_card (fleet owners' PANs live there too, keyed by their person id); INVALID documents are
-- ignored. A PAN owned by someone outside this city counts as not found.
matchPan ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m PanMatch
matchPan merchantOpCityId pan = do
  panHash <- getDbHash pan
  panCards <- QPanCard.findAllByEncryptedPanNumber panHash
  let ownerIds = L.nub $ (.driverId) <$> filter ((/= Documents.INVALID) . (.verificationStatus)) (panCards :: [DPC.DriverPanCard])
  owners <- catMaybes <$> mapM QPerson.findById ownerIds
  let inScope =
        [ (person, recipientType)
          | person <- owners,
            person.merchantOperatingCityId == merchantOpCityId,
            Just recipientType <- [recipientTypeForRole person.role]
        ]
  pure $ case inScope of
    [] -> PanNotFound
    [(person, recipientType)] -> PanMatched person recipientType
    _ -> PanAmbiguous

recipientTypeForRole :: DP.Role -> Maybe DTF.TDSRecipientType
recipientTypeForRole = \case
  DP.DRIVER -> Just DTF.DRIVER
  DP.FLEET_OWNER -> Just DTF.FLEET_OWNER
  DP.FLEET_BUSINESS -> Just DTF.FLEET_OWNER
  _ -> Nothing

personDisplayName :: DP.Person -> Text
personDisplayName person = T.unwords $ person.firstName : catMaybes [person.middleName, person.lastName]
