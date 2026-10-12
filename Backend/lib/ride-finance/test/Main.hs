module Main where

import Data.List (subsequences)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)
import RideFinance.Calculations.Build
import RideFinance.Calculations.Ledger
import RideFinance.Calculations.Totals
import RideFinance.Core.Projection (ProjectFareParamsBreakup (..), parseProjectFareParamsBreakup, projectFareParamsBreakupTotal)
import RideFinance.Core.Tax
import RideFinance.Core.Types
import RideFinance.Display.Club
import RideFinance.Display.Config
import RideFinance.Display.Present
import Test.Hspec

sampleBreakup :: ProjectFareParamsBreakup
sampleBreakup =
  ProjectFareParamsBreakup
    { discountApplicableRideFareTaxExclusive = 200,
      discountApplicableRideFareTax = 50,
      discountApplicableRideFareUntaxed = 20,
      nonDiscountApplicableRideFareTaxExclusive = 40,
      nonDiscountApplicableRideFareTax = 10,
      nonDiscountApplicableRideFareUntaxed = 12,
      tollFareTaxExclusive = 30,
      tollFareTax = 7.5,
      cancellationFeeTaxExclusive = 20,
      cancellationTax = 5,
      parkingChargeTaxExclusive = 16,
      parkingChargeTax = 4,
      paymentChargeTaxExclusive = 6,
      paymentChargeTax = 1.5
    }

mkInputs :: TaxRegime -> Maybe ChargeBearer -> Settlement -> HighPrecMoney -> RideFinanceInputs
mkInputs regime chargeBearer settlement requestedDiscount =
  RideFinanceInputs
    { currency = INR,
      breakup = sampleBreakup,
      tip = 15,
      commission = fromGrossAtRate (Just 25.5) 25.1,
      cancellationCommission = fromGrossAtRate (Just 25.5) 12.55,
      paymentCharge = TaxedAmount {taxExclusive = 6.4, tax = 1.6},
      leviedPaymentCharge = Nothing,
      memos = [(CARRIED_CANCELLATION_DUES, 12), (SERVICE_CHARGE, 0)],
      tds = Just 3,
      ..
    }

settlements :: [Settlement]
settlements =
  [ Settlement {settlementType = Online, collectedAtBooth = Set.empty},
    Settlement {settlementType = Cash, collectedAtBooth = Set.empty}
  ]
    <> [ Settlement {settlementType = paidVia, collectedAtBooth = Set.fromList booth}
         | paidVia <- [Online, Cash],
           booth <- [[COMMISSION], [PARKING_CHARGE], [COMMISSION, PARKING_CHARGE], allComponents]
       ]

scenarios :: [RideFinance]
scenarios =
  [ fromProjection (mkInputs regime bearer settlement discount)
    | regime <- [VAT, GST],
      bearer <- [Nothing, Just PAYMENT_CUSTOMER, Just PAYMENT_DRIVER, Just PAYMENT_PLATFORM],
      settlement <- settlements,
      discount <- [0, 40, 1000]
  ]

data SampleRow = SampleRow
  { rowName :: Text,
    rowGroupId :: Maybe Text,
    rowValue :: HighPrecMoney
  }
  deriving (Show, Eq)

sampleKeys :: FareComponent -> Keys Text
sampleKeys = \case
  RIDE_FARE -> Keys {taxExclusiveKeys = ["BaseRide"], taxKeys = ["VATOnline", "VATCash"], clubbedKey = Just "RideInclVat"}
  COMMISSION -> Keys {taxExclusiveKeys = ["CommissionOnline", "CommissionCash"], taxKeys = ["CommissionVAT"], clubbedKey = Nothing}
  _ -> noKeys

sampleRowOps :: RowOps Text SampleRow
sampleRowOps =
  RowOps
    { rowKey = Just . (.rowName),
      rowGroup = (.rowGroupId),
      addAmounts = \anchor other -> anchor {rowValue = anchor.rowValue + other.rowValue},
      withKey = \name row -> row {rowName = name}
    }

ungroupedRows :: [SampleRow]
ungroupedRows =
  [ SampleRow "BaseRide" Nothing 100,
    SampleRow "Tips" Nothing 5,
    SampleRow "VATOnline" Nothing 15,
    SampleRow "CommissionCash" Nothing 20,
    SampleRow "CommissionVAT" Nothing 3
  ]

groupedRows :: [SampleRow]
groupedRows =
  [ SampleRow "BaseRide" (Just "g1") 100,
    SampleRow "VATOnline" (Just "g1") 15,
    SampleRow "BaseRide" (Just "g2") 10,
    SampleRow "VATOnline" (Just "g2") 1
  ]

onlineVat :: Maybe ChargeBearer -> RideFinance
onlineVat bearer = fromProjection (mkInputs VAT bearer (Settlement {settlementType = Online, collectedAtBooth = Set.empty}) 0)

sampleLayout :: EarningsLayout
sampleLayout =
  EarningsLayout
    { componentKeys = \case
        RIDE_FARE -> noKeys {taxKeys = ["RIDE_FARE_VAT"]}
        TOLL_FARE -> noKeys {taxKeys = ["TOLL_VAT"]}
        COMMISSION -> Keys {taxExclusiveKeys = ["COMMISSION"], taxKeys = ["COMMISSION_VAT"], clubbedKey = Just "COMMISSION"}
        PAYMENT_CHARGE -> Keys {taxExclusiveKeys = ["PAYMENT_CHARGE"], taxKeys = ["PAYMENT_CHARGE_VAT"], clubbedKey = Just "PAYMENT_CHARGE"}
        _ -> noKeys,
      customerPaidCode = "FARE_PAID",
      discountCode = "DISCOUNT",
      tipCode = "TIPS",
      paymentChargeCollectedCode = "PAYMENT_CHARGE_COLLECTED",
      memoCode = show
    }

policies :: [ClubPolicy]
policies = map Set.fromList (subsequences [minBound .. maxBound])

main :: IO ()
main = hspec $ do
  describe "Tax" $ do
    it "splits a gross at an inclusive rate and recombines exactly" $ do
      let t = fromGrossAtRate (Just 25.5) 125.5
      t.taxExclusive `shouldBe` 100
      t.tax `shouldBe` 25.5
      inclusive t `shouldBe` 125.5
    it "keeps the gross untaxed without a positive rate" $ do
      fromGrossAtRate Nothing 10 `shouldBe` TaxedAmount {taxExclusive = 10, tax = 0}
      fromGrossAtRate (Just 0) 10 `shouldBe` TaxedAmount {taxExclusive = 10, tax = 0}

  describe "Projection" $
    it "defaults the untaxed ride parts to zero when a BPP does not send them" $ do
      let parsed = parseProjectFareParamsBreakup [("RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE", 100), ("RIDE_FARE_DISCOUNT_APPLICABLE_TAX", 24), ("RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE", 30)]
      (.discountApplicableRideFareUntaxed) <$> parsed `shouldBe` Just 0
      (.nonDiscountApplicableRideFareUntaxed) <$> parsed `shouldBe` Just 0
      projectFareParamsBreakupTotal <$> parsed `shouldBe` Just 154

  describe "Totals" $ do
    it "customer fare total equals the projected breakup total" $
      forM_ scenarios $ \rf -> customerFareTotal rf `shouldBe` projectFareParamsBreakupTotal sampleBreakup
    it "clamps the discount to the discount-applicable base" $
      forM_ [0, 40, 1000] $ \discount ->
        (fromProjection (mkInputs VAT Nothing (Settlement {settlementType = Online, collectedAtBooth = Set.empty}) discount)).appliedDiscount `shouldBe` min discount 270
    it "discount only reduces the ride fare" $
      forM_ scenarios $ \rf ->
        sum (map inclusive (concat (Map.elems rf.chargesAfterDiscount))) `shouldBe` customerFareTotal rf - rf.appliedDiscount
    it "keeps the ride fare parts and cancellation dues as separate components" $
      forM_ scenarios $ \rf -> do
        Map.lookup RIDE_FARE rf.charges
          `shouldBe` Just
            [ TaxedAmount {taxExclusive = 200, tax = 50},
              TaxedAmount {taxExclusive = 20, tax = 0},
              TaxedAmount {taxExclusive = 40, tax = 10},
              TaxedAmount {taxExclusive = 12, tax = 0}
            ]
        Map.lookup CANCELLATION_FARE rf.charges `shouldBe` Just [TaxedAmount {taxExclusive = 20, tax = 5}]
    it "keeps each taxed ride part at its rate and untaxed parts untaxed after the discount" $
      forM_ scenarios $ \rf -> case Map.lookup RIDE_FARE rf.chargesAfterDiscount of
        Just [discountApplicable, discountApplicableUntaxed, nonDiscountApplicable, nonDiscountApplicableUntaxed] -> do
          discountApplicable.tax * 200 `shouldBe` discountApplicable.taxExclusive * 50
          nonDiscountApplicable `shouldBe` TaxedAmount {taxExclusive = 40, tax = 10}
          discountApplicableUntaxed.tax `shouldBe` 0
          nonDiscountApplicableUntaxed `shouldBe` TaxedAmount {taxExclusive = 12, tax = 0}
        other -> expectationFailure ("unexpected ride fare parts: " <> show other)
    it "VAT stays with the driver and all GST on driver earnings is remitted" $
      forM_ scenarios $ \rf -> case rf.regime of
        VAT -> taxRemitted rf `shouldBe` 0
        GST -> taxRemitted rf `shouldBe` sum [p.tax | (c, ps) <- Map.toList rf.chargesAfterDiscount, earnedByDriver c, p <- ps]

  describe "Present" $ do
    it "customer lines sum to the customer payable for every club policy" $
      forM_ scenarios $ \rf -> forM_ policies $ \policy ->
        linesTotal (presentCustomer defaultKeys policy rf) `shouldBe` customerPayable rf
    it "clubbing a component replaces its pair with one inclusive line" $ do
      let rf = onlineVat Nothing
          separate = presentCustomer defaultKeys Set.empty rf
          clubbed = presentCustomer defaultKeys (Set.singleton TOLL_FARE) rf
      map (.code) (filter ((== Just TOLL_FARE) . (.component)) separate) `shouldBe` ["TOLL_FARE", "TOLL_FARE_TAX"]
      map (.code) (filter ((== Just TOLL_FARE) . (.component)) clubbed) `shouldBe` ["TOLL_FARE_INCL_TAX"]

  describe "Earnings" $ do
    it "non-memo earning lines sum to the net before TDS plus remitted tax for every club policy" $
      forM_ scenarios $ \rf -> forM_ policies $ \policy ->
        linesTotal (filter ((/= Memo) . (.role)) (presentEarnings sampleLayout policy rf)) `shouldBe` driverNetBeforeTds rf + taxRemitted rf
    it "starts with the customer-paid row, which holds every clubbed tax" $ do
      let rf = onlineVat (Just PAYMENT_DRIVER)
          clubbedAll = presentEarnings sampleLayout (Set.fromList allComponents) rf
          earnedInclusive = sum [inclusive p | (c, ps) <- Map.toList rf.charges, earnedByDriver c, p <- ps]
      map (.code) (take 1 clubbedAll) `shouldBe` ["FARE_PAID"]
      map (.amount) (take 1 clubbedAll) `shouldBe` [earnedInclusive - rf.appliedDiscount + rf.tip]
      filter ((== Just TaxPart) . (.part)) clubbedAll `shouldBe` []
    it "moves a separated tax out of the customer-paid row" $ do
      let rf = onlineVat (Just PAYMENT_DRIVER)
          split = presentEarnings sampleLayout Set.empty rf
          clubbed = presentEarnings sampleLayout (Set.singleton RIDE_FARE) rf
          farePaid = sum . map (.amount) . filter ((== "FARE_PAID") . (.code))
      map (.code) (filter ((== Just TaxPart) . (.part)) split) `shouldBe` ["RIDE_FARE_VAT", "TOLL_VAT"]
      map (.code) (filter ((== Just TaxPart) . (.part)) clubbed) `shouldBe` ["TOLL_VAT"]
      farePaid clubbed - farePaid split `shouldBe` componentTax rf.charges RIDE_FARE
    it "shows the payment charge as collected and deducted for the customer bearer, deducted for the driver bearer" $ do
      let chargeLines bearer = filter (T.isPrefixOf "PAYMENT_CHARGE" . (.code)) (presentEarnings sampleLayout Set.empty (onlineVat bearer))
          codesFor = map (.code) . chargeLines
          chargeTotal = linesTotal . chargeLines
      codesFor (Just PAYMENT_CUSTOMER) `shouldBe` ["PAYMENT_CHARGE_COLLECTED", "PAYMENT_CHARGE", "PAYMENT_CHARGE_VAT"]
      chargeTotal (Just PAYMENT_CUSTOMER) `shouldBe` 0
      codesFor (Just PAYMENT_DRIVER) `shouldBe` ["PAYMENT_CHARGE", "PAYMENT_CHARGE_VAT"]
      chargeTotal (Just PAYMENT_DRIVER) `shouldBe` -8
      codesFor (Just PAYMENT_PLATFORM) `shouldBe` []
      codesFor Nothing `shouldBe` []
    it "deducts the levied payment charge instead of rescaling the priced one" $ do
      let levied = TaxedAmount {taxExclusive = 4, tax = 1}
          rf = fromProjection (mkInputs VAT (Just PAYMENT_DRIVER) (Settlement {settlementType = Online, collectedAtBooth = Set.empty}) 30) {leviedPaymentCharge = Just levied}
      rf.paymentCharge `shouldBe` levied
      deduction rf PAYMENT_CHARGE `shouldBe` levied
    it "emits tip and fare memos as information only, in order" $ do
      let earningLines = presentEarnings sampleLayout Set.empty (onlineVat (Just PAYMENT_DRIVER))
      map (.code) earningLines
        `shouldBe` ["FARE_PAID", "RIDE_FARE_VAT", "TOLL_VAT", "TIPS", "COMMISSION", "COMMISSION_VAT", "PAYMENT_CHARGE", "PAYMENT_CHARGE_VAT", "CARRIED_CANCELLATION_DUES"]
      [(line.code, line.amount) | line <- earningLines, line.role == Memo] `shouldBe` [("TIPS", 15), ("CARRIED_CANCELLATION_DUES", 12)]
      map (.role) (filter ((== Just COMMISSION) . (.component)) earningLines) `shouldBe` [Deduction, Deduction]

  describe "Components" $ do
    it "derives driver earnings from customer charges that are not deducted from the driver" $
      filter earnedByDriver allComponents `shouldBe` [RIDE_FARE, TOLL_FARE, CANCELLATION_FARE, PARKING_CHARGE]
    it "treats only toll and parking as external charges" $
      filter isExternalComponent allComponents `shouldBe` [TOLL_FARE, PARKING_CHARGE]
    it "classifies keys into their component and amount part" $ do
      classify sampleKeys "VATCash" `shouldBe` Just (RIDE_FARE, TaxPart)
      classify sampleKeys "BaseRide" `shouldBe` Just (RIDE_FARE, TaxExclusivePart)
      classify sampleKeys "RideInclVat" `shouldBe` Just (RIDE_FARE, TaxInclusivePart)
      classify sampleKeys "Tips" `shouldBe` Nothing

  describe "Club" $ do
    it "leaves rows untouched when nothing is clubbed" $
      clubRows sampleKeys sampleRowOps Set.empty ungroupedRows `shouldBe` ungroupedRows
    it "folds a clubbed component into its first row and renames it to the clubbed key" $
      clubRows sampleKeys sampleRowOps (Set.fromList [RIDE_FARE, COMMISSION]) ungroupedRows
        `shouldBe` [SampleRow "RideInclVat" Nothing 115, SampleRow "Tips" Nothing 5, SampleRow "CommissionCash" Nothing 23]
    it "clubs each group separately" $
      clubRows sampleKeys sampleRowOps (Set.singleton RIDE_FARE) groupedRows
        `shouldBe` [SampleRow "RideInclVat" (Just "g1") 115, SampleRow "RideInclVat" (Just "g2") 11]
    it "anchors on the tax row when there is no tax-exclusive row" $
      clubRows sampleKeys sampleRowOps (Set.singleton RIDE_FARE) [SampleRow "Tips" Nothing 5, SampleRow "VATCash" Nothing 7]
        `shouldBe` [SampleRow "Tips" Nothing 5, SampleRow "RideInclVat" Nothing 7]

  describe "Config" $ do
    it "clubs only the listed components and splits everything by default" $ do
      clubPolicy (Just [PAYMENT_CHARGE]) `shouldBe` Set.singleton PAYMENT_CHARGE
      clubPolicy Nothing `shouldBe` Set.empty
    it "uses the first matching invoice rule and splits everything when none matches" $ do
      let rules =
            [ VatClubRule {issuedToType = Just "DRIVER", invoiceType = Just "Ride", components = [COMMISSION]},
              VatClubRule {issuedToType = Nothing, invoiceType = Nothing, components = allComponents}
            ]
      invoiceClubPolicy rules "DRIVER" "Ride" `shouldBe` Set.singleton COMMISSION
      invoiceClubPolicy rules "CUSTOMER" "Ride" `shouldBe` Set.fromList allComponents
      invoiceClubPolicy [] "CUSTOMER" "Ride" `shouldBe` Set.empty

  describe "Ledger" $
    it "owner legs net to the driver net before TDS" $
      forM_ scenarios $ \rf -> ownerNet (ledgerLegs rf) `shouldBe` driverNetBeforeTds rf

  describe "Cash and online split" $ do
    let fare = projectFareParamsBreakupTotal sampleBreakup
        finance bearer settlement discount = fromProjection (mkInputs VAT bearer settlement discount)
    it "online payment settles the driver net online" $ do
      let rf = finance (Just PAYMENT_CUSTOMER) (Settlement {settlementType = Online, collectedAtBooth = Set.empty}) 40
      cashOnlineSplit rf `shouldBe` CashOnlineSplit {collectInCash = Nothing, settledOnline = Just (driverNetBeforeTds rf)}
    it "cash payment collects what the customer pays and settles the offer online" $
      cashOnlineSplit (finance Nothing (Settlement {settlementType = Cash, collectedAtBooth = Set.empty}) 40)
        `shouldBe` CashOnlineSplit {collectInCash = Just (fare - 40 + 15), settledOnline = Just 40}
    it "cash payment without an offer settles nothing online" $
      cashOnlineSplit (finance Nothing (Settlement {settlementType = Cash, collectedAtBooth = Set.empty}) 0)
        `shouldBe` CashOnlineSplit {collectInCash = Just (fare + 15), settledOnline = Nothing}
    it "a booth collecting every component settles the driver net online" $ do
      let rf = finance (Just PAYMENT_DRIVER) (Settlement {settlementType = Online, collectedAtBooth = Set.fromList allComponents}) 0
      cashOnlineSplit rf `shouldBe` CashOnlineSplit {collectInCash = Nothing, settledOnline = Just (driverNetBeforeTds rf)}
    it "a booth collecting parking leaves it out of the cash collected" $
      cashOnlineSplit (finance Nothing (Settlement {settlementType = Cash, collectedAtBooth = Set.singleton PARKING_CHARGE}) 40)
        `shouldBe` CashOnlineSplit {collectInCash = Just (fare - 40 + 15 - 20), settledOnline = Just 40}
    it "a booth collecting commission leaves it out of the cash collected" $
      cashOnlineSplit (finance Nothing (Settlement {settlementType = Cash, collectedAtBooth = Set.singleton COMMISSION}) 40)
        `shouldBe` CashOnlineSplit {collectInCash = Just (fare - 40 + 15 - 25.1), settledOnline = Just 40}
    it "an online booking at a partial booth settles the driver net online" $ do
      let rf = finance Nothing (Settlement {settlementType = Online, collectedAtBooth = Set.singleton COMMISSION}) 40
      cashOnlineSplit rf `shouldBe` CashOnlineSplit {collectInCash = Nothing, settledOnline = Just (driverNetBeforeTds rf)}
  where
    linesTotal :: [Line] -> HighPrecMoney
    linesTotal = sum . map (.amount)
    ownerNet :: [Leg] -> HighPrecMoney
    ownerNet = sum . map (\leg -> if leg.direction == CreditToOwner then leg.amount else negate leg.amount)
