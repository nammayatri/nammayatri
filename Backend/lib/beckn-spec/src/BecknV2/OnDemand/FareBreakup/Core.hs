{-# LANGUAGE DeriveTraversable #-}

module BecknV2.OnDemand.FareBreakup.Core
  ( RideTaxRates (..),
    BreakupLine (..),
    TaxTags (..),
    chargeTitles,
    taxTags,
    tagTitle,
    taxTitles,
    taxAmountTitles,
    isChargeTitle,
    isTaxTitle,
    platformFeeTitle,
    platformFeeTaxTitles,
    platformFeeLines,
    nonDiscountableTitles,
    componentLines,
    linesAmount,
    discountableAmount,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))

data RideTaxRates a = RideTaxRates
  { rideFare :: a,
    toll :: a,
    parking :: a,
    airportEntryFee :: a,
    cancellationFee :: a,
    paymentCharge :: a
  }
  deriving stock (Show, Read, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (RideTaxRates a)

instance FromJSON a => FromJSON (RideTaxRates a)

instance (Typeable a, ToSchema a) => ToSchema (RideTaxRates a)

instance Applicative RideTaxRates where
  pure x = RideTaxRates x x x x x x
  f <*> x =
    RideTaxRates
      { rideFare = f.rideFare x.rideFare,
        toll = f.toll x.toll,
        parking = f.parking x.parking,
        airportEntryFee = f.airportEntryFee x.airportEntryFee,
        cancellationFee = f.cancellationFee x.cancellationFee,
        paymentCharge = f.paymentCharge x.paymentCharge
      }

data BreakupLine = BreakupLine
  { title :: Text,
    amount :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

chargeTitles :: RideTaxRates [Text]
chargeTitles =
  RideTaxRates
    { rideFare = [],
      toll = [show Enums.TOLL_CHARGES],
      parking = [show Enums.PARKING_CHARGE, show Enums.PARKING_CHARGES],
      airportEntryFee = [show Enums.AIRPORT_ENTRY_FEE],
      cancellationFee = [show Enums.CANCELLATION_CHARGES, show Enums.RIDE_CANCELLATION_CHARGES],
      paymentCharge = [show Enums.PAYMENT_CHARGE]
    }

data TaxTags = TaxTags
  { tax :: (Enums.QuoteBreakupTitle, [Enums.QuoteBreakupTitle]),
    cgst :: (Enums.QuoteBreakupTitle, [Enums.QuoteBreakupTitle]),
    sgst :: (Enums.QuoteBreakupTitle, [Enums.QuoteBreakupTitle]),
    igst :: (Enums.QuoteBreakupTitle, [Enums.QuoteBreakupTitle])
  }

taxTags :: RideTaxRates TaxTags
taxTags =
  RideTaxRates
    { rideFare = taxTags' (Enums.RIDE_FARE_TAX, [Enums.RIDE_FARE_VAT, Enums.RIDE_VAT, Enums.TAX, Enums.FIXED_GOVERNMENT_RATE]) Enums.RIDE_FARE_CGST Enums.RIDE_FARE_SGST Enums.RIDE_FARE_IGST,
      toll = taxTags' (Enums.TOLL_TAX, [Enums.TOLL_VAT, Enums.TOLL_FARE_TAX]) Enums.TOLL_CGST Enums.TOLL_SGST Enums.TOLL_IGST,
      parking = taxTags' (Enums.PARKING_TAX, [Enums.PARKING_VAT, Enums.PARKING_CHARGE_TAX]) Enums.PARKING_CGST Enums.PARKING_SGST Enums.PARKING_IGST,
      airportEntryFee = taxTags' (Enums.AIRPORT_ENTRY_FEE_TAX, [Enums.AIRPORT_ENTRY_FEE_VAT]) Enums.AIRPORT_ENTRY_FEE_CGST Enums.AIRPORT_ENTRY_FEE_SGST Enums.AIRPORT_ENTRY_FEE_IGST,
      cancellationFee = taxTags' (Enums.CANCELLATION_FEE_TAX, [Enums.CANCELLATION_FEE_VAT, Enums.RIDE_CANCELLATION_TAX, Enums.CANCELLATION_TAX]) Enums.CANCELLATION_FEE_CGST Enums.CANCELLATION_FEE_SGST Enums.CANCELLATION_FEE_IGST,
      paymentCharge = taxTags' (Enums.PAYMENT_CHARGE_TAX, [Enums.PAYMENT_CHARGE_VAT]) Enums.PAYMENT_CHARGE_CGST Enums.PAYMENT_CHARGE_SGST Enums.PAYMENT_CHARGE_IGST
    }
  where
    taxTags' taxTag cgstTag sgstTag igstTag = TaxTags {tax = taxTag, cgst = (cgstTag, []), sgst = (sgstTag, []), igst = (igstTag, [])}

tagTitle :: (Enums.QuoteBreakupTitle, [Enums.QuoteBreakupTitle]) -> Text
tagTitle = show . fst

tagTitles :: (Enums.QuoteBreakupTitle, [Enums.QuoteBreakupTitle]) -> [Text]
tagTitles (current, legacy) = map show (current : legacy)

taxAmountTitles :: RideTaxRates [Text]
taxAmountTitles = tagTitles . (.tax) <$> taxTags

taxTitles :: RideTaxRates [Text]
taxTitles = (\tags -> concatMap tagTitles [tags.tax, tags.cgst, tags.sgst, tags.igst]) <$> taxTags

isChargeTitle :: RideTaxRates (Text -> Bool)
isChargeTitle =
  RideTaxRates
    { rideFare = const False,
      toll = (`elem` chargeTitles.toll),
      parking = (`elem` chargeTitles.parking),
      airportEntryFee = \t -> t `elem` chargeTitles.airportEntryFee || isJust (Enums.gateFeeBreakupItemName t),
      cancellationFee = (`elem` chargeTitles.cancellationFee),
      paymentCharge = (`elem` chargeTitles.paymentCharge)
    }

isTaxTitle :: Text -> Bool
isTaxTitle t = t `elem` platformFeeTaxTitles || any (t `elem`) taxTitles

platformFeeTitle :: Text
platformFeeTitle = show Enums.PLATFORM_FEE

platformFeeTaxTitles :: [Text]
platformFeeTaxTitles = map show [Enums.CGST, Enums.SGST, Enums.PLATFORM_FEE_CGST, Enums.PLATFORM_FEE_SGST, Enums.PLATFORM_FEE_IGST, Enums.PLATFORM_FEE_VAT]

platformFeeLines :: [BreakupLine] -> (HighPrecMoney, HighPrecMoney)
platformFeeLines ls = (linesAmount (== platformFeeTitle) ls, linesAmount (`elem` platformFeeTaxTitles) ls)

nonDiscountableTitles :: [Text]
nonDiscountableTitles =
  map
    show
    [ Enums.PET_CHARGES,
      Enums.STATE_ENTRY_PERMIT_CHARGES,
      Enums.DRIVER_SELECTED_FARE,
      Enums.CUSTOMER_SELECTED_FARE,
      Enums.INSURANCE_CHARGES,
      Enums.CARD_CHARGES_ON_FARE,
      Enums.CARD_CHARGES_FIXED,
      Enums.SAFETY_PLUS_CHARGES,
      Enums.NO_CHARGES,
      Enums.LUGGAGE_CHARGE,
      Enums.DRIVER_ALLOWANCE,
      Enums.AIRPORT_CONVENIENCE_FEE,
      Enums.RETURN_FEE,
      Enums.BOOTH_CHARGE,
      Enums.SCHEDULING_CHARGE,
      Enums.NEGOTIATED_FARE_DELTA,
      Enums.ADD_ON_CHARGES,
      Enums.NYREGULAR_SUBSCRIPTION_CHARGE,
      Enums.BUYER_ADDITIONAL_AMOUNT,
      Enums.DRIVER_BATA
    ]

linesAmount :: (Text -> Bool) -> [BreakupLine] -> HighPrecMoney
linesAmount matches ls = sum [l.amount | l <- ls, matches l.title]

componentLines :: [BreakupLine] -> RideTaxRates (HighPrecMoney, HighPrecMoney)
componentLines ls =
  RideTaxRates
    { rideFare = (linesAmount isRideFareCharge ls, snd perComponent.rideFare),
      toll = perComponent.toll,
      parking = perComponent.parking,
      airportEntryFee = perComponent.airportEntryFee,
      cancellationFee = perComponent.cancellationFee,
      paymentCharge = perComponent.paymentCharge
    }
  where
    perComponent = (\isCharge titles -> (linesAmount isCharge ls, linesAmount (`elem` titles) ls)) <$> isChargeTitle <*> taxAmountTitles
    isRideFareCharge t = not (t == show Enums.OFFER_DISCOUNT || t == platformFeeTitle || isTaxTitle t || or (($ t) <$> isChargeTitle))

discountableAmount :: HighPrecMoney -> [BreakupLine] -> HighPrecMoney
discountableAmount fare ls =
  max 0 (fare - nonRideCharges - platformFeeBase - platformFeeTax - linesAmount (`elem` nonDiscountableTitles) ls)
  where
    lineSums = componentLines ls
    nonRideCharges = sum (uncurry (+) <$> lineSums) - uncurry (+) lineSums.rideFare
    (platformFeeBase, platformFeeTax) = platformFeeLines ls
