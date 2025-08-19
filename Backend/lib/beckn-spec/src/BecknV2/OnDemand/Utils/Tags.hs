module BecknV2.OnDemand.Utils.Tags
  ( mkBppTermsTagGroup,
    mkBuyerFinderFeeTagGroup,
    mkSettlementTagGroup,
    mkSettlementDetailsTagGroup,
    BuyerFinderFee,
    City,
    SettlementType,
    SettlementWindow,
  )
where

import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import Kernel.Prelude
import Kernel.Types.Common

type BuyerFinderFee = Text

type City = Text

type SettlementType = Text

type SettlementWindow = Text

-- ONDC compliant
mkBppTermsTagGroup ::
  City ->
  Maybe SettlementType ->
  Maybe Price ->
  Maybe SettlementWindow ->
  Maybe Kernel.Prelude.BaseUrl ->
  Maybe BuyerFinderFee ->
  Spec.TagGroup
mkBppTermsTagGroup txnCity mSettlementType mPrice mSettlementWindow mSettlementTermsUrl mbff = do
  let mAmount = show . (.amount) <$> mPrice
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.BPP_TERMS,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList =
        Just $
          [mkFeePercentageTag mbff]
            <> mkSettlementTags txnCity mAmount mSettlementWindow mSettlementTermsUrl
            <> maybeToList (mkSettlementTypeTag <$> mSettlementType)
    }

-- for backward compatibility
mkBuyerFinderFeeTagGroup :: Maybe BuyerFinderFee -> Spec.TagGroup
mkBuyerFinderFeeTagGroup mbff =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.BUYER_FINDER_FEES,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just [mkFeePercentageTag mbff]
    }

-- for backward compatibility
mkSettlementTagGroup :: City -> Maybe Text -> Maybe SettlementWindow -> Maybe BaseUrl -> Spec.TagGroup
mkSettlementTagGroup txnCity mSettlementAmount mSettlementWindow mSettlementTermsUrl =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.SETTLEMENT_TERMS,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just $ mkSettlementTags txnCity mSettlementAmount mSettlementWindow mSettlementTermsUrl
    }

-- for backward compatibility
mkSettlementDetailsTagGroup :: Maybe SettlementType -> Maybe Spec.TagGroup
mkSettlementDetailsTagGroup mSettlementType = do
  st <- mSettlementType
  return $
    Spec.TagGroup
      { tagGroupDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just $ show Tag.SETTLEMENT_DETAILS,
                descriptorName = Nothing,
                descriptorShortDesc = Nothing
              },
        tagGroupDisplay = Just False,
        tagGroupList = Just [mkSettlementTypeTag st]
      }

mkFeePercentageTag :: Maybe BuyerFinderFee -> Spec.Tag
mkFeePercentageTag mbff =
  Spec.Tag
    { tagDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.BUYER_FINDER_FEES_PERCENTAGE,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagValue = Just $ fromMaybe "0" mbff,
      tagDisplay = Just False
    }

mkSettlementTags :: City -> Maybe Text -> Maybe SettlementWindow -> Maybe BaseUrl -> [Spec.Tag]
mkSettlementTags txnCity mSettlementAmount mSettlementWindow mSettlementTermsUrl =
  catMaybes
    [ mSettlementAmount <&> \samount ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.SETTLEMENT_AMOUNT,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just samount,
            tagDisplay = Just False
          },
      Just $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.SETTLEMENT_WINDOW,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just $ fromMaybe "PT1D" mSettlementWindow,
            tagDisplay = Just False
          },
      Just $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.DELAY_INTEREST,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just "0",
            tagDisplay = Just False
          },
      Just $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.SETTLEMENT_BASIS,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just "INVOICE_RECIEPT",
            tagDisplay = Just False
          },
      Just $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.MANDATORY_ARBITRATION,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just "TRUE",
            tagDisplay = Just False
          },
      Just $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.COURT_JURISDICTION,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just txnCity,
            tagDisplay = Just False
          },
      Just $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.STATIC_TERMS,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just $ maybe "https://api.example-bap.com/booking/terms" showBaseUrl mSettlementTermsUrl,
            tagDisplay = Just False
          }
    ]

mkSettlementTypeTag :: SettlementType -> Spec.Tag
mkSettlementTypeTag st =
  Spec.Tag
    { tagDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.SETTLEMENT_TYPE,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagValue = Just st,
      tagDisplay = Just False
    }
