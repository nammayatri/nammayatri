{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SharedLogic.TicketRule.Core where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Time (Day (..), TimeOfDay (..), fromGregorian)
import GHC.Generics
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data Rule = Rule
  { condition :: Condition,
    action :: Action,
    timezone :: Int
  }
  deriving (Show, Eq, Generic, Ord, ToSchema)

-- | Represents days of the week
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq, Generic, Ord, ToSchema)

instance ToJSON Weekday

instance FromJSON Weekday

-- | Represents complex booking conditions
data Condition
  = And [Condition]
  | Or [Condition]
  | Not Condition
  | TimeOfDayRange [(TimeOfDay, TimeOfDay)]
  | Weekday [Weekday]
  | DateRange [(Day, Day)]
  | Dates [Day]
  | DaysBeforeBooking Comparator Int
  | RemainingCapacity Comparator Int
  | BookedCount Comparator Int
  deriving (Show, Eq, Generic, Ord, ToSchema)

data BusinessHourType = Slot TimeOfDay | Duration TimeOfDay TimeOfDay deriving (Show, Eq, Generic, Ord, ToJSON, FromJSON, ToSchema)

data BusinessHourDef = BusinessHourDef
  { btype :: BusinessHourType,
    bookingClosingTime :: Maybe TimeOfDay
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToSchema)

data OverrideBusinessHour = OverrideBusinessHour
  { serviceId :: Text,
    businessHours :: [BusinessHourDef]
  }
  deriving (Show, Eq, Generic, Ord, ToSchema, FromJSON, ToJSON)

data Comparator = LessThan | LessThanOrEqual | Equal | GreaterThanOrEqual | GreaterThan
  deriving (Show, Eq, Generic, Ord, ToSchema)

instance FromJSON Comparator where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { constructorTagModifier = camelTo2 '_'
        }

instance ToJSON Comparator where
  toJSON =
    genericToJSON $
      defaultOptions
        { constructorTagModifier = camelTo2 '_'
        }

-- | Serializing Condition to JSON
instance ToJSON Condition where
  toJSON (And conditions) = object ["type" .= ("and" :: Text), "value" .= conditions]
  toJSON (Or conditions) = object ["type" .= ("or" :: Text), "value" .= conditions]
  toJSON (Not condition) = object ["type" .= ("not" :: Text), "value" .= condition]
  toJSON (TimeOfDayRange ranges) = object ["type" .= ("time_of_day_range" :: Text), "value" .= ranges]
  toJSON (Weekday weekdays) = object ["type" .= ("weekday" :: Text), "value" .= weekdays]
  toJSON (DateRange ranges) = object ["type" .= ("date_range" :: Text), "value" .= ranges]
  toJSON (Dates dates) = object ["type" .= ("dates" :: Text), "value" .= dates]
  toJSON (DaysBeforeBooking comp days) = object ["type" .= ("days_before_booking" :: Text), "comparator" .= comp, "value" .= days]
  toJSON (RemainingCapacity comp capacity) = object ["type" .= ("remaining_capacity" :: Text), "comparator" .= comp, "value" .= capacity]
  toJSON (BookedCount comp count) = object ["type" .= ("booked_count" :: Text), "comparator" .= comp, "value" .= count]

-- | Deserializing Condition from JSON
instance FromJSON Condition where
  parseJSON = withObject "Condition" $ \obj -> do
    type_ <- obj .: "type" :: Parser Text
    case type_ of
      "and" -> And <$> obj .: "value"
      "or" -> Or <$> obj .: "value"
      "not" -> Not <$> obj .: "value"
      "time_of_day_range" -> TimeOfDayRange <$> obj .: "value"
      "weekday" -> Weekday <$> obj .: "value"
      "date_range" -> DateRange <$> obj .: "value"
      "dates" -> Dates <$> obj .: "value"
      "days_before_booking" -> DaysBeforeBooking <$> obj .: "comparator" <*> obj .: "value"
      "remaining_capacity" -> RemainingCapacity <$> obj .: "comparator" <*> obj .: "value"
      "booked_count" -> BookedCount <$> obj .: "comparator" <*> obj .: "value"
      _ -> fail $ "Unknown condition type: " ++ show (type_ :: Text)

-- |
-- JSON Structure Documentation for Condition Type
--
-- The Condition type can be serialized to and deserialized from JSON with the following structure:
--
-- 1. And Condition:
--    {
--      "type": "and",
--      "value": [condition1, condition2, ...]
--    }
--
-- 2. Or Condition:
--    {
--      "type": "or",
--      "value": [condition1, condition2, ...]
--    }
--
-- 3. Not Condition:
--    {
--      "type": "not",
--      "value": condition
--    }
--
-- 4. TimeOfDayRange Condition:
--    {
--      "type": "time_of_day_range",
--      "value": [
--        ["09:00:00", "17:00:00"],
--        ["20:00:00", "23:00:00"]
--      ]
--    }
--
-- 5. Weekday Condition:
--    {
--      "type": "weekday",
--      "value": ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
--    }
--
-- 6. DateRange Condition:
--    {
--      "type": "date_range",
--      "value": [
--        ["2024-01-01", "2024-12-31"]
--      ]
--    }
--
-- 7. Dates Condition:
--    {
--      "type": "dates",
--      "value": ["2024-01-01", "2024-01-02", "2024-01-03"]
--    }
--
-- 8. DaysBeforeBooking Condition:
--    {
--      "type": "days_before_booking",
--      "comparator": "less_than",
--      "value": 2
--    }
--
-- 9. RemainingCapacity Condition:
--    {
--      "type": "remaining_capacity",
--      "comparator": "greater_than",
--      "value": 5
--    }
--
-- 10. BookedCount Condition:
--    {
--      "type": "booked_count",
--      "comparator": "less_than_or_equal",
--      "value": 10
--    }
--
-- Comparator values can be one of: "less_than", "less_than_or_equal", "equal", "greater_than_or_equal", "greater_than"
--
-- Example of a complex nested condition:
-- {
--   "type": "and",
--   "value": [
--     {
--       "type": "or",
--       "value": [
--         {
--           "type": "time_of_day_range",
--           "value": [["09:00:00", "17:00:00"]]
--         },
--         {
--           "type": "time_of_day_range",
--           "value": [["20:00:00", "23:00:00"]]
--         }
--       ]
--     },
--     {
--       "type": "weekday",
--       "value": ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
--     },
--     {
--       "type": "not",
--       "value": {
--         "type": "days_before_booking",
--         "comparator": "less_than",
--         "value": 2
--       }
--     },
--     {
--       "type": "remaining_capacity",
--       "comparator": "greater_than",
--       "value": 5
--     },
--     {
--       "type": "date_range",
--       "value": [["2024-01-01", "2024-12-31"]]
--     }
--   ]
-- }

-- | Test function to demonstrate JSON serialization and deserialization
testTicketRuleJSON :: IO ()
testTicketRuleJSON = do
  -- Create a complex condition
  let complexCondition =
        And
          [ Or
              [ TimeOfDayRange [(TimeOfDay 9 0 0, TimeOfDay 17 0 0)],
                TimeOfDayRange [(TimeOfDay 20 0 0, TimeOfDay 23 0 0)]
              ],
            Weekday [Monday, Tuesday, Wednesday, Thursday, Friday],
            Not (DaysBeforeBooking LessThanOrEqual 2),
            DateRange [(fromGregorian 2024 1 1, fromGregorian 2024 12 31)]
          ]

  -- Convert to JSON
  let jsonValue = encode complexCondition
  putTextLn "JSON representation:"
  putTextLn $ decodeUtf8 jsonValue

  -- Convert back from JSON
  case eitherDecode jsonValue of
    Left err -> putTextLn $ "Error decoding JSON: " <> show err
    Right decodedCondition -> do
      putTextLn "\nDecoded condition:"
      putTextLn $ show decodedCondition
      putTextLn "\nVerification:"
      putTextLn $
        if decodedCondition == complexCondition
          then "Success: Original and decoded conditions match!"
          else "Error: Original and decoded conditions don't match!"

data ActionType
  = Open
  | Closed
  | PricingIncreaseBy HighPrecMoney
  | PriceDecreaseBy HighPrecMoney
  | PriceSet HighPrecMoney
  | ChangeDescription Text
  | OverrideBusinessHours OverrideBusinessHour
  deriving (Show, Eq, Generic, Ord, ToSchema)

instance ToJSON ActionType where
  toJSON Open = object ["type" .= ("open" :: Text)]
  toJSON Closed = object ["type" .= ("closed" :: Text)]
  toJSON (PricingIncreaseBy amount) =
    object
      [ "type" .= ("pricing_increase_by" :: Text),
        "value" .= amount
      ]
  toJSON (PriceDecreaseBy amount) =
    object
      [ "type" .= ("price_decrease_by" :: Text),
        "value" .= amount
      ]
  toJSON (PriceSet amount) =
    object
      [ "type" .= ("price_set" :: Text),
        "value" .= amount
      ]
  toJSON (ChangeDescription description) =
    object
      [ "type" .= ("change_description" :: Text),
        "value" .= description
      ]
  toJSON (OverrideBusinessHours overrideBusinessHour) =
    object
      [ "type" .= ("override_business_hours" :: Text),
        "value" .= overrideBusinessHour
      ]

instance FromJSON ActionType where
  parseJSON = withObject "ActionType" $ \obj -> do
    type_ <- obj .: "type" :: Parser Text
    case type_ of
      "open" -> pure Open
      "closed" -> pure Closed
      "pricing_increase_by" -> PricingIncreaseBy <$> obj .: "value"
      "price_decrease_by" -> PriceDecreaseBy <$> obj .: "value"
      "price_set" -> PriceSet <$> obj .: "value"
      "change_description" -> ChangeDescription <$> obj .: "value"
      "override_business_hours" -> OverrideBusinessHours <$> obj .: "value"
      _ -> fail $ "Unknown action type: " ++ show type_

-- | Represents an action that can be performed
data Action = Action
  { actionType :: ActionType
  }
  deriving (Show, Eq, Generic, Ord, ToSchema)

instance ToJSON Action

instance FromJSON Action

-- |
-- JSON Structure Documentation for Action Type
--
-- The Action type can be serialized to and deserialized from JSON with the following structure:
--
-- 1. Open Action:
--    {
--      "actionType": {
--        "type": "open"
--      }
--    }
--
-- 2. Closed Action:
--    {
--      "actionType": {
--        "type": "closed"
--      }
--    }
--
-- 3. Price Increase Action:
--    {
--      "actionType": {
--        "type": "pricing_increase_by",
--        "value": 1.5
--      }
--    }
--
-- 4. Price Decrease Action:
--    {
--      "actionType": {
--        "type": "price_decrease_by",
--        "value": 0.5
--      }
--    }
--
-- 5. Price Set Action:
--    {
--      "actionType": {
--        "type": "price_set",
--        "value": 100.0
--      }
--    }
--
-- 6. Change Description Action:
--    {
--      "actionType": {
--        "type": "change_description",
--        "value": "New route description"
--      }
--    }
--
-- 7. Override Business Hours Action:
--    {
--      "actionType": {
--        "type": "override_business_hours",
--        "value": {
--          "serviceId": "string_service_id",
--          "businessHours": [
--            { -- Example with Slot type
--              "btype": {
--                 "tag": "Slot",
--                 "contents": "09:00:00"
--              },
--              "bookingClosingTime": "08:30:00" -- Optional: null if not present
--            },
--            { -- Example with Duration type
--              "btype": {
--                 "tag": "Duration",
--                 "contents": ["10:00:00", "18:00:00"]
--              },
--              "bookingClosingTime": null -- Optional: null if not present
--            }
--            // ... potentially more business hour definitions
--          ]
--        }
--      }
--    }
--
-- Note:
-- - For pricing_increase_by and price_decrease_by, the value represents a multiplier (e.g., 1.5 means 50% increase, 0.5 means 50% decrease)
-- - For price_set, the value represents the absolute price to set

-- | Test function for Action JSON serialization
testActionJSON :: IO ()
testActionJSON = do
  let actions =
        [ Action Open,
          Action Closed,
          Action (PricingIncreaseBy 1.5),
          Action (PriceDecreaseBy 0.5),
          Action (PriceSet 100.0)
        ]

  putTextLn "JSON representation of actions:"
  forM_ actions $ \action -> do
    let jsonValue = encode action
    putTextLn $ decodeUtf8 jsonValue

    case eitherDecode jsonValue of
      Left err -> putTextLn $ "Error decoding JSON: " <> show err
      Right decodedAction -> do
        putTextLn $
          if decodedAction == action
            then "Success: Original and decoded actions match!"
            else "Error: Original and decoded actions don't match!"
        putTextLn ""

instance ToJSON Rule where
  toJSON (Rule condition action timezone) =
    object
      [ "condition" .= condition,
        "action" .= action,
        "timezone" .= timezone
      ]

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \obj -> do
    condition <- obj .: "condition"
    action <- obj .: "action"
    timezone <- obj .: "timezone"
    pure $ Rule condition action timezone

$(mkHttpInstancesForEnum ''Weekday)

-- |
-- JSON Structure Documentation for Rule Type
--
-- The Rule type can be serialized to and deserialized from JSON with the following structure:
--
-- {
--   "condition": {
--     // Condition JSON structure as defined in Condition type
--   },
--   "action": {
--     // Action JSON structure as defined in Action type
--   },
--   "timezone": 330  // Timezone as minutes offset from UTC (e.g., 330 for IST which is +5:30)
-- }
--
-- Example:
-- {
--   "condition": {
--     "type": "and",
--     "value": [
--       {
--         "type": "weekday",
--         "value": ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
--       },
--       {
--         "type": "time_of_day_range",
--         "value": [["09:00:00", "17:00:00"]]
--       }
--     ]
--   },
--   "action": {
--     "actionType": {
--       "type": "pricing_increase_by",
--       "value": 1.5
--     }
--   },
--   "timezone": 330
-- }

-- | Test function for Rule JSON serialization
testRuleJSON :: IO ()
testRuleJSON = do
  let rule =
        Rule
          ( And
              [ DaysBeforeBooking Equal 0,
                TimeOfDayRange [(TimeOfDay 17 0 0, TimeOfDay 23 59 59)]
              ]
          )
          (Action (Closed))
          330 -- IST timezone (+5:30)
  putTextLn "JSON representation of rule:"
  let jsonValue = encode rule
  putTextLn $ decodeUtf8 jsonValue

  case eitherDecode jsonValue of
    Left err -> putTextLn $ "Error decoding JSON: " <> show err
    Right decodedRule -> do
      putTextLn "\nDecoded rule:"
      putTextLn $ show decodedRule
      putTextLn "\nVerification:"
      putTextLn $
        if decodedRule == rule
          then "Success: Original and decoded rules match!"
          else "Error: Original and decoded rules don't match!"

-- Test function for a single specific Rule instance (matching user's example)
testSingleRuleJSON :: IO ()
testSingleRuleJSON = do
  let exampleRule =
        Rule
          { condition = Weekday [Monday, Tuesday, Wednesday, Thursday, Friday],
            action =
              Action
                { actionType = PricingIncreaseBy 10
                },
            timezone = 330
          }

  putTextLn "\n--- Testing Single Rule (User Example) using toJSON/fromJSON ---"
  putTextLn "Original Rule Value:"
  print exampleRule

  putTextLn "\nConverting to Aeson Value using toJSON:"
  let aesonValue = toJSON exampleRule
  -- For printing the Value as a JSON string:
  putTextLn "Aeson Value (encoded to JSON string for display):"
  putTextLn $ decodeUtf8 $ encode aesonValue

  putTextLn "\nAttempting to parse Aeson Value back to Rule using fromJSON (via parseEither parseJSON):"
  case parseEither parseJSON aesonValue of
    Left err -> putTextLn $ "Error parsing Aeson Value: " <> show err
    Right decodedRule -> do
      putTextLn "\nParsed rule value:"
      print decodedRule
      putTextLn "\nVerification of single rule (toJSON/fromJSON):"
      if decodedRule == exampleRule
        then putTextLn "Success: Original and parsed single rule match!"
        else putTextLn "Error: Original and parsed single rule do NOT match!"
  putTextLn "--- End of Single Rule Test (toJSON/fromJSON) ---"
