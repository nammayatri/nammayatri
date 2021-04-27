{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Transform where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.Brand
import Beckn.Types.Core.Category
import Beckn.Types.Core.DecimalValue (convertAmountToDecimalValue)
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Item
import Beckn.Types.Core.Order (OrderItem (..))
import Beckn.Types.Core.Person as BPerson
import Beckn.Types.Core.Price
import Beckn.Types.Core.Provider
import Beckn.Types.Core.Tag
import qualified Beckn.Types.Core.Tracking as CoreTracking
import Beckn.Types.Id
import Beckn.Types.Mobility.Catalog as Mobility
import Beckn.Types.Mobility.Driver as Mobility
import Beckn.Types.Mobility.Order as Mobility
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as BVehicle
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Organization as Organization
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common (fromMaybeM)
import Data.Text as T
import EulerHS.Prelude
import Types.API.Case
import Types.Error

mkCatalog :: Case -> [ProductInstance] -> ProviderInfo -> Flow Mobility.Catalog
mkCatalog c prodInsts provider =
  return
    Mobility.Catalog
      { _id = getId $ c ^. #_id,
        _categories = [mkCategory provider],
        _brands = [mkBrand provider],
        _models = [],
        _ttl = Nothing,
        _items = mkItem <$> prodInsts,
        _offers = [],
        _fare_products = []
      }

mkItemDescriptor :: ProductInstance -> Descriptor
mkItemDescriptor _prodInst =
  Descriptor
    { _name = Nothing,
      _code = Nothing,
      _symbol = Nothing,
      _short_desc = Nothing,
      _long_desc = Nothing,
      _images = Nothing,
      _audio = Nothing,
      _3d_render = Nothing
    }

mkBrand :: ProviderInfo -> Brand
mkBrand provider =
  Brand
    { _id = provider ^. #_id,
      _descriptor =
        Descriptor
          { _name = Just $ provider ^. #_name,
            _code = Nothing,
            _symbol = Nothing,
            _short_desc = Nothing,
            _long_desc = Nothing,
            _images = Nothing,
            _audio = Nothing,
            _3d_render = Nothing
          },
      _parent_brand_id = Nothing
    }

mkCategory :: ProviderInfo -> Category
mkCategory provider =
  Category
    { _id = provider ^. #_id,
      _descriptor =
        Descriptor
          { _name = Just $ provider ^. #_name,
            _code = Nothing,
            _symbol = Nothing,
            _short_desc = Nothing,
            _long_desc = Nothing,
            _images = Nothing,
            _audio = Nothing,
            _3d_render = Nothing
          },
      _parent_category_id = Nothing,
      _tags =
        [ Tag "contacts" (provider ^. #_contacts),
          Tag "stats" (provider ^. #_stats)
        ]
    }

mkItem :: ProductInstance -> Item
mkItem prodInst =
  Item
    { _id = getId $ prodInst ^. #_id,
      _parent_item_id = Nothing,
      _descriptor = mkItemDescriptor prodInst,
      _price = mkPrice prodInst,
      _promotional = False,
      _category_id = Nothing,
      _package_category_id = Nothing,
      _model_id = Nothing,
      _brand_id = Nothing,
      _tags = [],
      _ttl = Nothing
    }

mkPrice :: ProductInstance -> Price
mkPrice prodInst =
  let amt = convertAmountToDecimalValue <$> prodInst ^. #_price
   in Price
        { _currency = "INR", -- TODO : Fetch this from product
          _value = amt,
          _estimated_value = amt,
          _computed_value = amt,
          _listed_value = amt,
          _offered_value = amt,
          _minimum_value = amt,
          _maximum_value = amt
        }

mkOrder :: Case -> ProductInstance -> Maybe Trip -> Flow Mobility.Order
mkOrder _c pri trip = do
  now <- getCurrentTime
  searchPiId <- pri ^. #_parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  return
    Mobility.Order
      { _id = getId searchPiId,
        _items = [OrderItem (getId $ pri ^. #_productId) Nothing],
        _created_at = now,
        _updated_at = now,
        _state = Nothing,
        _billing = Nothing,
        _payment = Nothing,
        _trip = trip,
        _cancellation_reason_id = Nothing,
        _cancellation_reasons = [],
        _cancellation_policy = Nothing
      }

baseTrackingUrl :: Text
baseTrackingUrl = "http://api.sandbox.beckn.juspay.in/transport/v1/location"

mkTracking :: Text -> Text -> CoreTracking.Tracking
mkTracking method dataUrl =
  CoreTracking.Tracking
    { _url = if method == "PULL" then Just dataUrl else Nothing,
      _required_params = Nothing,
      _metadata = Nothing
    }

mkDriverObj :: Person.Person -> Mobility.Driver
mkDriverObj person =
  let bPerson = mkPerson person
   in Driver
        { name = bPerson ^. #name,
          image = bPerson ^. #image,
          dob = bPerson ^. #dob,
          organization_name = bPerson ^. #organization_name,
          gender = fromMaybe "" $ bPerson ^. #gender,
          email = bPerson ^. #email,
          phones = bPerson ^. #phones,
          experience = Nothing,
          rating = Nothing
        }

mkPerson :: Person.Person -> BPerson.Person
mkPerson person =
  BPerson.Person
    { name =
        Name
          { _additional_name = person ^. #_middleName,
            _family_name = person ^. #_lastName,
            _given_name = fromMaybe "" (person ^. #_firstName),
            _call_sign = Nothing,
            _honorific_prefix = Nothing,
            _honorific_suffix = Nothing
          },
      image = Nothing,
      dob = Nothing,
      organization_name = Nothing,
      gender = Just $ show $ person ^. #_gender,
      email = person ^. #_email,
      phones = maybeToList getPhone
    }
  where
    getPhone =
      case (person ^. #_mobileCountryCode, person ^. #_mobileNumber) of
        (Just ccode, Just number) -> Just $ ccode <> number
        (Nothing, Just number) -> Just number
        _ -> Nothing

mkVehicleObj :: Vehicle.Vehicle -> BVehicle.Vehicle
mkVehicleObj vehicle =
  BVehicle.Vehicle
    { category = show <$> vehicle ^. #_category,
      capacity = vehicle ^. #_capacity,
      make = vehicle ^. #_make,
      model = vehicle ^. #_model,
      size = vehicle ^. #_size,
      variant = maybe "" show (vehicle ^. #_variant),
      color = vehicle ^. #_color,
      energy_type = show <$> vehicle ^. #_energyType,
      registration =
        Just
          Registration
            { category = maybe "COMMERCIAL" show (vehicle ^. #_registrationCategory),
              number = vehicle ^. #_registrationNo
            }
    }

mkProvider :: Organization -> Provider
mkProvider orgInfo =
  Provider
    { _id = getId $ orgInfo ^. #_id,
      _descriptor =
        Descriptor
          { _name = Just $ orgInfo ^. #_name,
            _code = Nothing,
            _symbol = Nothing,
            _short_desc = Nothing,
            _long_desc = orgInfo ^. #_info,
            _images = Nothing,
            _audio = Nothing,
            _3d_render = Nothing
          },
      _poc =
        Just $
          BPerson.Person
            { name =
                Name
                  { _additional_name = Nothing,
                    _family_name = Nothing,
                    _given_name = "",
                    _call_sign = Nothing,
                    _honorific_prefix = Nothing,
                    _honorific_suffix = Nothing
                  },
              image = Nothing,
              dob = Nothing,
              organization_name = Just $ orgInfo ^. #_name,
              gender = Nothing,
              email = Nothing,
              phones = maybeToList $ orgInfo ^. #_mobileNumber
            }
    }
