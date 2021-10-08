module ExternalAPI.Transform where

import Beckn.External.Encryption
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
import qualified Beckn.Types.Core.Rating as CRating
import Beckn.Types.Core.Tag
import qualified Beckn.Types.Core.Tracking as CoreTracking
import Beckn.Types.Id
import Beckn.Types.Mobility.Catalog as Mobility
import Beckn.Types.Mobility.Driver as Mobility
import Beckn.Types.Mobility.Order as Mobility
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as BVehicle
import EulerHS.Prelude
import Types.API.Case
import Types.Storage.Case
import Types.Storage.Organization as Organization
import Types.Storage.Person as Person
import Types.Storage.ProductInstance as ProductInstance
import qualified Types.Storage.Vehicle as Vehicle

mkCatalog :: DBFlow m r => Case -> [ProductInstance] -> ProviderInfo -> m Mobility.Catalog
mkCatalog c prodInsts provider =
  return
    Mobility.Catalog
      { id = getId $ c.id,
        categories = [mkCategory provider],
        brands = [mkBrand provider],
        models = [],
        ttl = Nothing,
        items = mkItem <$> prodInsts,
        offers = [],
        fare_products = []
      }

mkItemDescriptor :: ProductInstance -> Descriptor
mkItemDescriptor prodInst =
  Descriptor
    { name = Nothing,
      code = Just $ show prodInst.vehicleVariant,
      symbol = Nothing,
      short_desc = Nothing,
      long_desc = Nothing,
      images = Nothing,
      audio = Nothing,
      _3d_render = Nothing
    }

mkBrand :: ProviderInfo -> Brand
mkBrand provider =
  Brand
    { id = provider.id,
      descriptor =
        Descriptor
          { name = Just $ provider.name,
            code = Nothing,
            symbol = Nothing,
            short_desc = Nothing,
            long_desc = Nothing,
            images = Nothing,
            audio = Nothing,
            _3d_render = Nothing
          },
      parent_brand_id = Nothing
    }

mkCategory :: ProviderInfo -> Category
mkCategory provider =
  Category
    { id = provider.id,
      descriptor =
        Descriptor
          { name = Just $ provider.name,
            code = Nothing,
            symbol = Nothing,
            short_desc = Nothing,
            long_desc = Nothing,
            images = Nothing,
            audio = Nothing,
            _3d_render = Nothing
          },
      parent_category_id = Nothing,
      tags =
        [ Tag "contacts" (provider.contacts),
          Tag "stats" (provider.stats)
        ]
    }

mkItem :: ProductInstance -> Item
mkItem prodInst =
  Item
    { id = getId $ prodInst.id,
      parent_item_id = Nothing,
      descriptor = mkItemDescriptor prodInst,
      price = mkPrice prodInst,
      promotional = False,
      category_id = Nothing,
      package_category_id = Nothing,
      model_id = Nothing,
      brand_id = Nothing,
      tags =
        mbAttach "discount" (show <$> prodInst.discount) $
          mbAttach "nearestDriverDist" prodInst.udf1 [],
      ttl = Nothing
    }
  where
    mbAttach name mbValue list =
      maybe list (\val -> Tag name val : list) mbValue

mkPrice :: ProductInstance -> Price
mkPrice prodInst =
  let amt = Just $ convertAmountToDecimalValue prodInst.price
   in Price
        { currency = "INR", -- TODO : Fetch this from product
          value = amt,
          estimated_value = amt,
          computed_value = amt,
          listed_value = amt,
          offered_value = amt,
          minimum_value = amt,
          maximum_value = amt
        }

mkOrder :: MonadFlow m => Id ProductInstance -> Maybe Trip -> Maybe CancellationSource -> m Mobility.Order
mkOrder searchPiId trip mbCancellationSource = do
  now <- getCurrentTime
  return
    Mobility.Order
      { id = getId searchPiId,
        items = [OrderItem (getId searchPiId) Nothing],
        created_at = now,
        updated_at = now,
        state = Nothing,
        billing = Nothing,
        payment = Nothing,
        trip = trip,
        cancellation_reason_id = mbCancellationSource,
        cancellation_reasons = [],
        cancellation_policy = Nothing
      }

baseTrackingUrl :: Text
baseTrackingUrl = "http://api.sandbox.beckn.juspay.in/transport/v1/location"

mkTracking :: Text -> Text -> CoreTracking.Tracking
mkTracking method dataUrl =
  CoreTracking.Tracking
    { url = if method == "PULL" then Just dataUrl else Nothing,
      required_params = Nothing,
      metadata = Nothing
    }

mkDriverObj :: EncFlow m r => Person.Person -> m Mobility.Driver
mkDriverObj person = do
  bPerson <- mkPerson person
  return $
    Driver
      { name = bPerson.name,
        image = bPerson.image,
        dob = bPerson.dob,
        organization_name = bPerson.organization_name,
        gender = fromMaybe "" $ bPerson.gender,
        email = bPerson.email,
        phones = bPerson.phones,
        experience = Nothing,
        rating = mkRating,
        registeredAt = person.createdAt
      }
  where
    mkRating = do
      person.rating
        <&> ( \rating ->
                CRating.Rating
                  { value = show rating,
                    unit = "U+2B50",
                    max_value = Just "5",
                    direction = Just "UP"
                  }
            )

mkPerson :: EncFlow m r => Person.Person -> m BPerson.Person
mkPerson person = do
  phone <- getPhone
  return $
    BPerson.Person
      { name =
          Name
            { additional_name = person.middleName,
              family_name = person.lastName,
              given_name = fromMaybe "" (person.firstName),
              call_sign = Nothing,
              honorific_prefix = Nothing,
              honorific_suffix = Nothing
            },
        image = Nothing,
        dob = Nothing,
        organization_name = Nothing,
        gender = Just $ show $ person.gender,
        email = person.email,
        phones = maybeToList phone
      }
  where
    getPhone = do
      decMobNum <- decrypt person.mobileNumber
      return $ case (person.mobileCountryCode, decMobNum) of
        (Just ccode, Just number) -> Just $ ccode <> number
        (Nothing, Just number) -> Just number
        _ -> Nothing

mkVehicleObj :: Vehicle.Vehicle -> BVehicle.Vehicle
mkVehicleObj vehicle =
  BVehicle.Vehicle
    { category = show <$> vehicle.category,
      capacity = vehicle.capacity,
      make = vehicle.make,
      model = vehicle.model,
      size = vehicle.size,
      variant = maybe "" show (vehicle.variant),
      color = vehicle.color,
      energy_type = show <$> vehicle.energyType,
      registration =
        Just
          Registration
            { category = maybe "COMMERCIAL" show (vehicle.registrationCategory),
              number = vehicle.registrationNo
            }
    }

mkProvider :: Organization -> Provider
mkProvider orgInfo =
  Provider
    { id = getId $ orgInfo.id,
      descriptor =
        Descriptor
          { name = Just $ orgInfo.name,
            code = Nothing,
            symbol = Nothing,
            short_desc = Nothing,
            long_desc = orgInfo.info,
            images = Nothing,
            audio = Nothing,
            _3d_render = Nothing
          },
      poc =
        Just $
          BPerson.Person
            { name =
                Name
                  { additional_name = Nothing,
                    family_name = Nothing,
                    given_name = "",
                    call_sign = Nothing,
                    honorific_prefix = Nothing,
                    honorific_suffix = Nothing
                  },
              image = Nothing,
              dob = Nothing,
              organization_name = Just $ orgInfo.name,
              gender = Nothing,
              email = Nothing,
              phones = maybeToList $ orgInfo.mobileNumber
            }
    }
