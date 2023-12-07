import psycopg2
from psycopg2 import sql
from dotenv import load_dotenv
import hashlib
import os
import uuid

load_dotenv()

def eval_hash(data, salt):
    combined_data = salt.encode('utf-8') + data.encode('utf-8')
    hash_object = hashlib.sha256(combined_data)
    return hash_object.hexdigest()

# Database connection parameters
DB_HOST = os.getenv('DB_HOST')
DB_PORT = os.getenv('DB_PORT')
DB_NAME = os.getenv('DB_NAME')
DB_USER = os.getenv('DB_USER')
DB_PASSWORD = os.getenv('DB_PASSWORD')

# Connection to the database
connection = psycopg2.connect(
    host=DB_HOST,
    port=DB_PORT,
    database=DB_NAME,
    user=DB_USER,
    password=DB_PASSWORD
)

NUM_DRIVERS = os.getenv('NUM_DRIVERS')

# Cursor to execute SQL queries
cursor = connection.cursor()

for i in range(1, int(NUM_DRIVERS)):
    user_id = str(uuid.uuid4())
    mobile_number = f"7777777{i:03d}"
    input_data = mobile_number
    input_salt = "How wonderful it is that nobody need wait a single moment before starting to improve the world"
    hash = eval_hash(input_data, input_salt)

    # Define values for the queries
    person_values = {
        "id": user_id,
        "first_name": f"Driver-{i}",
        "unencrypted_mobile_number": input_data,
        "mobile_number_hash": r'\x' + hash,
    }

    driver_info_values = {
        "driver_id": user_id,
    }

    driver_vehicle_values = {
        "driver_id": user_id,
        "reg_no": f"{i:04d}",
    }

    driver_stats_values = {
        "driver_id": user_id,
    }

    # SQL queries
    person_sql_query = sql.SQL("""
        INSERT INTO atlas_driver_offer_bpp.person (
            id, first_name, middle_name, last_name, role, gender,
            identifier_type, email, password_hash, mobile_number_encrypted,
            mobile_number_hash, mobile_country_code, identifier, is_new,
            merchant_id, device_token, description, created_at, updated_at,
            rating, language, client_version, bundle_version,
            unencrypted_mobile_number, whatsapp_notification_enroll_status,
            unencrypted_alternate_mobile_number,
            alternate_mobile_number_encrypted, alternate_mobile_number_hash,
            hometown, languages_spoken, onboarded_from_dashboard,
            face_image_id, merchant_operating_city_id
        ) VALUES (
            %(id)s, %(first_name)s, NULL, NULL, 'DRIVER', 'MALE',
            'MOBILENUMBER', NULL, NULL,
            '0.1.0|0|Oo+V+gWyRfCb0lakCHTmw5sCsI5sWiMqffGhHnbN7VskRrz14sx0vYamPlykAy9RrxaE1dPBGYkRuoLufQ==', %(mobile_number_hash)s, '+91', NULL,
            false, 'favorit0-0000-0000-0000-00000favorit', NULL, NULL,
            '2023-11-17 07:39:53.071774+00', '2023-11-17 07:54:11.274128+00',
            NULL, NULL, NULL, NULL, %(unencrypted_mobile_number)s,
            NULL, NULL,  NULL, NULL,  NULL, '{}', false, NULL, NULL
        );
    """)

    driver_info_sql_query = sql.SQL("""
        INSERT INTO atlas_driver_offer_bpp.driver_information (
            driver_id, active, on_ride,  created_at, updated_at, enabled, verified, referral_code, admin_id,
            blocked,last_enabled_on, can_downgrade_to_hatchback, can_downgrade_to_sedan, can_downgrade_to_taxi,
            mode, merchant_id, num_of_locks, aadhaar_verified, subscribed,
            payment_pending, blocked_reason, block_expiry_time, auto_pay_status, comp_aadhaar_image_path,
            available_upi_apps, payer_vpa, enabled_at
        ) VALUES (
            %(driver_id)s, true, false, '2023-11-17 07:39:53.071774+00', '2023-11-17 07:39:53.071774+00',true, false,
            NULL, NULL, false, NULL, false,false,false,NULL,NULL, 0, false, true, false, NULL, NULL, NULL,NULL,NULL, NULL, NULL
        );
    """)

    driver_vehicle_sql_query = sql.SQL("""
        INSERT INTO atlas_driver_offer_bpp.vehicle (
            driver_id, capacity, category, make, model,
            size, variant, color, energy_type, registration_no,
            registration_category, merchant_id,created_at, updated_at, vehicle_class, vehicle_name
        ) VALUES (
            %(driver_id)s, 5, NULL, NULL, 'Tahoe', NULL, 'SUV', 'Green', NULL, %(reg_no)s , NULL, 'favorit0-0000-0000-0000-00000favorit',
            '2023-11-17 07:39:53.071774+00', '2023-11-17 07:39:53.071774+00', '3WT', NULL
        );
    """)

    driver_stats_sql_query = sql.SQL("""
        INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id, idle_since)
        VALUES (
            %(driver_id)s, '2022-04-12 15:15:42.283174+00'
        );
    """)

    # Execute queries
    cursor.execute(person_sql_query, person_values)
    connection.commit()
    cursor.execute(driver_info_sql_query, driver_info_values)
    connection.commit()
    cursor.execute(driver_vehicle_sql_query, driver_vehicle_values)
    connection.commit()
    cursor.execute(driver_stats_sql_query, driver_stats_values)
    connection.commit()
