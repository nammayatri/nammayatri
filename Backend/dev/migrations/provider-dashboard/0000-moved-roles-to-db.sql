CREATE TABLE atlas_bpp_dashboard.role (
id character(36) NOT NULL,
name character varying(255) NOT NULL,
dashboard_access_type character varying(255) NOT NULL,
description character varying(1024) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16419_primary PRIMARY KEY (id)
,CONSTRAINT  unique_name UNIQUE (name)
);
ALTER TABLE atlas_bpp_dashboard.role OWNER TO atlas_bpp_dashboard_user;

CREATE TABLE atlas_bpp_dashboard.access_matrix (
id character(36) NOT NULL,
role_id character(36) REFERENCES atlas_bpp_dashboard.role (id) NOT NULL,
api_entity character varying(255) NOT NULL,
user_access_type character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16402_primary PRIMARY KEY (id)
,CONSTRAINT  unique_role_id_api_entity UNIQUE (role_id, api_entity)
);
ALTER TABLE atlas_bpp_dashboard.access_matrix OWNER TO atlas_bpp_dashboard_user;

-- ONLY FOR LOCAL
INSERT INTO atlas_bpp_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
    ('e5a69a26-d165-455a-a711-33a41e0d47c6', 'CUSTOMER', 'DASHBOARD_USER', 'customer', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('508a0bac-258d-44a6-ac55-aef57ab87a76', 'DRIVER', 'DASHBOARD_USER', 'driver', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('d5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'JUSPAY_OPS', 'DASHBOARD_USER', 'Juspay OPS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('37947162-3b5d-4ed6-bcac-08841be1534d', 'JUSPAY_ADMIN', 'DASHBOARD_ADMIN', 'Juspay admin can create and assign other roles', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', 'CUSTOMER_SERVICE', 'DASHBOARD_USER', 'customer service', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('e5a69a26-d165-455a-a711-33a41e0d4812', 'FLEET', 'FLEET_OWNER', 'fleetOwner', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('00000000-0000-0000-rental-fleet-role', 'RENTAL_FLEET', 'RENTAL_FLEET_OWNER', 'rentalFleetOwner', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('00000000-0000-0000-000-operator-role', 'OPERATOR', 'DASHBOARD_OPERATOR', 'dashboardOperator', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00');

-- ONLY FOR LOCAL
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at) VALUES
    ('03b312b7-1899-4708-984f-e4b1e70e8a39', 'e5a69a26-d165-455a-a711-33a41e0d47c6', 'RIDES', 'USER_READ_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('e3e73e71-6e47-4c50-9b23-1f6a6831faeb', '508a0bac-258d-44a6-ac55-aef57ab87a76', 'RIDES', 'USER_READ_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('ac28f052-d158-4a8a-a61f-b560a912282a', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'CUSTOMERS', 'USER_WRITE_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('05548ffb-1a09-4924-bf4c-0939c78c0bc2', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'DRIVERS', 'USER_WRITE_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('85a95c12-e07c-4039-9088-4b2476bb4cab', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'RIDES', 'USER_FULL_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('9daf8530-5e73-4dba-b9a2-a109a83fae1a', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'MONITORING', 'USER_FULL_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('7b26f117-e4cf-423b-8d61-23282f20ad18', '37947162-3b5d-4ed6-bcac-08841be1534d', 'CUSTOMERS', 'USER_FULL_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('7035bab1-1574-410a-8015-c3ba7312d339', '37947162-3b5d-4ed6-bcac-08841be1534d', 'DRIVERS', 'USER_FULL_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('753a1f34-ebf9-42d2-b9fa-e44928e418ae', '37947162-3b5d-4ed6-bcac-08841be1534d', 'RIDES', 'USER_FULL_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('42d554de-5263-44e8-81af-639a04724834', '37947162-3b5d-4ed6-bcac-08841be1534d', 'MONITORING', 'USER_FULL_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('b4759096-35a6-4231-ad2e-e3416513aad5', 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', 'CUSTOMERS', 'USER_READ_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('16e3ce63-6f65-491c-9bef-df58acbd66b2', 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', 'DRIVERS', 'USER_READ_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('dfcf29b7-1244-4ce1-bae1-2554df8a1424', 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', 'RIDES', 'USER_FULL_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('4c9bd917-8906-4a96-9b64-51ea6ea56474', 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', 'MONITORING', 'USER_WRITE_ACCESS', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00');

ALTER TABLE atlas_bpp_dashboard.person
    ADD COLUMN role_id character(36) DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6' NOT NULL;

ALTER TABLE
   atlas_bpp_dashboard.person
ADD
   CONSTRAINT person_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_bpp_dashboard.role(id);

-- ONLY FOR LOCAL
UPDATE atlas_bpp_dashboard.person
    SET role_id = '508a0bac-258d-44a6-ac55-aef57ab87a76'
    WHERE role = 'DRIVER';
UPDATE atlas_bpp_dashboard.person
    SET role_id = 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd'
    WHERE role = 'JUSPAY_OPS';
UPDATE atlas_bpp_dashboard.person
    SET role_id = '37947162-3b5d-4ed6-bcac-08841be1534d'
    WHERE role = 'JUSPAY_ADMIN';
UPDATE atlas_bpp_dashboard.person
    SET role_id = 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa'
    WHERE role = 'CUSTOMER_SERVICE';
UPDATE atlas_bpp_dashboard.person
    SET role_id = 'e5a69a26-d165-455a-a711-33a41e0d4812'
    WHERE role = 'FLEET_OWNER';
UPDATE atlas_bpp_dashboard.person
    SET role_id = '00000000-0000-0000-rental-fleet-role'
    WHERE role = 'RENTAL_FLEET_OWNER';
UPDATE atlas_bpp_dashboard.person
    SET role_id = '00000000-0000-0000-000-operator-role'
    WHERE role = 'OPERATOR';
ALTER TABLE atlas_bpp_dashboard.person
  DROP COLUMN role;
