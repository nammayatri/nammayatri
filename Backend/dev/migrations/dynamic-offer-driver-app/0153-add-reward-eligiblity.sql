CREATE TABLE atlas_driver_offer_bpp.reward_eligiblity (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   reward_id CHARACTER (36) NOT NULL,
   driver_id character varying(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person(id),
   quantity integer DEFAULT 0 NOT NULL,
   quantity_unit CHARACTER VARYING(255),
   collected boolean DEFAULT false NOT NULL,
   collected_at TIMESTAMP WITH TIME ZONE,
   created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
   updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
    ,CONSTRAINT  RewardEligiblity_reward_id_fkey FOREIGN KEY (reward_id) REFERENCES atlas_driver_offer_bpp.rewards(id)
);