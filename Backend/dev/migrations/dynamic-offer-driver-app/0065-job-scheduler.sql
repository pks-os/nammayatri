CREATE TABLE atlas_driver_offer_bpp.job (
  id character varying(255) PRIMARY KEY,
  job_type character varying(255) NOT NULL,
  job_data text NOT NULL,
  scheduled_at timestamp NOT NULL,
  job_expire_at timestamp,
  maximum_delay int,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  max_errors int NOT NULL,
  curr_errors int NOT NULL,
  status character varying(255) NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.job OWNER TO atlas_driver_offer_bpp_user;
