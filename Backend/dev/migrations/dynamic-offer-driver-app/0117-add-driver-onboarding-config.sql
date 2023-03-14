CREATE TABLE atlas_driver_offer_bpp.driver_onboarding_config (
  merchant_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_driver_offer_bpp.merchant (id),
  onboarding_try_limit int NOT NULL,
  onboarding_retry_time_in_hours int NOT NULL,
  onboard_support_sms_template text NOT NULL,
  check_rc_insurance_expiry bool NOT NULL,
  check_rc_expiry bool NOT NULL,
  check_rc_vehicle_class bool NOT NULL,
  check_dl_expiry bool NOT NULL,
  check_dl_vehicle_class bool NOT NULL,
  check_image_extraction bool NOT NULL,
  check_image_extraction_for_dashboard bool NOT NULL,
  valid_dl_vehicleclass_infixes text[] NOT NULL
);
WITH OnboardingConfig AS (
  SELECT
    T1.id,
    3,
    24,
    'Driver Onboarding Alert!! Driver is facing following issues while onboarding to ({#org#}). Reasons: {#reasons#}. Please contact him +91-{#driver-phone#}.',
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,

    CAST ('{"AUTORICKSHAW","LMV"
            , "3W-NT"
            , "3WT"
            , "3W-T"
            , "LIGHT MOTOR VEHICLE"
            , "3W-CAB"}' as text[])

  FROM
    atlas_driver_offer_bpp.merchant AS T1
) INSERT INTO atlas_driver_offer_bpp.driver_onboarding_config (
  merchant_id, onboarding_try_limit,
  onboarding_retry_time_in_hours,
  onboard_support_sms_template, check_rc_insurance_expiry,
  check_rc_expiry, check_rc_vehicle_class,
  check_dl_expiry, check_dl_vehicle_class,
  check_image_extraction, check_image_extraction_for_dashboard,
  valid_dl_vehicleclass_infixes
) (
  SELECT
    *
  FROM
    OnboardingConfig
);
