CREATE TABLE all_types (
  the_bigint                bigint                not null,
  the_bigserial             bigserial             not null,
  the_boolean               boolean               not null,
  the_bytea                 bytea                 not null,
  the_character             character             not null,
  the_characters            character(2)          not null,
  the_varying_character     character varying     not null,
  the_varying_characters    character varying(2)  not null,
  the_date                  date                  not null,
  the_double                double precision      not null,
  the_integer               integer               not null,
  the_json                  json                  not null,
  the_jsonb                 jsonb                 not null,
  the_numeric               numeric               not null,
  the_numerics              numeric(4, 2)         not null,
  the_real                  real                  not null,
  the_smallint              smallint              not null,
  the_smallserial           smallserial           not null,
  the_serial                serial                not null,
  the_text                  text                  not null,
  the_time                  time                  not null,
  the_times                 time(2)               not null,
  the_timestamp             timestamp             not null,
  the_timestamps            timestamp(2)          not null,
  the_uuid                  uuid                  not null,
  the_ulid                  char(16)              not null
)
