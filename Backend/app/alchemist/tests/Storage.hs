storageDSLInput =
  [s|
    Table "TicketService" "ticket_service"
    Field "id" "Id Domain.Types.TicketService.TicketService" "CHARACTER(36)" PrimaryKey NotNull,
    Field "placesId" "Text" "CHARACTER(36)" PrimaryKey NotNull,
    Field "service" "Text" "VARCHAR(50)" SecondaryKey NotNull,
    Field "maxVerification" "Int" "INT" NotNull Default 1,
    Field "openTimings" "Maybe TimeOfDay" "time without time zone" Default CURRENT_TIMESTAMP,
    Field "closeTimings" "Maybe TimeOfDay" "time without time zone",
    Field "validityTimings" "Maybe TimeOfDay" "time without time zone"
|]

-- field name
-- field type (Haskell, SQL (optional))
-- default value in SQL (optional)
-- PRIMARY_KEY
-- SECONDARY_KEY
-- imports
