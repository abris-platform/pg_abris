EXTENSION = pgabris
DATA = pgabris--0.0.1.sql
REGRESS = pgabris_test
PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
