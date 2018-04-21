EXTENSION = pg_abris
DATA = pg_abris--0.0.1.sql
REGRESS = pg_abris_test
PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
