PAGER="less -S" psql -h localhost -p "${DB_PRIMARY_PORT:-5434}" -d atlas_dev --username atlas
