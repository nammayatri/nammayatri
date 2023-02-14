#!/bin/bash
DB=atlas_dev
SCHEMA=$1
USER=$SCHEMA"_user"
CONN_STRING="host=localhost port=5434 user=atlas dbname=atlas_dev password=atlas"
SCHEMA_FILE="$SCHEMA.sql"
SCHEMA_COMMAND_FILE="$SCHEMA.command.sql"

sed "s/SCHEMANAME/$SCHEMA/" ordered_schema_tables.sql > $SCHEMA_COMMAND_FILE

rm $SCHEMA_FILE

echo '-- auxiliary settings' > $SCHEMA_FILE

echo "CREATE USER $USER WITH PASSWORD 'atlas';" >> $SCHEMA_FILE
pg_dump "$CONN_STRING" -sx --schema=$SCHEMA -T "$SCHEMA.*" \
  | grep -iv -e '^--' -e '^$' >> $SCHEMA_FILE
echo "" >> $SCHEMA_FILE

s=0

echo '-- tables' >> $SCHEMA_FILE
psql --tuples-only --csv "$CONN_STRING" -f $SCHEMA_COMMAND_FILE \
  | grep -iv -e 'schema_migrations' \
  | cut -f2 -d, \
  | uniq \
  | while read t; do
  (
  rm -f "$t.sql2";
  pg_dump "$CONN_STRING" -sx -t "$SCHEMA.$t" \
    | grep -i -v -e '^--' -e '^set' -e 'pg_catalog'  \
    | while read a; do
  if [ $s -eq 0 ] ; then
    c=$(echo "$a" | sed -e 's/CREATE TABLE.*/azzup/')
    if [ "w$c" = "wazzup" ] ; then s=1 ; fi
    echo "$a"
  elif [ $s -eq 1 ] ; then
    if [ "a$a" = 'a);' ] ; then s=2 ; else echo $a ; fi
  elif [ $s -eq 2 ] ; then
    b=$(echo "$a" | sed -e 's/\s*ADD CONSTRAINT\(.*\);/CONSTRAINT \1/')
    if [ "a$b" != "a$a" ] ; then
      echo ",$b"
    elif [ "a$a" != "a${a/OWNER TO/}" ] ; then
      echo "$a" >> "$t.sql2"
    elif [ "a$a" != "a${a/TRIGGER/}" ] ; then
      echo $a >> "$t.sql2"
    elif [ "a$a" != "a${a/INDEX/}" ] ; then
      echo "$a" >> "$t.sql2"
    elif [ "a$a" != "a${a/ALTER SEQUENCE/}" ] ; then
      echo "$a" >> "$t.sql2"
    elif [ "a$a" != "a${a/SET DEFAULT/}" ] ; then
      echo "$a" >> "$t.sql2"

    elif [ "a$a" != "a${a/CREATE SEQUENCE/}" ] ; then
      echo "$a" >> "$t.sql2"
      s=3
    fi
  elif [ $s -eq 3 ] ; then
    if [ "a$a" = "a" ] ; then
      echo "" >> "$t.sql2"
      s=2
    else
      echo "$a" >> "$t.sql2"
    fi
  fi

  done
  echo ");"
  ) > "$t.sql"
  if test -f "$t.sql2" ; then cat "$t.sql2" >> "$t.sql"; rm -f "$t.sql2"; fi
  cat "$t.sql" >> $SCHEMA_FILE
  echo '' >> $SCHEMA_FILE
  rm $t.sql
  done

echo '-- necessary data' >> $SCHEMA_FILE
pg_dump "$CONN_STRING" -ax --rows-per-insert=10000 --schema=$SCHEMA -T "$SCHEMA.schema_migrations" \
  | grep -i -v -e '^--' -e '^set' -e 'pg_catalog' -e '^$' >> $SCHEMA_FILE
echo '' >> $SCHEMA_FILE
echo '-- thank you for getting here!' >> $SCHEMA_FILE
rm $SCHEMA_COMMAND_FILE
