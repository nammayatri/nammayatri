https://stackoverflow.com/questions/36781984/load-postgres-dump-after-docker-compose-up

/docker-entrypoint-initdb.d

Whatever is mapped to is executed.

This is a directly, and we treat whatever is there as a Docker seed.

-v `pwd`/.docker/sql-seed:/docker-entrypoint-initdb.d \


```docker-compose ```

docker-compose up -d

TODO: clear description