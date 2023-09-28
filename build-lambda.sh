#!/usr/bin/env fish

docker build -t cisserver .
mkdir -p out
set container_id (docker create cisserver)
docker cp $container_id:/usr/local/bin/cisserver out/
docker rm $container_id
