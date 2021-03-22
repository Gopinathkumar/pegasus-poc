#!/bin/sh
docker cp ../jdbc_oracle/ojdbc8.jar spark-master:/spark/jars
docker cp ../jdbc_oracle/ojdbc8.jar spark-worker-1:/spark/jars
docker cp ../jdbc_oracle/ojdbc8.jar spark-worker-2:/spark/jars

