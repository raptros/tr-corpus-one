#!/bin/bash --login

conf=$HADOOP_CONF_DIR
candc=$CANDC_HOME
basePort=12200

start() {
    port=$(( $1 + $basePort ))
    nohup $candc/bin/soap_server --models $candc/models --server localhost:$port --candc-printer boxer 1>/dev/null 2>/dev/null &
}

for (( i = 0; i < $CANDC_INSTANCE_COUNT; i++ ))
do 
    start $i
done

exit
