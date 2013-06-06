#!/bin/bash

conf=$HADOOP_CONF_DIR
candc=$CANDC_HOME
port=9000
scripts=$HOME/tr-corpus-one/scripts

start_one() {
    serv=$1
    echo "starting on $serv"
    ssh $server "$scripts/start_on_this_node.sh"
}

start_each() {
    servers=$(cat $conf/slaves $conf/masters);
    for server in $servers
    do
        start_one $server
    done
}

test_each() {
    $scripts/test_on_each.sh
}

echo "starting on each server"
start_each

echo "started, sleeping"

sleep 10
test_each

echo "done"
