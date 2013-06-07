#!/bin/bash
scripts=$HOME/tr-corpus-one/scripts

run_stop_on_server() {
    server=$1
    ssh $server "$scripts/stop_on_this_node.sh"
}



stop_each() {
    runcount=0
    servcount=0
    for server in $(cat $conf/slaves $conf/masters)
    do
        run_stop_on_server $server
    done
}

echo "stopping on each server"
stop_each

echo "done"
