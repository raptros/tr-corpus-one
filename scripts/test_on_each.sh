#!/bin/bash

conf=$HADOOP_CONF_DIR
candc=$CANDC_HOME
scripts=$HOME/tr-corpus-one/scripts

run_test_on_server() {
    server=$1
    ssh $server "$scripts/test_on_this_node.sh"
}


test_server() { 
    server=$1
    res=$(run_test_on_server $server | grep passed)
    if [[ $res ]]; then return 0; else return 1; fi
}

test_each() {
    runcount=0
    servcount=0
    for server in $(cat $conf/slaves $conf/masters)
    do
        let $(( servcount++ ))
        if test_server $server; then let $(( runcount++ )); fi
    done
    if [[ $runcount == $servcount ]]
    then echo "passed"
    else echo "failed, only $linecount servers passed"
    fi
}

echo "testing"
test_each
