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

test_each_server() {
    servers=$(cat $conf/slaves $conf/masters)
    counter=0
    for server in $servers
    do
        if test_server $server
        then let $(( counter++ ))
        fi
    done
    echo $counter
}

test_all() {
    linecount=$(test_each_server)
    servcount=$(cat $conf/masters $conf/slaves|wc -l)
    if [[ $linecount == $servcount ]]
    then echo "passed"
    else echo "failed, only $linecount servers passed"
    fi
}

echo "testing"
test_all
