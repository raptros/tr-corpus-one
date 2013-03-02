#!/bin/bash

conf=$HOME/tacc-hadoop/hadoop-conf/
candc=$HOME/install/candc

start_all() {
    cmd="nohup $candc/bin/soap_server --models $candc/models --server localhost:9000 --candc-printer boxer 1>/dev/null 2>/dev/null"
    for server in $(cat $conf/slaves $conf/masters);
    do
        ssh $server $cmd&
    done
}

test_all() {
    test_sentence="i am the very model of a modern major general"
    test_cmd="echo $test_sentence|$candc/bin/soap_client --url http://localhost:9000|$candc/bin/boxer --stdin --semantics fol --box true 2>/dev/null|tail -n 1"
    linecount=$(for server in $(cat $conf/slaves $conf/masters)
    do
        ssh $server $test_cmd
    done|wc -l)
    servcount=$(cat $conf/masters $conf/slaves|wc -l)
    if [[ $linecount == $servcount ]]
    then echo "passed"
    else echo "failed, only $linecount servers passed"
    fi
}

main() {
    #echo "starting on each server"
    #start_all
    #sleep 20
    echo "testing"
    test_all
}

main
