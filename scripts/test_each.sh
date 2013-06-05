#!/bin/bash

conf=$HADOOP_CONF_DIR
candc=$CANDC_HOME

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
    echo "testing"
    test_all
}

main
