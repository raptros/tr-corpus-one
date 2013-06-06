#!/bin/bash --login

conf=$HADOOP_CONF_DIR
candc=$CANDC_HOME
basePort=9000

test_sentence="i am the very model of a modern major general"

test_instance() {
    port=$(( $1 + $basePort ))
    res=$(echo $test_sentence | $candc/bin/soap_client --url http://localhost:$port 2>/dev/null | $candc/bin/boxer --stdin --semantics fol --box true 2>/dev/null | grep fol)
    if [[ $res ]]
    then return 0
    else return 1
    fi
}

test_each() {
    counter=0
    for (( i = 0; i < $CANDC_INSTANCE_COUNT; i++))
    do
        if test_instance $i
        then let $(( counter++ ))
        fi
    done
    echo $counter
}

test_all() {
    count=$(test_each)
    if [[ $count -eq $CANDC_INSTANCE_COUNT ]]
    then echo "passed"
    else echo "failed - $count instances succeeded"
    fi
}

test_all

exit
