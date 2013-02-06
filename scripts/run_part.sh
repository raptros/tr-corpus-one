#!/bin/bash

part=bb
file=reverb_lexical_preproc_$part.txt
all_out=$SCRATCH/out

jar_path=$HOME/tr-corpus-one/target/tacc-hadoop-assembly.jar

runit() {
    hadoop jar $jar_path "$@";
}

do_section() {
    section=$1
    output=out_${part}_${section}
    echo "working on section $section"
    hadoop fs -put $SCRATCH/sents/$section.txt sents/$section.txt
    runit trc1.FindRules $file sents/$section.txt $output
    hadoop fs -get $output $all_out/$part/$section/outs
    echo "done; cleaning up"
    hadoop fs -rmr $output
    hadoop fs -rm sents/$section.txt
}

run() {
    echo "starting"
    hadoop fs -put $SCRATCH/$file $file
    for section in apw nyt xie; #afe apw nyt xie
    do do_section $section;
    done;
    echo "done"
}

run

