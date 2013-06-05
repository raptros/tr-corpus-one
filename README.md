# tr-corpus-one

it converts lexical inference rules into first order logic inference rules by examining how the rules transform logical representations of a corpus of sentences.

## inputs
there are two inputs
* a set of lexical inference rules. they have a left hand side, a right hand side, and a weight.
* a bunch of sentences. use gigaword.

### preprocessing the rules
because this uses hadoop, each rule needs to be on its own line. also, each rule will need to be numbered. i think i used 
```
nl -w 8 -p -sTAB -nln <file>
```
to do the job. the number of digits really depends on how many rules there are, though.

I based the code on [Jonathan Berant's ruleset][berant]; you can see how the format works in the [readme][breadme]. for other formats, you will have to figure out how to make it apply correctly on your own. the main things to account for are
* how to extract the left hand side so that it can be matched against sentences
* how to apply regular expressions to transform sentences

once you've got the rule format dealt with, you'll need to clean some stuff out of it. in particular
* remove all of the passivization rules (e.g. rules like "6       abduct  be abducted by@R@       10000.0") and rules that only affect articles "19194   be the characteristic of@R@     be characteristic of@R@ 10000.0")
* use the other app included in [the TrCorpus.scala][src/main/scala/TrCorpus.scala] to remove remove rules that translate between single content words only. this app takes two inputs - the stopwords file and the rules file - and produces two outputs: the single-content word rules in FOL, and the remaining rules to be run through MaxTransform.

### preparing sentences
every sentence needs to be on its own line. other than that, you can (and probably should) split your sentence data into multiple sections. if you hand scoobi a directory, it will group all the files in the directory into a single dataset, which makes it easy to run on splits.

## cluster
the main pipeline in this code is built using [scoobi][], a library designed to make it easy for scala code to work with hadoop. the primary construct there is the distributed list, and you should read the documentation on that library to understand how it works.

### TACC
i used the TACC resource for running this. if you're using tacc, there is [a resource][tacc-hadoop] you can use to set up hadoop clusters for running this code on. start by reading the [tacc-hadoop readme] to set up your account and start your cluster.

once you've successfully done the cluster test in that guide, you can use this package easily. you can then clone this repo right into your home directory on tacc and use the `sbt assembly` task (or run `run jar`) to prepare the jar with all the dependencies needed to run on hadoop. `run cluster <mainclass> <args>` (which just runs `hadoop jar (jar)`) will run whatever mainclass on the cluster. tacc-hadoop takes care of setting up the shell environment.

also, note that you have to put data onto the hadoop filesystem - you can use the put command to push files and directories from your scratch directory (which is where you should be storing the datasets) up onto hdfs. also see the `hadoop fs` command.

### local execution
scoobi makes it very easy to run stuff on your local machine for testing - basically, if you use the sbt target `run-main` on a ScoobiApp in an environment without a cluster configuration available, scoobi will runn the app in local mode. useful for debugging.

## parser
in order to get logical forms, this code uses the [Clark and Curran tools][candc] to parse sentences and extract first-order logic. you will need to use the version in the svn repo, and you will also need to follow the instructions for setting up the soap client and server and for installing Boxer. 

once you have those compiled and everything, you need to set up the environment for running:
* export the environment variable $CANDC\_HOME to point to your C&C parser installation
* start the soap server with the correct arguments - `$CANDC\_HOME/bin/soap\_server --models $CANDC\_HOME/models --server localhost:9000 --candc-printer boxer` will do the job
* on the cluster, every machine will need to have the soap server running - i've included some scripts that can check the hadoop conf and use ssh to connect to each machine to run the server, and then test that it's running properly on each.

[berant]: http://u.cs.biu.ac.il/~nlp/downloads/ReverbLocalGlobal.html
[breadme]: http://u.cs.biu.ac.il/~nlp/downloads/REVERB/reverb_readme.txt
[tacc-hadoop]: https://github.com/utcompling/tacc-hadoop/
[tacc-hadoop readme]: https://github.com/utcompling/tacc-hadoop/blob/master/README.md
[scoobi]: https://github.com/NICTA/scoobi
[scalaz]: https://github.com/scalaz/scalaz
[candc]: http://svn.ask.it.usyd.edu.au/trac/candc
