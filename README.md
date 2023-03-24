# QuickREST

QuickREST can be used to explore different behaviours of a REST API.

Currently only swagger/OpenAPI 2.0 is supported.

## Quick start

To explore a behaviour with QuickREST: `java -jar quick-rest.jar start -p 3000 -H localhost --url http://localhost:3000/swagger.json -b state-mutation -t 100`

To use existing examples as tests: `java -jar quick-rest.jar test -p 3000 -H localhost --file ./out/state-mutation.edn`

## Build

Building QuickREST requires a [Clojure](https://clojure.org/) installation.

To build a self-contained jar-file, use `make uberjar`. This command will produce a `quick-rest.jar` in the target folder.

## Running

QuickREST can be used to explore different behaviours of a REST API.

To run QuickREST use the following invocation:
`java -jar quick-rest.jar <action> -p <port> -H <host> --url <url> -b <property> -t <tests>`

QuickREST support two different modes, one is to perform exploration of behaviours the other one is to run available examples as tests.

These different modes are the first parameter when starting QuickREST.

Mode | Description
-----|------------
`start` | Start exploration of behaviour
`test` | Execute existing examples as tests

An example of exploring with QuickREST: `java -jar quick-rest.jar start -p 3000 -H localhost --url http://localhost:3000/swagger.json -b state-mutation -t 100`

An example of testing with QuickREST: `java -jar quick-rest.jar test -p 3000 -H localhost --file ./out/state-mutation.edn`

The following options are available:

Option | Default | Description
-------|---------|------------
-p, --port PORT | 80 | Port number
-H, --hostname HOST | localhost | SUT host
-f, --file NAME | - | File name of an OpenAPI Specification
-u, --url NAME | - | File name of an OpenAPI Specification
-b, --behavior NAME | - | The name of the behavior to explore
-l, --min-seq-size INT | 1 | The minimum number of operations per behavior property
-s, --max-seq-size INT | 1 | The maximum number of operations per behavior property
-t, --tests INT | 100 | The maximum number of tests per behavior property
-h, --help | - | Show help

The `-b` option specify the behaviour to explore. The following behaviours are available:

Option name|Description
-----------|-----------
response-equality | Find examples of sequences where an operation return the same response when invoked twice
response-inequality | Find examples of sequences where an operation return different responses when invoked twice
state-mutation | Find sequences of operations where the state of a GET operation has changed
state-identity | Find sequences of operations where the state of a GET operation has changed, but is then undone, bringing the state back to the initial state
fuzz | Explore the SUT to find crashes (status code 500)
