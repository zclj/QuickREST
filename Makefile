.PHONY: test

test:
	clojure -X:test:run-tests

ktest:
	bin/kaocha --reporter kaocha.report/documentation

uberjar:
	clj -T:build uberjar
