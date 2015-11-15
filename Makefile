SRC=$(shell find src -name "*erl" -or -name "*src")
COMPILE=.compile-flag
BUILD=.build-flag
DEPS=.deps-flag

docker=sudo docker

all: $(COMPILE) $(BUILD)

$(DEPS): rebar.config
	rebar get-deps
	touch $@

$(COMPILE): $(SRC) $(DEPS)
	rebar compile generate
	touch $@

$(BUILD): Dockerfile $(COMPILE)
	$(docker) build --tag="syrup" .
	touch $@

.PHONY: test
test: $(BUILD)
	./run_tests.sh
