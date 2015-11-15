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

.PHONY: docker-image
docker-image: Dockerfile $(COMPILE)
	$(docker) build --tag="syrup" .

.PHONY: test
test: $(COMPILE)
	./run_tests.sh
