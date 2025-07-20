DOCKER_IMAGE = erlang-dev
DOCKER_RUN = docker run --rm -v $${PWD}:/app -w /app $(DOCKER_IMAGE)
SRC_DIRS = src/core src/evolution src/substrate src/utils
EBIN_DIR = build/ebin
ERLC_FLAGS = -o $(EBIN_DIR) -I src

.PHONY: all clean build-image shell

all: build-image $(EBIN_DIR)
	@for dir in $(SRC_DIRS); do \
		echo "Compiling $$dir..."; \
		$(DOCKER_RUN) erlc $(ERLC_FLAGS) $$dir/*.erl; \
	done

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

build-image:
	@if [ -z "$$(docker images -q $(DOCKER_IMAGE))" ]; then \
		echo "Building Docker image..."; \
		docker build -t $(DOCKER_IMAGE) docker/; \
	fi

shell: build-image
	docker run -it --rm -v ${PWD}:/app -w /app $(DOCKER_IMAGE) erl -pa $(EBIN_DIR)

clean:
	rm -rf $(EBIN_DIR)