ANSI_COLOR_GREEN = \x1b[32m
ANSI_COLOR_RESET = \x1b[0m

# project structure
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = $(BUILD_DIR)/bin
EXAMPLES_DIR = examples

# runtime files
RUNTIME_DIR = src/honey/runtime
RUNTIME_SRC = $(RUNTIME_DIR)/start-darwin-arm64.s
RUNTIME_OBJ = $(BUILD_DIR)/runtime/start-darwin-arm64.o

# test files
TEST_DIR = tests
TEST_SOURCES = $(wildcard $(TEST_DIR)/*-test.c)
TEST_BINS = $(TEST_SOURCES:$(TEST_DIR)/%.c=$(BUILD_DIR)/tests/%)

# compiler and flags
CC = clang

# build flags
CFLAGS = -Wall -Wextra -I$(SRC_DIR) -std=c2x

# find all .c files recursively in src/
SOURCES = $(shell find $(SRC_DIR) -name '*.c')

# generate object file paths
OBJECTS = $(SOURCES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

# filter out main.o for tests (since tests have their own main)
TEST_OBJECTS = $(filter-out $(BUILD_DIR)/main.o, $(OBJECTS))

# test-specific object directory (compiled with LOG_DISABLED)
TEST_OBJ_DIR = $(BUILD_DIR)/test-objs
TEST_LIB_OBJECTS = $(SOURCES:$(SRC_DIR)/%.c=$(TEST_OBJ_DIR)/%.o)
TEST_LIB_OBJECTS := $(filter-out $(TEST_OBJ_DIR)/main.o, $(TEST_LIB_OBJECTS))

# targets
.PHONY: all test clean compdb

# prevent make from deleting test object files after building
.SECONDARY: $(TEST_LIB_OBJECTS)

all: honey

honey: $(BUILD_DIR) $(BIN_DIR) $(RUNTIME_OBJ) $(OBJECTS)
	$(CC) -o $(BIN_DIR)/$@ $(OBJECTS)
	@echo "$(ANSI_COLOR_GREEN)honey built$(ANSI_COLOR_RESET): $(BIN_DIR)/honey\n"

# run all tests
test: $(TEST_BINS)
	@echo "$(ANSI_COLOR_GREEN)running all tests...$(ANSI_COLOR_RESET)"
	@for test in $(TEST_BINS); do \
		$$test || exit 1; \
	done

# create build directory structure mirroring src/
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# create bin directory
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# build runtime object file
$(RUNTIME_OBJ): $(RUNTIME_SRC)
	@mkdir -p $(dir $@)
	$(CC) -c $< -o $@

# compile c sources (maintain directory structure)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# compile test-specific object files with LOG_DISABLED
$(TEST_OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -DLOG_DISABLED -c $< -o $@

# build individual test executables (link with test-specific objects)
$(BUILD_DIR)/tests/%: $(TEST_DIR)/%.c $(TEST_LIB_OBJECTS)
	@mkdir -p $(BUILD_DIR)/tests
	$(CC) $(CFLAGS) -DLOG_DISABLED=1 $< $(TEST_LIB_OBJECTS) -o $@
	@echo "$(ANSI_COLOR_GREEN)test built$(ANSI_COLOR_RESET): $@\n"

clean:
	rm -rf $(BUILD_DIR) output.s output.o program test-program

# generate compile_commands.json for lsp
compdb:
	bear -- make clean all test
	@echo "compile_commands.json generated\n"
