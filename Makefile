ANSI_COLOR_GREEN = \x1b[32m
ANSI_COLOR_RESET = \x1b[0m

# project structure
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = $(BUILD_DIR)/bin
TEST_DIR = $(BUILD_DIR)/test
EXAMPLES_DIR = examples

# compiler and flags
CC = clang

# build flags
CFLAGS = -Wall -Wextra -I$(SRC_DIR) -std=c11

# library name
LIB_NAME = libhoney.a

# find all .c files recursively in src/
SOURCES = $(shell find $(SRC_DIR) -name '*.c')

# generate object file paths
OBJECTS = $(SOURCES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

# targets
.PHONY: all clean

all: honey $(BUILD_DIR)/$(LIB_NAME)

honey: $(BUILD_DIR) $(BIN_DIR) $(OBJECTS)
	$(CC) -o $(BIN_DIR)/$@ $(OBJECTS)
	@echo "$(ANSI_COLOR_GREEN)Honey built$(ANSI_COLOR_RESET): $(BIN_DIR)/honey\n"

# create build directory structure mirroring src/
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# create bin directory
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# create test directory
$(TEST_DIR):
	mkdir -p $(TEST_DIR)

# compile c sources (maintain directory structure)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c $(MODE_FILE)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# create static library (to use in tests)
$(BUILD_DIR)/$(LIB_NAME): $(OBJECTS)
	ar rcs $@ $(OBJECTS)
	@echo "$(ANSI_COLOR_GREEN)Library built$(ANSI_COLOR_RESET): $(BUILD_DIR)/$(LIB_NAME)\n"

# build tests
test: $(BUILD_DIR)/$(LIB_NAME) $(TEST_DIR)
	$(CC) $(CFLAGS) test/honey-test.c -L$(BUILD_DIR) -lhoney $(LDFLAGS) -o $(TEST_DIR)/honey-test
	@echo "$(ANSI_COLOR_GREEN)Test built$(ANSI_COLOR_RESET): $(TEST_DIR)/honey-test\n"

clean:
	rm -rf $(BUILD_DIR) output.s output.o honey_prog

# generate compile_commands.json for lsp
compdb:
	bear -- make clean all test
	@echo "compile_commands.json generated\n"
