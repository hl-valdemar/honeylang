ANSI_COLOR_GREEN = \x1b[32m
ANSI_COLOR_RESET = \x1b[0m

# project structure
SRC_DIR = src
BUILD_DIR = build
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

honey: $(BUILD_DIR) $(OBJECTS)
	$(CC) -o $(BUILD_DIR)/$@ $(OBJECTS)
	@echo "$(ANSI_COLOR_GREEN)Honey built$(ANSI_COLOR_RESET): $(BUILD_DIR)/honey\n"

# create build directory structure mirroring src/
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# compile c sources (maintain directory structure)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c $(MODE_FILE)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# create static library (to use in tests)
$(BUILD_DIR)/$(LIB_NAME): $(OBJECTS)
	ar rcs $@ $(OBJECTS)
	@echo "$(ANSI_COLOR_GREEN)Library built$(ANSI_COLOR_RESET): $(BUILD_DIR)/$(LIB_NAME)\n"

# build examples - using external flags (only public api visible)
# examples: $(BUILD_DIR)/$(LIB_NAME)
# 	$(CC) $(EXTERNAL_CFLAGS) $(EXAMPLES_DIR)/basic-window.c -L$(BUILD_DIR) -lhoney $(LDFLAGS) -o $(BUILD_DIR)/basic-window
# 	@echo "$(ANSI_COLOR_GREEN)Example built$(ANSI_COLOR_RESET): $(BUILD_DIR)/basic-window\n"

# build tests
tests: $(BUILD_DIR)/$(LIB_NAME)
	$(CC) $(CFLAGS) tests/parser.c -L$(BUILD_DIR) -lhoney $(LDFLAGS) -o $(BUILD_DIR)/parser
	@echo "$(ANSI_COLOR_GREEN)Test built$(ANSI_COLOR_RESET): $(BUILD_DIR)/parser\n"

	$(CC) $(CFLAGS) tests/codegen.c -L$(BUILD_DIR) -lhoney $(LDFLAGS) -o $(BUILD_DIR)/codegen
	@echo "$(ANSI_COLOR_GREEN)Test built$(ANSI_COLOR_RESET): $(BUILD_DIR)/codegen\n"

clean:
	rm -rf $(BUILD_DIR) output.s output.o honey_prog

# generate compile_commands.json for lsp
compdb:
	bear -- make clean all tests
	@echo "compile_commands.json generated\n"
