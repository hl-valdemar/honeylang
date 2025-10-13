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

# find all .c files recursively in src/
SOURCES = $(shell find $(SRC_DIR) -name '*.c')

# generate object file paths
OBJECTS = $(SOURCES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

# targets
.PHONY: all clean

all: honey

honey: $(BUILD_DIR) $(OBJECTS)
	$(CC) -o $(BUILD_DIR)/$@ $(OBJECTS)

# create build directory structure mirroring src/
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# compile c sources (maintain directory structure)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c $(MODE_FILE)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# build examples - using external flags (only public api visible)
# examples: $(BUILD_DIR)/$(LIB_NAME)
# 	$(CC) $(EXTERNAL_CFLAGS) $(EXAMPLES_DIR)/basic-window.c -L$(BUILD_DIR) -lpine $(LDFLAGS) -o $(BUILD_DIR)/basic-window
# 	@echo "$(ANSI_COLOR_GREEN)Example built$(ANSI_COLOR_RESET): $(BUILD_DIR)/basic-window\n"

run:
	./$(BUILD_DIR)/honey

clean:
	rm -rf $(BUILD_DIR)
