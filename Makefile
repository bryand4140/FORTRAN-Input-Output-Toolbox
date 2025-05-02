# Compiler and flags
FC = gfortran

# FFLAGS for dubuging and code development
FFLAGS = -Wall -g -O1 -fcheck=all -march=native -std=f2008 -J$(BUILD_DIR)

## FFLAGS for regular use
#FFLAGS = -w -O3 -march=native -fcheck=all -std=f2008 -J$(BUILD_DIR)

# Directories
SRC_DIR = source
MODULES_DIR = modules
BUILD_DIR = build

# Platform detection for shell commands
ifeq ($(OS),Windows_NT)
	SHELL := pwsh.exe
	MKDIR_P = mkdir -Force
	RM = Remove-Item -Recurse -Force
else
	SHELL := /bin/bash
	MKDIR_P = mkdir -p
	RM = rm -rf
endif

# Source module files
MODULES = $(MODULES_DIR)/MOD_Select_Precision.f90 \
		  $(MODULES_DIR)/IO_Toolbox.f90

# Object files for modules
MODULE_OBJS = $(patsubst $(MODULES_DIR)/%.f90,$(BUILD_DIR)/%.o,$(MODULES))

# Executable names (no extension)
EXES = main 

# Source files for executables
MAIN_SRC    = $(SRC_DIR)/main.f90

# Object files for executables
MAIN_OBJ    = $(BUILD_DIR)/main.o

# Default target: build all executables
all: $(EXES)

# Build rules for executables
main: $(MODULE_OBJS) $(MAIN_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

# Generic rule to compile module files
$(BUILD_DIR)/%.o: $(MODULES_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Generic rule to compile source files (main, test, etc.)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Ensure build directory exists
$(BUILD_DIR):
	$(MKDIR_P) $(BUILD_DIR)

# Clean target to remove build artifacts and executables
clean:
	$(RM) $(BUILD_DIR)
	-$(RM) main main2 test

.PHONY: all clean
