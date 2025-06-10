# Makefile for Genealogical Inference Engine
# Copyright (C) 2025 Aidan Pace

GUILE = guile
GUILD = guild
GUILE_FLAGS = -L src -L .
GUILE_WARN_FLAGS = -W unused-variable -W unused-toplevel -W shadowed-toplevel

# Source files
SOURCES = $(shell find src -name '*.scm')
COMPILED = $(SOURCES:.scm=.go)

# Directories
SRCDIR = src
TESTDIR = tests
DOCDIR = doc
EXAMPLEDIR = examples

# Default target
.PHONY: all
all: compile

# Compile all Scheme files
.PHONY: compile
compile: $(COMPILED)

%.go: %.scm
	$(GUILD) compile $(GUILE_WARN_FLAGS) -L $(SRCDIR) -o $@ $<

# Run tests
.PHONY: test
test: compile
	@echo "Running tests..."
	@for test in $(TESTDIR)/*.scm; do \
		echo "Running $$test"; \
		$(GUILE) $(GUILE_FLAGS) $$test; \
	done

# Validate against known family structures
.PHONY: validate
validate: compile
	@echo "Running validation tests..."
	$(GUILE) $(GUILE_FLAGS) tests/validation.scm

# Performance benchmarking
.PHONY: benchmark
benchmark: compile
	@echo "Running benchmarks..."
	$(GUILE) $(GUILE_FLAGS) tests/benchmark.scm

# Run example
.PHONY: example
example: compile
	@echo "Running example inference..."
	$(GUILE) $(GUILE_FLAGS) examples/basic-inference.scm

# Documentation generation
.PHONY: docs
docs:
	@echo "Generating documentation..."
	@mkdir -p $(DOCDIR)
	@echo "Documentation generation would go here"

# REPL with project modules loaded
.PHONY: repl
repl: compile
	$(GUILE) $(GUILE_FLAGS) --listen

# Clean compiled files
.PHONY: clean
clean:
	find . -name "*.go" -delete
	find . -name "*~" -delete

# Clean everything including generated docs
.PHONY: distclean
distclean: clean
	rm -rf $(DOCDIR)/generated

# Install (local development)
.PHONY: install
install: compile
	@echo "Installing to local Guile site directory..."
	@echo "This would copy modules to ~/.guile.d/ or similar"

# Check code style
.PHONY: lint
lint:
	@echo "Checking code style..."
	@for file in $(SOURCES); do \
		echo "Checking $$file"; \
		$(GUILE) $(GUILE_FLAGS) -c "(use-modules (ice-9 readline)) (primitive-load \"$$file\")" 2>&1 | grep -E "warning:|error:"; \
	done

# Help
.PHONY: help
help:
	@echo "Genealogical Inference Engine - Makefile targets:"
	@echo "  make all       - Compile all source files (default)"
	@echo "  make compile   - Compile all Scheme files"
	@echo "  make test      - Run test suite"
	@echo "  make validate  - Validate against known family structures"
	@echo "  make benchmark - Run performance benchmarks"
	@echo "  make example   - Run example inference"
	@echo "  make repl      - Start REPL with project loaded"
	@echo "  make docs      - Generate documentation"
	@echo "  make clean     - Remove compiled files"
	@echo "  make distclean - Remove all generated files"
	@echo "  make lint      - Check code style"
	@echo "  make help      - Show this help message"