+++
title = "Development"
weight = 60
template = "section.html"
sort_by = "weight"
+++

Resources for contributing to hx and understanding its internals.

## Overview

hx is an open-source project written in Rust. This section covers:

- **Performance Benchmarks** — How we measure and track performance
- **Testing** — Test suite structure and how to run tests
- **Contributing** — Guidelines for contributing to hx

## Quality Standards

hx maintains high quality through:

### Comprehensive Testing

- 430+ unit tests across all crates
- 15+ end-to-end integration tests
- Snapshot tests for output stability
- Cross-platform CI (Linux, macOS, Windows)

### Performance Tracking

- Criterion benchmarks for critical paths
- Comparison benchmarks against cabal and stack
- Historical tracking of performance regressions

### Code Quality

- `cargo fmt` for consistent formatting
- `cargo clippy` with `-D warnings` for linting
- Documentation for all public APIs

## Sections

- **[Benchmarks](/docs/development/benchmarks)** — Performance measurements and methodology
- **[Testing](/docs/development/testing)** — Test suite overview and how to contribute tests
