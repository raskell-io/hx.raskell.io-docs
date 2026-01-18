+++
title = "Concepts"
weight = 40
template = "section.html"
sort_by = "weight"
+++

Understanding how hx works and the concepts behind it.

## Overview

hx is built on several key concepts:

- **Project Structure** — How hx projects are organized
- **Dependency Resolution** — How dependencies are managed
- **Build System** — How builds work under the hood
- **Configuration Layering** — How settings are merged

## Philosophy

hx follows these design principles:

### Wrap Then Replace

hx wraps existing Haskell tools (GHC, Cabal, GHCup) rather than replacing them outright. This means:
- Compatibility with existing tooling
- Familiar underlying behavior
- Ability to eject if needed

### Sensible Defaults

hx provides opinionated defaults that work for most projects:
- Recommended GHC versions
- Standard project layouts
- Default formatter and linter

### Fast Feedback

Common operations are optimized for speed:
- Incremental builds
- Type checking without codegen
- Watch mode for rapid iteration

### Reproducible Builds

hx emphasizes reproducibility:
- Lockfiles for dependency pinning
- Toolchain version pinning
- Checksums for verification

## Sections

- **[Project Structure](/docs/concepts/project-structure)** — How hx projects are organized
- **[Dependency Resolution](/docs/concepts/dependency-resolution)** — How dependencies work
- **[Build System](/docs/concepts/build-system)** — How builds are executed
- **[Architecture](/docs/concepts/architecture)** — How hx is built internally
