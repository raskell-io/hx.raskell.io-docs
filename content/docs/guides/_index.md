+++
title = "Guides"
weight = 50
template = "section.html"
sort_by = "weight"
+++

Step-by-step tutorials and workflow guides for common tasks.

## Tutorials

- **[CI/CD Setup](/docs/guides/ci-cd)** — Set up continuous integration
- **[Migrating from Stack](/docs/guides/migrating-from-stack)** — Move a Stack project to hx
- **[Migrating from Cabal](/docs/guides/migrating-from-cabal)** — Adopt hx in a Cabal project
- **[Creating a Library](/docs/guides/creating-library)** — Build and publish a library

## Workflows

- **[Development Workflow](/docs/guides/development-workflow)** — Day-to-day development
- **[Troubleshooting](/docs/guides/troubleshooting)** — Common issues and solutions

## Quick Reference

### New Project

```bash
hx new my-project
cd my-project
hx build
hx test
```

### Existing Project

```bash
cd existing-project
hx init
hx lock
hx build
```

### Daily Development

```bash
hx watch test          # TDD workflow
hx build --release     # Production build
hx doctor              # Check environment
```
