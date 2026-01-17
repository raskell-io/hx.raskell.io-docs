+++
title = "Quick Start"
weight = 2
+++

## Create a New Project

```bash
hx new my-project
cd my-project
```

This creates a new Haskell project with the following structure:

```
my-project/
├── app/
│   └── Main.hs
├── src/
│   └── Lib.hs
├── test/
│   └── Spec.hs
├── my-project.cabal
└── README.md
```

## Build Your Project

```bash
hx build
```

## Run Your Project

```bash
hx run
```

## Add Dependencies

```bash
hx add text containers
```

## Run Tests

```bash
hx test
```

## REPL

Start a GHCi session with your project loaded:

```bash
hx repl
```
