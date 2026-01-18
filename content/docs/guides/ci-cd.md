+++
title = "CI/CD Setup"
weight = 1
+++

Set up continuous integration and deployment for your hx project.

## GitHub Actions

### Basic Workflow

Create `.github/workflows/ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install hx
        run: curl -fsSL https://get.raskell.io/hx | sh

      - name: Cache dependencies
        uses: actions/cache@v4
        with:
          path: |
            ~/.cabal
            ~/.ghcup
            dist-newstyle
          key: ${{ runner.os }}-${{ hashFiles('hx.lock') }}

      - name: Setup toolchain
        run: hx toolchain install

      - name: Verify lockfile
        run: hx lock --frozen

      - name: Build
        run: hx build

      - name: Test
        run: hx test

      - name: Lint
        run: hx lint

      - name: Format check
        run: hx fmt --check
```

### Multi-Platform

```yaml
name: CI

on: [push, pull_request]

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - name: Install hx (Unix)
        if: runner.os != 'Windows'
        run: curl -fsSL https://get.raskell.io/hx | sh

      - name: Install hx (Windows)
        if: runner.os == 'Windows'
        shell: pwsh
        run: |
          Invoke-WebRequest -Uri https://get.raskell.io/hx.ps1 -OutFile hx-install.ps1
          ./hx-install.ps1

      - name: Cache
        uses: actions/cache@v4
        with:
          path: |
            ~/.cabal
            ~/.ghcup
            dist-newstyle
          key: ${{ runner.os }}-${{ hashFiles('hx.lock') }}

      - name: Build and test
        run: |
          hx toolchain install
          hx build
          hx test
```

### Multiple GHC Versions

```yaml
jobs:
  build:
    strategy:
      matrix:
        ghc: ['9.6.4', '9.8.2', '9.10.1']

    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install hx
        run: curl -fsSL https://get.raskell.io/hx | sh

      - name: Install GHC ${{ matrix.ghc }}
        run: hx toolchain install --ghc ${{ matrix.ghc }}

      - name: Build
        run: hx build

      - name: Test
        run: hx test
```

### Release Workflow

```yaml
name: Release

on:
  push:
    tags: ['v*']

jobs:
  build:
    strategy:
      matrix:
        include:
          - os: ubuntu-latest
            target: x86_64-unknown-linux-gnu
          - os: macos-latest
            target: x86_64-apple-darwin
          - os: macos-latest
            target: aarch64-apple-darwin

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - name: Install hx
        run: curl -fsSL https://get.raskell.io/hx | sh

      - name: Build release
        run: |
          hx toolchain install
          hx build --release

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: my-app-${{ matrix.target }}
          path: dist-newstyle/**/my-app

  release:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - name: Download artifacts
        uses: actions/download-artifact@v4

      - name: Create release
        uses: softprops/action-gh-release@v1
        with:
          files: my-app-*/**/my-app
```

## GitLab CI

### .gitlab-ci.yml

```yaml
stages:
  - build
  - test
  - deploy

variables:
  GHC_VERSION: "9.8.2"

cache:
  key: ${CI_COMMIT_REF_SLUG}
  paths:
    - ~/.cabal/
    - ~/.ghcup/
    - dist-newstyle/

before_script:
  - curl -fsSL https://get.raskell.io/hx | sh
  - hx toolchain install --ghc $GHC_VERSION

build:
  stage: build
  script:
    - hx build

test:
  stage: test
  script:
    - hx test

lint:
  stage: test
  script:
    - hx lint
    - hx fmt --check

deploy:
  stage: deploy
  only:
    - tags
  script:
    - hx build --release
    - # deploy commands
```

## CircleCI

### .circleci/config.yml

```yaml
version: 2.1

executors:
  haskell:
    docker:
      - image: cimg/base:stable
    working_directory: ~/project

jobs:
  build:
    executor: haskell
    steps:
      - checkout
      - restore_cache:
          keys:
            - deps-{{ checksum "hx.lock" }}
      - run:
          name: Install hx
          command: curl -fsSL https://get.raskell.io/hx | sh
      - run:
          name: Setup toolchain
          command: hx toolchain install
      - run:
          name: Build
          command: hx build
      - run:
          name: Test
          command: hx test
      - save_cache:
          key: deps-{{ checksum "hx.lock" }}
          paths:
            - ~/.cabal
            - ~/.ghcup
            - dist-newstyle

workflows:
  version: 2
  build-test:
    jobs:
      - build
```

## Best Practices

### 1. Cache Effectively

Cache these directories:
- `~/.cabal/` — Package cache
- `~/.ghcup/` — Toolchain installations
- `dist-newstyle/` — Build artifacts

Use lockfile hash as cache key.

### 2. Verify Lockfile

```yaml
- name: Verify lockfile
  run: hx lock --frozen
```

Fails if lockfile is out of date.

### 3. Fail Fast

```yaml
- name: Test
  run: hx test --fail-fast
```

Stop on first failure for faster feedback.

### 4. Parallel Jobs

```yaml
jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - run: hx lint && hx fmt --check

  test:
    runs-on: ubuntu-latest
    steps:
      - run: hx test

  build:
    runs-on: ubuntu-latest
    steps:
      - run: hx build --release
```

### 5. Use Environment Variables

```yaml
env:
  HX_LOG: info
  HX_PROGRESS: 0
  CI: true
```

## Docker

### Dockerfile

```dockerfile
FROM haskell:9.8.2

# Install hx
RUN curl -fsSL https://get.raskell.io/hx | sh

WORKDIR /app

# Copy dependency files first (for caching)
COPY hx.toml hx.lock *.cabal ./

# Install dependencies
RUN hx sync

# Copy source
COPY . .

# Build
RUN hx build --release

# Create minimal image
FROM debian:bookworm-slim
COPY --from=0 /app/dist-newstyle/.../my-app /usr/local/bin/
CMD ["my-app"]
```

### Docker Compose for CI

```yaml
# docker-compose.ci.yml
version: '3'
services:
  test:
    build: .
    command: hx test
```

## See Also

- [hx lock --frozen](/docs/commands/lock) — Frozen builds
- [hx test --fail-fast](/docs/commands/test) — Test options
