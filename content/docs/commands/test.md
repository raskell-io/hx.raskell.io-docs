+++
title = "hx test"
weight = 3
+++

Run the test suite.

## Synopsis

```bash
hx test [OPTIONS] [-- <ARGS>...]
```

## Description

The `test` command builds and runs your project's test suite. It supports filtering tests, parallel execution, and various test frameworks.

## Options

### Test Options

```
    --test <NAME>       Run a specific test suite
    --match <PATTERN>   Run tests matching pattern
    --no-run            Build tests without running
    --coverage          Generate test coverage report
    --fail-fast         Stop on first test failure
```

### Build Options

```
    --release           Build tests with optimizations
-j, --jobs <N>          Parallel compilation jobs
    --backend <BACKEND> Compiler backend [ghc, bhc]
```

### Output Options

```
-v, --verbose           Show all test output
    --test-options      Additional options to pass to test runner
```

## Examples

### Run All Tests

```bash
# Build and run all test suites
hx test
```

### Run Specific Test Suite

```bash
# Run only the "unit-tests" suite
hx test --test unit-tests

# Run only the "integration-tests" suite
hx test --test integration-tests
```

### Filter Tests by Pattern

```bash
# Run tests matching "Parser"
hx test --match "Parser"

# Run tests matching a regex
hx test --match "API.*GET"
```

### Pass Arguments to Test Framework

Different test frameworks accept different arguments:

```bash
# HSpec: run only specific tests
hx test -- --match "should parse valid input"

# Tasty: increase timeout
hx test -- --timeout 30s

# QuickCheck: more iterations
hx test -- --quickcheck-tests 1000
```

### Fail Fast

```bash
# Stop on first failure
hx test --fail-fast
```

### Test Coverage

```bash
# Generate coverage report
hx test --coverage
```

Coverage reports are generated in `dist-newstyle/hpc/`.

### Verbose Output

```bash
# Show all test output
hx test --verbose
```

## Test Frameworks

hx works with all major Haskell test frameworks:

### HSpec

```haskell
-- test/Spec.hs
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parses valid input" $ do
      parse "test" `shouldBe` Right "test"
```

```bash
hx test --match "Parser"
hx test -- --match "parses valid"
```

### Tasty

```haskell
-- test/Main.hs
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Addition" $ 1 + 1 @?= 2
  ]
```

```bash
hx test -- --pattern "Addition"
hx test -- --timeout 10s
```

### QuickCheck

```haskell
import Test.QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
```

```bash
hx test -- --quickcheck-tests 500
hx test -- --quickcheck-max-size 100
```

## Configuration

Configure test behavior in `hx.toml`:

```toml
[test]
# Default test suite to run
default-suite = "unit-tests"

# Show timing information
show-timing = true

# Fail fast by default
fail-fast = false

# Coverage settings
[test.coverage]
enabled = false
output-dir = "coverage"
```

## Multiple Test Suites

Define multiple test suites in your `.cabal` file:

```cabal
test-suite unit-tests
  type: exitcode-stdio-1.0
  main-is: Unit.hs
  hs-source-dirs: test/unit

test-suite integration-tests
  type: exitcode-stdio-1.0
  main-is: Integration.hs
  hs-source-dirs: test/integration

test-suite property-tests
  type: exitcode-stdio-1.0
  main-is: Properties.hs
  hs-source-dirs: test/property
```

Run each suite individually:

```bash
hx test --test unit-tests
hx test --test integration-tests
hx test --test property-tests
```

Or run all:

```bash
hx test
```

## CI Integration

For continuous integration:

```bash
# Recommended CI test command
hx test --fail-fast --verbose
```

In GitHub Actions:

```yaml
- name: Run tests
  run: hx test --fail-fast
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All tests passed |
| 1 | Test failures |
| 5 | Build failure |

## See Also

- [hx build](/docs/commands/build) — Build without testing
- [hx watch test](/docs/commands/watch) — Auto-run tests on changes
- [hx bench](/docs/commands/bench) — Run benchmarks
