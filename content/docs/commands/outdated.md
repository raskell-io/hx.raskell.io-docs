+++
title = "hx outdated"
weight = 22
+++

Check for outdated dependencies.

## Synopsis

```bash
hx outdated [OPTIONS]
```

## Description

The `outdated` command checks your dependencies against the latest versions available on Hackage and reports which packages have updates available.

## Options

```
    --all               Show all dependencies (including up-to-date)
    --major             Show only major version updates
    --exit-code         Exit with code 1 if outdated packages exist
    --json              Output as JSON
-v, --verbose           Show detailed output
```

## Examples

### Check All Dependencies

```bash
hx outdated
```

Output:
```
Package          Current    Latest     Constraint
aeson            2.1.0.0    2.2.1.0    ^>=2.1
text             2.0.1      2.1        >=2.0
containers       0.6.7      0.6.8      (any)

3 packages have updates available
```

### Show All (Including Up-to-date)

```bash
hx outdated --all
```

### Major Updates Only

```bash
hx outdated --major
```

Output:
```
Package          Current    Latest     Breaking
aeson            2.1.0.0    3.0.0.0    Yes
lens             5.1        6.0        Yes

2 major updates available
```

### JSON Output

```bash
hx outdated --json
```

```json
{
  "outdated": [
    {
      "name": "aeson",
      "current": "2.1.0.0",
      "latest": "2.2.1.0",
      "constraint": "^>=2.1"
    }
  ]
}
```

### CI Check

```bash
# Fail if any packages are outdated
hx outdated --exit-code
```

## Version Categories

### Patch Update

Bug fixes, no API changes:
```
2.1.0.0 -> 2.1.0.1
```

### Minor Update

New features, backwards compatible:
```
2.1.0 -> 2.2.0
```

### Major Update

Breaking changes:
```
2.1.0 -> 3.0.0
```

## Update Workflow

1. Check what's outdated:
   ```bash
   hx outdated
   ```

2. Update specific packages:
   ```bash
   hx update aeson text
   ```

3. Or update all:
   ```bash
   hx update
   ```

4. Run tests:
   ```bash
   hx test
   ```

5. Commit:
   ```bash
   git add hx.lock *.cabal
   git commit -m "Update dependencies"
   ```

## Constraint Compatibility

The output shows if updates are compatible with your constraints:

```
Package     Current   Latest    Constraint    Compatible
aeson       2.1.0.0   2.1.2.0   ^>=2.1        ✓
text        2.0.1     3.0       ^>=2.0        ✗ (major)
```

## CI Integration

Report outdated dependencies without failing:

```yaml
- name: Check for updates
  run: hx outdated
  continue-on-error: true
```

Fail on outdated:

```yaml
- name: Ensure up-to-date
  run: hx outdated --exit-code
```

## See Also

- [hx update](/docs/commands/update) — Update dependencies
- [hx lock](/docs/commands/lock) — Generate lockfile
