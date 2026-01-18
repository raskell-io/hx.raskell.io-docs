+++
title = "Cross-Compilation"
weight = 2
+++

Build Haskell programs for different platforms than your development machine.

## Overview

hx supports cross-compilation, allowing you to build binaries for:
- Different operating systems (Linux, macOS, Windows)
- Different architectures (x86_64, ARM64, RISC-V)
- Special platforms (WebAssembly)

## Supported Targets

### GHC Targets

| Target | Description |
|--------|-------------|
| `x86_64-linux-gnu` | Linux x86_64 (glibc) |
| `x86_64-linux-musl` | Linux x86_64 (musl, static) |
| `aarch64-linux-gnu` | Linux ARM64 |
| `x86_64-apple-darwin` | macOS Intel |
| `aarch64-apple-darwin` | macOS Apple Silicon |
| `x86_64-pc-windows-msvc` | Windows x86_64 |

### BHC Additional Targets

| Target | Description |
|--------|-------------|
| `wasm32-wasi` | WebAssembly (WASI) |
| `riscv64-linux-gnu` | RISC-V 64-bit |

## Basic Cross-Compilation

### Command Line

```bash
# Build for Linux ARM64
hx build --target aarch64-linux-gnu

# Build for macOS Intel from ARM Mac
hx build --target x86_64-apple-darwin

# Build for WebAssembly (requires BHC)
hx build --backend bhc --target wasm32-wasi
```

### Configuration

Set a default target in `hx.toml`:

```toml
[build]
target = "aarch64-linux-gnu"
```

Or per-compiler:

```toml
[compiler.bhc]
target = "wasm32-wasi"
```

## Cross-Compilation Setup

### Prerequisites

Cross-compilation requires:
1. A cross-compiler for the target
2. Target libraries (libc, etc.)
3. Cross-compilation toolchain

### Installing Cross-Compilers

#### GHC Cross-Compilers

```bash
# Install cross-GHC via ghcup (if available)
ghcup install ghc --cross aarch64-linux-gnu 9.8.2

# Or use hx
hx toolchain install --ghc 9.8.2 --target aarch64-linux-gnu
```

#### BHC Cross-Compilers

BHC includes multi-target support:

```bash
hx toolchain install --bhc 0.2.0
# BHC can target multiple platforms from single install
```

### Target Libraries

For Linux targets, install appropriate libraries:

```bash
# Ubuntu/Debian
sudo apt install gcc-aarch64-linux-gnu libc6-dev-arm64-cross

# Fedora
sudo dnf install gcc-aarch64-linux-gnu glibc-devel.aarch64
```

## Platform-Specific Builds

### Linux x86_64 to ARM64

```bash
# Install cross-compilation tools
sudo apt install gcc-aarch64-linux-gnu

# Build
hx build --target aarch64-linux-gnu --release
```

### macOS Intel to Apple Silicon

```bash
# Xcode includes both toolchains
hx build --target aarch64-apple-darwin --release
```

### macOS to Linux

Requires Linux cross-compilation toolchain:

```bash
# Using Nix or Docker is recommended
hx build --target x86_64-linux-gnu --release
```

### WebAssembly

Requires BHC backend:

```bash
hx build --backend bhc --target wasm32-wasi --release
```

## Static Linking

For fully static binaries (useful for containers):

```toml
[build]
target = "x86_64-linux-musl"
ghc-options = ["-static", "-optl-static"]
```

```bash
hx build --target x86_64-linux-musl --release
```

The resulting binary has no runtime dependencies.

## Docker-Based Cross-Compilation

For complex cross-compilation, use Docker:

```dockerfile
# Dockerfile.cross
FROM ghcr.io/haskell/ghc-cross:9.8.2-aarch64-linux

WORKDIR /app
COPY . .

RUN hx build --release
```

```bash
docker build -f Dockerfile.cross -t myapp-arm64 .
docker run --rm myapp-arm64 cat /app/dist-newstyle/.../myapp > myapp-arm64
```

## CI Cross-Compilation

### GitHub Actions

```yaml
name: Cross-compile

jobs:
  build:
    strategy:
      matrix:
        target:
          - x86_64-linux-gnu
          - aarch64-linux-gnu
          - x86_64-apple-darwin
          - aarch64-apple-darwin

    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Setup hx
        run: curl -fsSL https://get.raskell.io/hx | sh

      - name: Install cross-compiler
        run: hx toolchain install --ghc 9.8.2 --target ${{ matrix.target }}

      - name: Build
        run: hx build --target ${{ matrix.target }} --release

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: myapp-${{ matrix.target }}
          path: dist-newstyle/**/myapp
```

## Troubleshooting

### Missing Cross-Compiler

```
error: Cross-compiler for aarch64-linux-gnu not found

fix: Install cross-GHC:
     hx toolchain install --ghc 9.8.2 --target aarch64-linux-gnu
```

### Missing Target Libraries

```
error: Cannot find libc for target aarch64-linux-gnu

fix: Install cross-compilation libraries:
     sudo apt install libc6-dev-arm64-cross
```

### FFI Issues

Foreign function calls may need platform-specific handling:

```haskell
#if defined(aarch64_HOST_ARCH)
foreign import ccall "arm_specific_func" armFunc :: IO ()
#else
foreign import ccall "x86_specific_func" x86Func :: IO ()
#endif
```

## Best Practices

### 1. Test on Target Platform

Cross-compiled binaries should be tested on actual target hardware or emulation:

```bash
# Test ARM64 binary with QEMU
qemu-aarch64-static ./myapp
```

### 2. Use Release Mode

```bash
hx build --target aarch64-linux-gnu --release
```

### 3. Check Binary Type

```bash
file dist-newstyle/.../myapp
# myapp: ELF 64-bit LSB executable, ARM aarch64, ...
```

### 4. Consider Static Linking

For deployment simplicity:

```toml
[build]
ghc-options = ["-static"]
```

## See Also

- [hx build](/docs/commands/build) — Build command
- [Compiler Backends](/docs/features/compiler-backends) — GHC and BHC
- [Toolchain Management](/docs/features/toolchain) — Installing compilers
