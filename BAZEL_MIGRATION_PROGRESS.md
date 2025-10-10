# Scalameta Bazel Migration Progress

## Overview

This document tracks the progress of migrating the Scalameta sbt build to Bazel. The goal is to build the `scalameta` target using Bazel with official Scala rules, rules_jvm_external, and Scala 2.13.16.

## Project Structure

Scalameta is organized into several modules with the following dependency hierarchy:

### Core Modules (scalameta/ directory)

1. **semanticdb** (semanticdb/semanticdb/)
   - Protocol buffer definitions for semantic information
   - Generated Scala code from `.proto` files
   - No dependencies on other scalameta modules
   - Dependencies: scalapb-runtime, protobuf-java

2. **common** (scalameta/common/)
   - Shared utilities and helpers
   - Dependencies: semanticdb, sourcecode

3. **io** (scalameta/io/)
   - Input/output APIs
   - Dependencies: common

4. **dialects** (scalameta/dialects/)
   - Scala dialect definitions
   - Dependencies: common

5. **inputs** (scalameta/inputs/)
   - Source code input APIs
   - Dependencies: common, io

6. **tokens** (scalameta/tokens/)
   - Token definitions and abstractions
   - Dependencies: common, dialects, inputs

7. **tokenizers** (scalameta/tokenizers/)
   - Tokenization implementation
   - Dependencies: common, dialects, inputs, tokens, fastparse

8. **trees** (scalameta/trees/)
   - Abstract syntax tree definitions
   - Dependencies: common, dialects, inputs, tokens, tokenizers

9. **transversers** (scalameta/transversers/)
   - AST traversal and transformation
   - Dependencies: common, trees

10. **parsers** (scalameta/parsers/)
    - Parsing implementation
    - Dependencies: common, dialects, inputs, tokens, tokenizers, trees

11. **quasiquotes** (scalameta/quasiquotes/)
    - Quasiquote support
    - Dependencies: common, dialects, inputs, trees, parsers

12. **scalameta** (scalameta/scalameta/)
    - Umbrella module exposing all public APIs
    - Dependencies: All of the above

### Additional Modules (Not yet migrated)

- **semanticdbScalacCore** - Scalac plugin core library
- **semanticdbScalacPlugin** - Scalac compiler plugin
- **semanticdbJavacPlugin** - Javac compiler plugin
- **metacp** - Classpath to SemanticDB converter
- **metac** - Scalac launcher
- **metai** - SemanticDB classpath indexer
- **metap** - SemanticDB decompiler
- **symtab** - Symbol table
- **cli** - Shared CLI infrastructure
- **testkit** - Testing utilities
- **tests** - Test suites

## Bazel Configuration

### Files Created

1. **MODULE.bazel**
   - Defines the Bazel module for scalameta
   - Currently configured with rules_jvm_external for Maven dependencies
   - Note: Bzlmod is disabled in favor of WORKSPACE approach

2. **WORKSPACE**
   - Sets up rules_scala (v6.11.2) from GitHub releases
   - Configures Scala 2.13.16 toolchain
   - Sets up rules_jvm_external for Maven dependency management
   - Defines all Maven dependencies needed by the project

3. **.bazelrc**
   - Bazel build configuration
   - Disables bzlmod (using WORKSPACE instead)
   - Sets Java 11 as the build JDK
   - Enables Scala compiler workers for performance
   - Configures test output and caching

### Maven Dependencies

The following dependencies are configured in WORKSPACE:

- **Scala**: scala-library, scala-compiler, scala-reflect, scalap (2.13.16)
- **ScalaPB**: scalapb-runtime, lenses, scalapb-runtime-grpc, compilerplugin (1.0.0)
- **Common**: sourcecode, fastparse, fansi, geny
- **Testing**: scalatest, scalacheck
- **Testkit**: jarchivelib, commons-io, diffutils
- **Coursier**: coursier, coursier-cache
- **Protobuf**: protobuf-java

## BUILD Files Created

BUILD.bazel files have been created for all core modules:

- `semanticdb/semanticdb/BUILD.bazel`
- `scalameta/common/BUILD.bazel`
- `scalameta/io/BUILD.bazel`
- `scalameta/dialects/BUILD.bazel`
- `scalameta/inputs/BUILD.bazel`
- `scalameta/tokens/BUILD.bazel`
- `scalameta/tokenizers/BUILD.bazel`
- `scalameta/trees/BUILD.bazel`
- `scalameta/transversers/BUILD.bazel`
- `scalameta/parsers/BUILD.bazel`
- `scalameta/quasiquotes/BUILD.bazel`
- `scalameta/scalameta/BUILD.bazel`

Each BUILD file:
- Uses `scala_library` rule from rules_scala
- Includes both `shared/src/main/scala` and `jvm/src/main/scala` source directories
- Declares dependencies on other Scala modules using `scala_deps`
- Declares Maven dependencies using `deps` with `@maven//` prefix

## Current Status and Blockers

### Completed

- ✅ Analyzed sbt build structure and dependencies
- ✅ Created WORKSPACE and .bazelrc configuration
- ✅ Set up rules_jvm_external with Maven dependencies
- ✅ Created BUILD.bazel files for all core modules
- ✅ Established module dependency graph

### In Progress

- 🔄 Protobuf code generation for semanticdb module
- 🔄 Building and testing the scalameta target

### Blockers and Issues

1. **Protobuf Code Generation**
   - The semanticdb module requires ScalaPB to generate Scala code from `.proto` files
   - Current approach: Expecting generated files in `jvm/target/scala-2.13/src_managed/`
   - Alternative: Use sbt to pre-generate protobuf files, then build with Bazel
   - Future: Integrate scalapb code generation directly in Bazel build

2. **Macro Support**
   - Many modules use Scala macros heavily
   - Need to ensure macro paradise plugin is properly configured
   - May need compiler plugins configuration in BUILD files

3. **Cross-Version Sources**
   - Some modules have Scala version-specific sources (e.g., `scala-2.12/`, `scala-2.13/`)
   - Currently only focusing on 2.13 sources
   - May need to include version-specific directories in srcs globs

## Next Steps

### Immediate (Required for basic build)

1. **Generate Protobuf Files**
   ```bash
   # Option 1: Use sbt to generate protobuf files
   sbt semanticdbJVM/compile

   # Option 2: Set up scalapb in Bazel (more complex)
   # Add scalapb rules and configure protobuf compilation
   ```

2. **Test Basic Build**
   ```bash
   # Try building individual modules
   bazel build //scalameta/common
   bazel build //scalameta/io

   # Build the full scalameta target
   bazel build //scalameta/scalameta
   ```

3. **Fix Compilation Errors**
   - Address any missing source files
   - Fix dependency issues
   - Handle macro compilation requirements

### Medium Term

1. **Integrate ScalaPB Code Generation**
   - Add proto compilation rules to semanticdb BUILD file
   - Configure scalapb_proto_library properly
   - Ensure generated files are properly dependencies

2. **Add Compiler Plugin Support**
   - Configure scala paradise macro plugin
   - Add any other required compiler plugins
   - Set appropriate scalac options

3. **Migrate Additional Modules**
   - Add BUILD files for semanticdb plugins (scalac, javac)
   - Migrate metacp, metap, metai tools
   - Add testkit and test modules

### Long Term

1. **Full Test Suite**
   - Migrate all test modules
   - Set up test targets
   - Ensure test data and resources are properly included

2. **CI Integration**
   - Update CI scripts to use Bazel
   - Set up remote caching
   - Configure build and test workflows

3. **Performance Optimization**
   - Tune worker settings
   - Optimize dependency graph
   - Configure remote execution if needed

## Build Commands

```bash
# Build the scalameta umbrella module
bazel build //scalameta/scalameta:scalameta

# Build all modules
bazel build //scalameta/...

# Clean build
bazel clean
bazel build //scalameta/scalameta:scalameta

# Query dependencies
bazel query 'deps(//scalameta/scalameta:scalameta)'

# Show build graph
bazel query 'deps(//scalameta/scalameta:scalameta)' --output graph > graph.dot
```

## Technical Notes

### Why WORKSPACE over Bzlmod?

- rules_scala is not yet available in Bazel Central Registry (BCR)
- WORKSPACE approach is more mature and stable for rules_scala
- Can migrate to bzlmod once rules_scala is available in BCR

### Scala 2.13.16 Choice

- Latest stable Scala 2.13 version
- Good compatibility with existing codebase
- Matches recent semanticdb tags in git history

### Source Directory Structure

The project uses sbt-crossproject structure:
- `shared/src/main/scala/` - Cross-platform code
- `jvm/src/main/scala/` - JVM-specific code
- `js/src/main/scala/` - Scala.js code (not migrated)
- `native/src/main/scala/` - Scala Native code (not migrated)

For this migration, we're only focusing on JVM platform.

## References

- [Bazel Scala Rules](https://github.com/bazelbuild/rules_scala)
- [rules_jvm_external](https://github.com/bazelbuild/rules_jvm_external)
- [ScalaPB](https://scalapb.github.io/)
- [Scalameta Documentation](https://scalameta.org/)

## Troubleshooting

### Common Issues

1. **"No such package" errors**
   - Ensure BUILD.bazel files exist in referenced directories
   - Check that target names match package names

2. **Dependency not found**
   - Verify Maven artifact coordinates in WORKSPACE
   - Check that artifact exists in Maven Central
   - Try fetching dependencies: `bazel sync`

3. **Compilation errors**
   - Check source file globs include all necessary files
   - Verify Scala version compatibility
   - Ensure macro dependencies are properly declared

4. **Macro compilation issues**
   - Add `enable_compiler_plugin_api = True` to scala_library
   - Check that scala-reflect is in dependencies
   - Verify paradise plugin configuration

## CRITICAL BLOCKER (October 10, 2025)

### rules_scala 7.1.2 + Bazel 7.4.1 Compatibility Issue

**Status**: BLOCKED

**Error**:
```
ERROR: Failed to load Starlark extension '@@bazel_features//:features.bzl'.
Cycle in the workspace file detected. This indicates that a repository is used prior to being defined.
The following chain of repository dependencies lead to the missing definition.
 - @@bazel_features
```

**Root Cause**:
- rules_scala 7.1.2 has a cyclic dependency issue with Bazel 7.4.1
- The `bazel_features` repository is referenced before it's properly initialized
- This appears to be a known incompatibility between rules_scala 7.x and certain Bazel 7.x versions

**Attempted Solutions** (all failed):
1. ✗ Added explicit `rules_java_dependencies()` and `rules_java_toolchains()` calls
2. ✗ Added explicit `bazel_skylib_workspace()` initialization
3. ✗ Tried adding `bazel_features_deps()` - results in circular dependency
4. ✗ Tried using Bazel 8.4.2 - even worse protobuf incompatibilities
5. ✗ Tried rules_scala 6.6.0 - missing deps.bzl file (different structure)
6. ✗ Switched to Scala 2.12.20 - same error
7. ✗ Tried overriding protobuf version - structural incompatibilities

**Current Configuration**:
- `.bazelversion`: `7.4.1`
- `WORKSPACE`: rules_scala 7.1.2
- Scala version configured: 2.12.20 (was 2.13.16)
- Maven dependencies split into `maven_2_12` and `maven_2_13`

**Possible Solutions**:

1. **Try Bazel 6.x** (Most likely to work)
   ```bash
   echo "6.5.0" > .bazelversion
   bazel clean --expunge
   bazel build //scalameta/scalameta:scalameta
   ```

2. **Wait for rules_scala 8.x**
   - Track: https://github.com/bazelbuild/rules_scala/issues/1652
   - rules_scala 8.x will support Bazel 8+
   - No ETA available

3. **Use older rules_scala version**
   - Try rules_scala 6.5.0 or earlier
   - May require different WORKSPACE setup structure

4. **Switch to rules_scala from higherkindness**
   - Alternative Scala rules implementation
   - May have better compatibility

**Recommendation**: Try Bazel 6.5.0 first, as it's the LTS version that rules_scala 7.x was likely tested against.

## Contact

For questions or issues with this migration, refer to:
- Scalameta GitHub: https://github.com/scalameta/scalameta
- Bazel Slack #scala channel
- rules_scala issues: https://github.com/bazelbuild/rules_scala/issues
