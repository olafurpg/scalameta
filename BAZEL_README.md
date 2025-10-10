# Scalameta Bazel Build

This directory contains the initial Bazel build configuration for the Scalameta project.

## What Has Been Done

### ✅ Completed

1. **Build Configuration**
   - Created `WORKSPACE` file with rules_scala v6.11.2 and rules_jvm_external v6.5
   - Created `MODULE.bazel` for future bzlmod support
   - Created `.bazelrc` with optimized build settings for Scala

2. **Dependency Management**
   - Configured all Maven dependencies via rules_jvm_external
   - Set up Scala 2.13.16 toolchain
   - Included dependencies for: ScalaPB, testing, utilities

3. **Module BUILD Files**
   - Created BUILD.bazel files for 12 core modules:
     - semanticdb (with protobuf support)
     - common, io, dialects, inputs
     - tokens, tokenizers
     - trees, transversers
     - parsers, quasiquotes
     - scalameta (umbrella module)

4. **Documentation**
   - `BAZEL_MIGRATION_PROGRESS.md` - Detailed migration status and technical notes
   - `BAZEL_QUICKSTART.md` - Quick reference guide for building
   - `BAZEL_README.md` - This file

## Files Created

```
scalameta/
├── WORKSPACE                          # Bazel workspace configuration
├── MODULE.bazel                       # Bzlmod module definition (for future)
├── .bazelrc                          # Bazel build settings
├── BAZEL_README.md                   # This file
├── BAZEL_QUICKSTART.md              # Quick start guide
├── BAZEL_MIGRATION_PROGRESS.md      # Detailed progress tracking
├── semanticdb/
│   └── semanticdb/
│       └── BUILD.bazel              # Protobuf-based module
└── scalameta/
    ├── common/BUILD.bazel
    ├── io/BUILD.bazel
    ├── dialects/BUILD.bazel
    ├── inputs/BUILD.bazel
    ├── tokens/BUILD.bazel
    ├── tokenizers/BUILD.bazel
    ├── trees/BUILD.bazel
    ├── transversers/BUILD.bazel
    ├── parsers/BUILD.bazel
    ├── quasiquotes/BUILD.bazel
    └── scalameta/BUILD.bazel        # Main umbrella target
```

## Next Steps for Building

### 1. Generate Protobuf Files (Required)

The semanticdb module needs ScalaPB-generated files. Generate them with sbt first:

```bash
sbt semanticdbJVM/compile
```

This creates files in `semanticdb/semanticdb/jvm/target/scala-2.13/src_managed/`

### 2. Try Building

```bash
# Fetch dependencies
bazel sync

# Build the main target
bazel build //scalameta/scalameta:scalameta
```

### 3. Debug Issues

If the build fails, check:
- Are protobuf files generated?
- Are source paths correct? (check with `ls scalameta/common/shared/src/main/scala/`)
- Are dependencies resolving? (try `bazel sync`)

See `BAZEL_QUICKSTART.md` for more troubleshooting tips.

## Architecture

### Module Dependency Graph

```
semanticdb (protobuf)
    ↓
common ← sourcecode
    ↓
┌───┴────┬─────────┐
io    dialects    |
└──┬────┴─────────┘
   ↓
inputs
   ↓
tokens
   ↓
tokenizers ← fastparse
   ↓
trees
   ↓
┌──┴────┬────────────┐
parsers  transversers |
   ↓    └────────────┘
quasiquotes
   ↓
scalameta (umbrella)
```

### Design Decisions

1. **WORKSPACE over Bzlmod**: rules_scala not yet in Bazel Central Registry
2. **Scala 2.13.16**: Latest stable 2.13, matches recent project tags
3. **JVM only**: Focusing on JVM platform, not JS/Native
4. **Protobuf pre-generation**: Using sbt for now, can integrate later
5. **Shared + JVM sources**: Including both cross-platform and JVM-specific code

## Known Limitations

1. **Protobuf Generation**: Not yet integrated in Bazel (requires sbt pre-build)
2. **Macro Support**: May need additional compiler plugin configuration
3. **Version-Specific Sources**: Only using 2.13 sources, not version-specific directories
4. **Scalac Plugins**: semanticdbScalac and other plugins not yet migrated
5. **Tests**: Test modules not yet migrated
6. **CLI Tools**: metap, metacp, metai not yet migrated

## Future Improvements

### Short Term
- Integrate ScalaPB code generation in Bazel
- Add compiler plugin support for macros
- Test the build end-to-end

### Medium Term
- Migrate scalac and javac plugins
- Migrate CLI tools (metap, metacp, etc.)
- Add test modules and test data

### Long Term
- Full CI integration
- Remote caching setup
- Build performance optimization
- Bazel migration for semanticdb tools

## Configuration Reference

### Key Files

- **WORKSPACE**: Main configuration
  - rules_scala setup
  - Maven dependency management
  - Scala version configuration

- **.bazelrc**: Build settings
  - Java 11 compilation
  - Worker strategy for performance
  - Disk cache configuration

- **BUILD.bazel**: Per-module build definitions
  - Source file globs
  - Dependencies (scala_deps, deps)
  - Visibility settings

### Scala Version

To change Scala version, edit WORKSPACE:
```python
scala_config(scala_version = "2.13.XX")
```

And update Maven artifacts to match:
```python
"org.scala-lang:scala-library:2.13.XX",
```

### Adding Dependencies

Edit `maven_install` in WORKSPACE:
```python
maven_install(
    artifacts = [
        "com.example:new-dependency_2.13:1.0.0",
        # ... existing artifacts
    ],
)
```

Then reference in BUILD.bazel:
```python
scala_library(
    name = "mymodule",
    deps = [
        "@maven//:com_example_new_dependency_2_13",
    ],
)
```

## Resources

- **Bazel**: https://bazel.build
- **rules_scala**: https://github.com/bazelbuild/rules_scala
- **rules_jvm_external**: https://github.com/bazelbuild/rules_jvm_external
- **Scalameta**: https://scalameta.org

## Support

For questions:
1. Check `BAZEL_QUICKSTART.md` for common issues
2. See `BAZEL_MIGRATION_PROGRESS.md` for technical details
3. Refer to rules_scala documentation
4. Ask in Bazel Slack #scala channel

---

**Status**: Initial configuration complete, ready for first build attempt

**Last Updated**: 2025-10-10

**Configuration Version**: Bazel 8.x, Scala 2.13.16, rules_scala 6.11.2
