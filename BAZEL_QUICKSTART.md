# Bazel Quickstart for Scalameta

## Prerequisites

- Bazel 8.x or later
- Java 11
- Protocol buffer compiler (for regenerating semanticdb protos)

## Initial Setup

1. **Fetch dependencies:**
   ```bash
   bazel sync
   ```

2. **Generate Protobuf files (required before first build):**
   ```bash
   # Use sbt to generate the protobuf Scala files
   sbt semanticdbJVM/compile

   # This generates files in:
   # semanticdb/semanticdb/jvm/target/scala-2.13/src_managed/
   ```

## Building

### Build individual modules

```bash
# Build common module
bazel build //scalameta/common

# Build with all dependencies
bazel build //scalameta/dialects
bazel build //scalameta/parsers
```

### Build the main scalameta target

```bash
bazel build //scalameta/scalameta:scalameta
```

### Build all modules

```bash
bazel build //scalameta/...
```

## Querying

### Show dependencies

```bash
# Show all dependencies of scalameta
bazel query 'deps(//scalameta/scalameta:scalameta)'

# Show direct dependencies only
bazel query 'deps(//scalameta/scalameta:scalameta, 1)'
```

### Visualize dependency graph

```bash
bazel query 'deps(//scalameta/scalameta:scalameta)' --output graph > graph.dot
dot -Tpng graph.dot -o graph.png
```

## Cleaning

```bash
# Clean build outputs (keeps external dependencies)
bazel clean

# Clean everything including external dependencies
bazel clean --expunge
```

## Configuration

### Bazel Configuration (.bazelrc)

Key settings:
- Java 11 for compilation
- Scala worker strategy for faster builds
- Disk cache at `~/.cache/bazel/scalameta`

### Customizing the Build

To change Scala version, edit `WORKSPACE`:
```python
scala_config(scala_version = "2.13.XX")
```

To add Maven dependencies, edit `maven_install` in `WORKSPACE`:
```python
maven_install(
    artifacts = [
        "your.group:artifact:version",
        # ... more artifacts
    ],
    # ...
)
```

## Troubleshooting

### Build fails with "No such package"

Make sure all BUILD.bazel files are present:
```bash
find . -name "BUILD.bazel" -type f
```

### Maven dependency not found

Try syncing dependencies:
```bash
bazel sync
```

Or clear and refetch:
```bash
bazel clean --expunge
bazel sync
```

### Protobuf compilation errors

Regenerate protobuf files:
```bash
sbt clean
sbt semanticdbJVM/compile
```

### Macro compilation issues

If you see macro-related errors, check:
1. scala-reflect is in dependencies
2. Source files are in correct directories
3. All required compiler plugins are configured

## Performance Tips

1. **Use workers**: Already configured in .bazelrc for Scala compilation

2. **Remote cache**: Set up remote caching for CI
   ```bash
   # In .bazelrc
   build --remote_cache=https://your-cache-server
   ```

3. **Disk cache**: Already configured at `~/.cache/bazel/scalameta`

4. **Incremental builds**: Bazel automatically handles incremental compilation

## Migration Status

See [BAZEL_MIGRATION_PROGRESS.md](BAZEL_MIGRATION_PROGRESS.md) for detailed migration status and next steps.

## Current Limitations

1. **Protobuf generation**: Currently requires sbt to pre-generate files
2. **JS/Native platforms**: Only JVM platform is supported
3. **Scalac plugins**: Not yet migrated
4. **Tests**: Test modules not yet migrated

## Getting Help

- Bazel documentation: https://bazel.build
- rules_scala: https://github.com/bazelbuild/rules_scala
- Scalameta: https://scalameta.org
