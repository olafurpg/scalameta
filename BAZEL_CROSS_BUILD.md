# Cross-Building Scalameta for Scala 2.12 and 2.13 with Bazel

## Overview

This document describes how to implement cross-building for scalameta to support both Scala 2.12 and 2.13 simultaneously using Bazel. Scalameta traditionally uses sbt for cross-building, but this repo is being migrated to Bazel.

## Background: How Cross-Building Works in Different Systems

### sbt (Current Production System)
sbt uses the `crossScalaVersions` setting to build the same source code against multiple Scala versions:

```scala
crossScalaVersions := Seq("2.12.20", "2.13.16")
```

This generates separate artifacts like `scalameta_2.12` and `scalameta_2.13`.

### Bazel Challenges
Bazel doesn't have native support for building the same target with multiple Scala versions simultaneously. The community has explored several approaches (see [bazelbuild/rules_scala#80](https://github.com/bazelbuild/rules_scala/issues/80), [#393](https://github.com/bazelbuild/rules_scala/issues/393), [#962](https://github.com/bazelbuild/rules_scala/issues/962)).

## Three Approaches to Cross-Building in Bazel

### Approach 1: Databricks Universe Style (Toolchain Resolution + Platform Constraints)

**Complexity:** High
**Maintainability:** Medium
**Recommended for:** Large monorepos with complex cross-build matrices

This is how Databricks Universe achieves cross-building in their monorepo.

**Key Components:**

1. **Multiple Toolchain Registrations** - Register separate toolchains for each Scala version, differentiated by platform constraints:

```python
# In bazel/toolchains/scala/BUILD.bazel
for scala_version in ["2.12", "2.13"]:
    scala_toolchain(
        name = "scala-{}-zinc".format(scala_version),
        scala_version = scala_version,
        compiler = ":scala-worker-compiler",
    )

    toolchain(
        name = "scala-{}-toolchain".format(scala_version),
        exec_compatible_with = [
            "//bazel/platforms/scala/version:" + scala_version,
        ],
        toolchain = "scala-{}-zinc".format(scala_version),
        toolchain_type = "@rules_scala//scala:toolchain_type",
    )
```

2. **Platform Constraint Definitions** - Define platform constraints for Scala versions:

```python
# In bazel/platforms/scala/version/BUILD.bazel
constraint_setting(name = "version")

constraint_value(
    name = "2.12",
    constraint_setting = ":version",
)

constraint_value(
    name = "2.13",
    constraint_setting = ":version",
)
```

3. **Automatic Constraint Injection** - Rules automatically add constraints to trigger toolchain resolution:

```python
# In your scala rule wrapper
def scala_library(name, scala_version = "2.13", **kwargs):
    kwargs["exec_compatible_with"] = kwargs.get("exec_compatible_with", []) + [
        "//bazel/platforms/scala/version:" + scala_version,
    ]

    rules_scala.scala_library(
        name = name,
        scala_version = scala_version,
        **kwargs
    )
```

4. **Maven Dependencies Per Version** - Separate Maven installs for each Scala version:

```python
# In MODULE.bazel
maven = use_extension("@rules_jvm_external//:extensions.bzl", "maven")

maven.install(
    name = "maven_2_12",
    artifacts = [
        "org.scala-lang:scala-library:2.12.20",
        "com.lihaoyi:sourcecode_2.12:0.1.4",
    ],
)

maven.install(
    name = "maven_2_13",
    artifacts = [
        "org.scala-lang:scala-library:2.13.16",
        "com.lihaoyi:sourcecode_2.13:0.1.4",
    ],
)

use_repo(maven, "maven_2_12", "maven_2_13")
```

**Pros:**
- Very flexible for complex cross-build matrices (multiple Scala versions × multiple dependency trees)
- Leverages Bazel's toolchain resolution mechanism
- Used in production at scale (Databricks Universe)

**Cons:**
- High setup complexity (toolchains, platforms, constraints)
- Requires understanding of Bazel toolchain resolution
- Overkill for simple cross-version builds

**Universe Implementation Reference:**
- `universe/bazel/rules/cross_scala_lib.bzl` - Main cross-building macro
- `universe/bazel/toolchains/scala/BUILD.bazel` - Toolchain definitions (lines 123-171)
- `universe/bazel/toolchains/scala/toolchain.bzl` - Toolchain rule implementation

---

### Approach 2: Macro-Based Cross-Building (RECOMMENDED)

**Complexity:** Low
**Maintainability:** High
**Recommended for:** Library projects like scalameta

This approach uses a Starlark macro to generate multiple targets from a single declaration.

**Implementation:**

```python
# In cross_scala.bzl
load("@rules_scala//scala:scala.bzl", "scala_library")

def scala_cross_library(
        name,
        srcs,
        deps = [],
        scala_versions = ["2.12.20", "2.13.16"],
        default_version = "2.13.16",
        visibility = None,
        **kwargs):
    """
    Build a Scala library for multiple Scala versions.

    This generates multiple targets:
    - {name}_2_12 - Scala 2.12 version
    - {name}_2_13 - Scala 2.13 version
    - {name} - Alias to default version

    Args:
        name: Base name of the library
        srcs: Source files (shared across all versions)
        deps: Dependencies (will be transformed to versioned targets)
        scala_versions: List of full Scala versions to build for
        default_version: Default version for the unversioned alias
        **kwargs: Additional arguments passed to scala_library
    """

    version_targets = {}

    for scala_version in scala_versions:
        # Extract binary version (2.12.20 -> 2.12)
        scala_binary = scala_version.rsplit(".", 1)[0]
        version_suffix = scala_binary.replace(".", "_")
        target_name = "{}_{}".format(name, version_suffix)

        # Transform deps to point to version-specific targets
        versioned_deps = []
        for dep in deps:
            if dep.startswith(":") or dep.startswith("//"):
                # Internal dependency - add version suffix
                if dep.startswith(":"):
                    versioned_deps.append(":{}_{} ".format(dep[1:], version_suffix))
                else:
                    # Handle //package:target format
                    parts = dep.split(":")
                    if len(parts) == 2:
                        versioned_deps.append("{}:{}_{} ".format(parts[0], parts[1], version_suffix))
                    else:
                        versioned_deps.append(dep)
            elif dep.startswith("@maven"):
                # Maven dependency - replace repository
                versioned_deps.append(dep.replace("@maven", "@maven_{}".format(version_suffix)))
            else:
                # External dependency - keep as-is
                versioned_deps.append(dep)

        # Add Scala standard library
        versioned_deps.append("@maven_{}//:org_scala_lang_scala_library".format(version_suffix))

        scala_library(
            name = target_name,
            srcs = srcs,
            scala_version = scala_version,
            deps = versioned_deps,
            visibility = visibility,
            **kwargs
        )

        version_targets[scala_version] = target_name

    # Create unversioned alias to default version
    default_suffix = default_version.rsplit(".", 1)[0].replace(".", "_")
    native.alias(
        name = name,
        actual = ":{}_{}".format(name, default_suffix),
        visibility = visibility,
    )

def scala_cross_test(
        name,
        srcs,
        deps = [],
        scala_versions = ["2.12.20", "2.13.16"],
        **kwargs):
    """
    Build a Scala test for multiple Scala versions.

    Generates test targets for each Scala version.
    """

    for scala_version in scala_versions:
        scala_binary = scala_version.rsplit(".", 1)[0]
        version_suffix = scala_binary.replace(".", "_")
        target_name = "{}_{}".format(name, version_suffix)

        # Transform deps
        versioned_deps = []
        for dep in deps:
            if dep.startswith(":") or dep.startswith("//"):
                if dep.startswith(":"):
                    versioned_deps.append(":{}_{} ".format(dep[1:], version_suffix))
                else:
                    parts = dep.split(":")
                    if len(parts) == 2:
                        versioned_deps.append("{}:{}_{} ".format(parts[0], parts[1], version_suffix))
                    else:
                        versioned_deps.append(dep)
            elif dep.startswith("@maven"):
                versioned_deps.append(dep.replace("@maven", "@maven_{}".format(version_suffix)))
            else:
                versioned_deps.append(dep)

        versioned_deps.append("@maven_{}//:org_scala_lang_scala_library".format(version_suffix))

        native.scala_test(
            name = target_name,
            srcs = srcs,
            scala_version = scala_version,
            deps = versioned_deps,
            **kwargs
        )
```

**Usage in BUILD files:**

```python
load("//:cross_scala.bzl", "scala_cross_library", "scala_cross_test")

scala_cross_library(
    name = "common",
    srcs = glob([
        "shared/src/main/scala/**/*.scala",
        "jvm/src/main/scala/**/*.scala",
    ]),
    scala_versions = ["2.12.20", "2.13.16"],
    deps = [
        "//semanticdb:semanticdb",  # Will become :semanticdb_2_12 or :semanticdb_2_13
        "@maven//:com_lihaoyi_sourcecode",  # Will become @maven_2_12//:... or @maven_2_13//:...
    ],
    visibility = ["//visibility:public"],
)

scala_cross_test(
    name = "common_test",
    srcs = glob(["shared/src/test/scala/**/*.scala"]),
    scala_versions = ["2.12.20", "2.13.16"],
    deps = [
        ":common",
        "@maven//:org_scalatest_scalatest",
    ],
)
```

This generates:
- `common_2_12` - Scala 2.12 library with deps from `@maven_2_12`
- `common_2_13` - Scala 2.13 library with deps from `@maven_2_13`
- `common` - Alias to `common_2_13`
- `common_test_2_12` - Test for 2.12
- `common_test_2_13` - Test for 2.13

**Pros:**
- Simple to implement and understand
- Works with official rules_scala
- Explicit target names make debugging easy
- Good for library projects
- Easy to build specific versions: `bazel build //scalameta/common:common_2_12`

**Cons:**
- More verbose BUILD files if you have version-specific sources
- Need to manually manage dependency transformations in the macro

---

### Approach 3: Custom Scala Compilation Rule

**Complexity:** Very High
**Maintainability:** Low
**Recommended for:** Specialized use cases only

Write a custom Bazel rule that directly invokes `scalac` without using rules_scala.

**Skeleton Implementation:**

```python
# In scala_compile.bzl

def _scala_compile_impl(ctx):
    """Custom Scala compilation using direct scalac invocation"""

    scala_version = ctx.attr.scala_version
    srcs = ctx.files.srcs
    output_jar = ctx.actions.declare_file("{}.jar".format(ctx.label.name))

    # Collect classpath from deps
    classpath_files = []
    for dep in ctx.attr.deps:
        if JavaInfo in dep:
            classpath_files.extend(dep[JavaInfo].transitive_runtime_jars.to_list())

    # Get Scala compiler jars based on version
    scala_binary = scala_version.rsplit(".", 1)[0].replace(".", "_")
    scala_lib = ctx.attr._scala_libraries[scala_binary][JavaInfo].transitive_runtime_jars.to_list()[0]
    scala_compiler = ctx.attr._scala_compilers[scala_binary][JavaInfo].transitive_runtime_jars.to_list()[0]
    scala_reflect = ctx.attr._scala_reflects[scala_binary][JavaInfo].transitive_runtime_jars.to_list()[0]

    # Build classpath string
    classpath_str = ":".join([f.path for f in classpath_files + [scala_lib]])

    # Prepare scalac arguments
    args = ctx.actions.args()
    args.add("-classpath", classpath_str)
    args.add("-d", output_jar.path)
    args.add_all(ctx.attr.scalacopts)
    args.add_all([src.path for src in srcs])

    # Run scalac via Java
    ctx.actions.run(
        inputs = depset(
            direct = srcs + [scala_lib, scala_compiler, scala_reflect],
            transitive = [depset(classpath_files)],
        ),
        outputs = [output_jar],
        executable = ctx.executable._java,
        arguments = [
            "-classpath",
            ":".join([scala_compiler.path, scala_reflect.path, scala_lib.path]),
            "scala.tools.nsc.Main",
        ] + [args],
        mnemonic = "ScalaCompile",
        progress_message = "Compiling Scala {} {}".format(scala_version, ctx.label),
    )

    return [
        DefaultInfo(files = depset([output_jar])),
        JavaInfo(
            output_jar = output_jar,
            compile_jar = output_jar,
            deps = [dep[JavaInfo] for dep in ctx.attr.deps if JavaInfo in dep],
        ),
    ]

scala_compile = rule(
    implementation = _scala_compile_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = [".scala", ".java"]),
        "deps": attr.label_list(providers = [[JavaInfo]]),
        "scala_version": attr.string(mandatory = True),
        "scalacopts": attr.string_list(default = []),
        "_scala_libraries": attr.label_keyed_string_dict(
            default = {
                "@maven_2_12//:org_scala_lang_scala_library": "2_12",
                "@maven_2_13//:org_scala_lang_scala_library": "2_13",
            },
            providers = [JavaInfo],
        ),
        "_scala_compilers": attr.label_keyed_string_dict(
            default = {
                "@maven_2_12//:org_scala_lang_scala_compiler": "2_12",
                "@maven_2_13//:org_scala_lang_scala_compiler": "2_13",
            },
            providers = [JavaInfo],
        ),
        "_scala_reflects": attr.label_keyed_string_dict(
            default = {
                "@maven_2_12//:org_scala_lang_scala_reflect": "2_12",
                "@maven_2_13//:org_scala_lang_scala_reflect": "2_13",
            },
            providers = [JavaInfo],
        ),
        "_java": attr.label(
            default = "@bazel_tools//tools/jdk:java",
            executable = True,
            cfg = "exec",
        ),
    },
)
```

**Pros:**
- Complete control over compilation
- Can implement custom logic (version-specific source selection, etc.)
- No dependency on rules_scala's evolution

**Cons:**
- Must reimplement everything rules_scala provides (workers, Zinc incremental compilation, plugins, etc.)
- High maintenance burden
- Likely slower than rules_scala's optimized workers
- Need to handle Java interop, resource files, etc.

**Not recommended** unless you have very specific needs that rules_scala cannot satisfy.

---

## Recommendation for Scalameta

**Use Approach 2 (Macro-Based Cross-Building)** because:

1. ✅ **Simple** - Just a macro wrapper around existing rules_scala
2. ✅ **Maintainable** - Easy to understand and modify
3. ✅ **Explicit** - Clear target names like `common_2_12` and `common_2_13`
4. ✅ **Perfect for libraries** - Scalameta is a library project that publishes cross-versioned artifacts
5. ✅ **Works with existing setup** - You already have separate `@maven_2_12` and `@maven_2_13` repositories
6. ✅ **Easy testing** - Can run tests for both versions: `bazel test //scalameta/...:all_2_12 //scalameta/...:all_2_13`

## Implementation Steps

### Step 1: Set up Maven Dependencies (Already Done)

Your `MODULE.bazel` already has the right structure:

```python
maven = use_extension("@rules_jvm_external//:extensions.bzl", "maven")

# Add 2.12 artifacts
maven.install(
    name = "maven_2_12",
    artifacts = [
        "org.scala-lang:scala-library:2.12.20",
        "org.scala-lang:scala-compiler:2.12.20",
        "org.scala-lang:scala-reflect:2.12.20",
        "com.lihaoyi:sourcecode_2.12:0.1.4",
        "com.lihaoyi:fastparse_2.12:1.0.0",
        # ... other 2.12 dependencies
    ],
)

# Keep existing 2.13 artifacts
maven.install(
    name = "maven_2_13",
    artifacts = [
        "org.scala-lang:scala-library:2.13.16",
        # ... existing 2.13 dependencies
    ],
)

use_repo(maven, "maven_2_12", "maven_2_13")
```

### Step 2: Configure rules_scala for Multiple Versions

In your `WORKSPACE` file:

```python
load("@rules_scala//:scala_config.bzl", "scala_config")

# Configure both Scala versions
scala_config(
    scala_version = "2.13.16",
    scala_versions = ["2.12.20", "2.13.16"],
)
```

### Step 3: Create the Cross-Building Macro

Create `cross_scala.bzl` in the root (see Approach 2 implementation above).

### Step 4: Convert BUILD Files

Replace existing `scala_library` with `scala_cross_library`:

**Before:**
```python
load("@rules_scala//scala:scala.bzl", "scala_library")

scala_library(
    name = "common",
    srcs = glob(["shared/src/main/scala/**/*.scala"]),
    deps = [
        "//semanticdb:semanticdb",
        "@maven//:com_lihaoyi_sourcecode_2_13",
    ],
)
```

**After:**
```python
load("//:cross_scala.bzl", "scala_cross_library")

scala_cross_library(
    name = "common",
    srcs = glob(["shared/src/main/scala/**/*.scala"]),
    scala_versions = ["2.12.20", "2.13.16"],
    deps = [
        "//semanticdb:semanticdb",
        "@maven//:com_lihaoyi_sourcecode",  # Macro handles _2_12/_2_13 suffix
    ],
)
```

### Step 5: Build and Test

```bash
# Build all versions
bazel build //scalameta/...

# Build specific version
bazel build //scalameta/common:common_2_12
bazel build //scalameta/common:common_2_13

# Test all versions
bazel test //scalameta/...:all_2_12
bazel test //scalameta/...:all_2_13

# Query to see generated targets
bazel query //scalameta/common:*
```

### Step 6: Publishing

For publishing cross-versioned artifacts:

```python
# In a publish rule or script
for version in ["2_12", "2_13"]:
    native.genrule(
        name = "publish_scalameta_{}".format(version),
        srcs = ["//scalameta/scalameta:scalameta_{}".format(version)],
        # ... publishing logic
    )
```

## Version-Specific Source Files

If you need version-specific sources (e.g., using different collection APIs in 2.12 vs 2.13):

```python
scala_cross_library(
    name = "common",
    srcs = glob([
        "shared/src/main/scala/**/*.scala",
    ]) + select({
        "@rules_scala//scala:version_2.12": glob(["shared/src/main/scala-2.12/**/*.scala"]),
        "@rules_scala//scala:version_2.13": glob(["shared/src/main/scala-2.13/**/*.scala"]),
    }),
    # ... rest of config
)
```

## Testing Both Versions in CI

In your CI pipeline:

```bash
# Test both versions
bazel test --test_tag_filters=-manual //scalameta/...:all_2_12
bazel test --test_tag_filters=-manual //scalameta/...:all_2_13

# Or use a test suite
bazel test //tests:all_versions
```

## Troubleshooting

### Issue: "Target '...' not found"

Make sure all transitive dependencies are also cross-built. If `common` depends on `tokens`, then `tokens` must also use `scala_cross_library`.

### Issue: Dependency resolution errors

Check that Maven coordinates in deps use the correct repository:
- `@maven//:artifact` → should be `@maven//:artifact` (macro will transform it)
- Don't include `_2.12` or `_2.13` suffixes in the macro call

### Issue: Scala version mismatch warnings

Ensure all Scala dependencies use the same binary version. The macro should handle this automatically via the `@maven_2_12` vs `@maven_2_13` repository selection.

## References

- [Databricks Universe cross_scala_lib](../../universe/bazel/rules/cross_scala_lib.bzl) - Production implementation at scale
- [rules_scala Cross-Compilation Docs](https://github.com/bazelbuild/rules_scala/blob/master/docs/cross-compilation.md)
- [Eugene Yokota's Cross-Build Guide](https://eed3si9n.com/cross-build-anything-with-bazel/)
- [rules_scala Issue #962](https://github.com/bazelbuild/rules_scala/issues/962) - Multiple Scala version discussion
- [rules_jvm_external](https://github.com/bazelbuild/rules_jvm_external) - Maven dependency management

## Next Steps

1. Add Scala 2.12 dependencies to `MODULE.bazel`
2. Create `cross_scala.bzl` with the macro implementation
3. Convert one module (e.g., `scalameta/common`) as a proof of concept
4. Test both versions compile successfully
5. Roll out to remaining modules
6. Update CI to test both versions
7. Set up publishing for cross-versioned artifacts
