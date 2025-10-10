"""
Cross-building support for Scala 2.12 and 2.13

This macro-based approach generates separate targets for each Scala version
from a single declaration, following the pattern:
- {name}_2_12 - Scala 2.12 version
- {name}_2_13 - Scala 2.13 version
- {name} - Alias to default version (2.13)
"""

load("@rules_scala//scala:scala.bzl", "scala_library", "scala_test")

def scala_cross_library(
        name,
        srcs = [],
        scala_deps = [],
        deps = [],
        exports = [],
        runtime_deps = [],
        data = [],
        resources = [],
        resource_strip_prefix = None,
        scalacopts = [],
        javacopts = [],
        javac_jvm_flags = [],
        scalac_jvm_flags = [],
        main_class = None,
        visibility = None,
        scala_versions = ["2.12.20", "2.13.16"],
        default_version = "2.13.16",
        tags = [],
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
        scala_deps: Scala library dependencies (will be transformed to versioned targets)
        deps: Regular dependencies (Maven deps will be transformed to versioned repos)
        exports: Exported dependencies (transformed like deps)
        runtime_deps: Runtime dependencies (transformed like deps)
        data: Data files
        resources: Resource files
        resource_strip_prefix: Prefix to strip from resource paths
        scalacopts: Scala compiler options
        javacopts: Java compiler options
        javac_jvm_flags: JVM flags for javac
        scalac_jvm_flags: JVM flags for scalac
        main_class: Main class for the library
        visibility: Target visibility
        scala_versions: List of full Scala versions to build for
        default_version: Default version for the unversioned alias
        tags: Build tags
        **kwargs: Additional arguments passed to scala_library
    """

    for scala_version in scala_versions:
        # Extract binary version (2.12.20 -> 2.12 -> 2_12)
        scala_binary = scala_version.rsplit(".", 1)[0]
        version_suffix = scala_binary.replace(".", "_")
        target_name = "{}_{}".format(name, version_suffix)

        # Transform scala_deps to point to version-specific targets
        versioned_scala_deps = []
        for dep in scala_deps:
            if dep.startswith(":"):
                # Local dependency - add version suffix
                versioned_scala_deps.append(":{}_{}".format(dep[1:], version_suffix))
            elif dep.startswith("//"):
                # Absolute label - add version suffix to target
                if ":" in dep:
                    # //package:target format
                    package, target = dep.rsplit(":", 1)
                    versioned_scala_deps.append("{}:{}_{}".format(package, target, version_suffix))
                else:
                    # //package format (implicit target name)
                    package_name = dep.split("/")[-1]
                    versioned_scala_deps.append("//{}:{}_{}".format(dep[2:], package_name, version_suffix))
            else:
                # External dependency - keep as-is
                versioned_scala_deps.append(dep)

        # Transform deps to use version-specific Maven repositories
        versioned_deps = []
        for dep in deps:
            if dep.startswith("@maven//") or dep.startswith("@maven//:"):
                # Maven dependency - replace repository name
                versioned_deps.append(dep.replace("@maven", "@maven_{}".format(version_suffix)))
            else:
                # Non-maven dependency (could be Java lib or external)
                versioned_deps.append(dep)

        # Add Scala standard library from the appropriate repository
        versioned_deps.append("@maven_{}//:org_scala_lang_scala_library".format(version_suffix))

        # Merge scala_deps and regular deps together
        all_deps = versioned_scala_deps + versioned_deps

        # Transform exports
        versioned_exports = []
        for exp in exports:
            if exp.startswith(":"):
                versioned_exports.append(":{}_{}".format(exp[1:], version_suffix))
            elif exp.startswith("//"):
                if ":" in exp:
                    package, target = exp.rsplit(":", 1)
                    versioned_exports.append("{}:{}_{}".format(package, target, version_suffix))
                else:
                    package_name = exp.split("/")[-1]
                    versioned_exports.append("//{}:{}_{}".format(exp[2:], package_name, version_suffix))
            elif exp.startswith("@maven"):
                versioned_exports.append(exp.replace("@maven", "@maven_{}".format(version_suffix)))
            else:
                versioned_exports.append(exp)

        # Transform runtime_deps
        versioned_runtime_deps = []
        for rdep in runtime_deps:
            if rdep.startswith("@maven"):
                versioned_runtime_deps.append(rdep.replace("@maven", "@maven_{}".format(version_suffix)))
            else:
                versioned_runtime_deps.append(rdep)

        scala_library(
            name = target_name,
            srcs = srcs,
            scala_version = scala_version,
            deps = all_deps,
            exports = versioned_exports,
            runtime_deps = versioned_runtime_deps,
            data = data,
            resources = resources,
            resource_strip_prefix = resource_strip_prefix,
            scalacopts = scalacopts,
            javacopts = javacopts,
            javac_jvm_flags = javac_jvm_flags,
            scalac_jvm_flags = scalac_jvm_flags,
            main_class = main_class,
            visibility = visibility,
            tags = tags,
            **kwargs
        )

    # Create unversioned alias to default version
    default_suffix = default_version.rsplit(".", 1)[0].replace(".", "_")
    native.alias(
        name = name,
        actual = ":{}_{}".format(name, default_suffix),
        visibility = visibility,
    )

def scala_cross_test(
        name,
        srcs = [],
        scala_deps = [],
        deps = [],
        data = [],
        resources = [],
        resource_strip_prefix = None,
        scalacopts = [],
        javacopts = [],
        javac_jvm_flags = [],
        scalac_jvm_flags = [],
        jvm_flags = [],
        main_class = None,
        visibility = None,
        scala_versions = ["2.12.20", "2.13.16"],
        tags = [],
        **kwargs):
    """
    Build a Scala test for multiple Scala versions.

    Generates test targets for each Scala version:
    - {name}_2_12 - Scala 2.12 test
    - {name}_2_13 - Scala 2.13 test

    Args:
        name: Base name of the test
        srcs: Source files
        scala_deps: Scala library dependencies (transformed to versioned targets)
        deps: Regular dependencies (Maven deps transformed to versioned repos)
        data: Data files
        resources: Resource files
        resource_strip_prefix: Prefix to strip from resource paths
        scalacopts: Scala compiler options
        javacopts: Java compiler options
        javac_jvm_flags: JVM flags for javac
        scalac_jvm_flags: JVM flags for scalac
        jvm_flags: JVM flags for test execution
        main_class: Main class for the test
        visibility: Target visibility
        scala_versions: List of Scala versions to test
        tags: Test tags
        **kwargs: Additional arguments passed to scala_test
    """

    for scala_version in scala_versions:
        scala_binary = scala_version.rsplit(".", 1)[0]
        version_suffix = scala_binary.replace(".", "_")
        target_name = "{}_{}".format(name, version_suffix)

        # Transform scala_deps
        versioned_scala_deps = []
        for dep in scala_deps:
            if dep.startswith(":"):
                versioned_scala_deps.append(":{}_{}".format(dep[1:], version_suffix))
            elif dep.startswith("//"):
                if ":" in dep:
                    package, target = dep.rsplit(":", 1)
                    versioned_scala_deps.append("{}:{}_{}".format(package, target, version_suffix))
                else:
                    package_name = dep.split("/")[-1]
                    versioned_scala_deps.append("//{}:{}_{}".format(dep[2:], package_name, version_suffix))
            else:
                versioned_scala_deps.append(dep)

        # Transform deps
        versioned_deps = []
        for dep in deps:
            if dep.startswith("@maven"):
                versioned_deps.append(dep.replace("@maven", "@maven_{}".format(version_suffix)))
            else:
                versioned_deps.append(dep)

        versioned_deps.append("@maven_{}//:org_scala_lang_scala_library".format(version_suffix))

        # Merge scala_deps and regular deps together
        all_deps = versioned_scala_deps + versioned_deps

        scala_test(
            name = target_name,
            srcs = srcs,
            scala_version = scala_version,
            deps = all_deps,
            data = data,
            resources = resources,
            resource_strip_prefix = resource_strip_prefix,
            scalacopts = scalacopts,
            javacopts = javacopts,
            javac_jvm_flags = javac_jvm_flags,
            scalac_jvm_flags = scalac_jvm_flags,
            jvm_flags = jvm_flags,
            main_class = main_class,
            visibility = visibility,
            tags = tags,
            **kwargs
        )
