# Scalameta WORKSPACE
# Using WORKSPACE for rules_scala and dependencies

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# bazel_features (required by rules_scala 7.x)
http_archive(
    name = "bazel_features",
    sha256 = "2f057dd02098a106095ea291b4344257398a059eadb2c74cc470de0f9664dccd",
    strip_prefix = "bazel_features-1.28.0",
    url = "https://github.com/bazel-contrib/bazel_features/releases/download/v1.28.0/bazel_features-v1.28.0.tar.gz",
)

load("@bazel_features//:deps.bzl", "bazel_features_deps")
bazel_features_deps()

# rules_license (required by rules_scala and rules_jvm_external)
http_archive(
    name = "rules_license",
    sha256 = "26d4021f6898e23b82ef953078389dd49ac2b5618ac564ade4ef87cced147b38",
    urls = [
        "https://github.com/bazelbuild/rules_license/releases/download/1.0.0/rules_license-1.0.0.tar.gz",
    ],
)

# rules_scala
http_archive(
    name = "rules_scala",
    sha256 = "5338511555719caa1625c2a7fcb64e367db0cf19dae1505c3f6af480d35605e9",
    strip_prefix = "rules_scala-7.1.2",
    url = "https://github.com/bazelbuild/rules_scala/releases/download/v7.1.2/rules_scala-v7.1.2.tar.gz",
)

# Load and setup all rules_scala dependencies
load("@rules_scala//scala:deps.bzl", "rules_scala_dependencies")
rules_scala_dependencies()

# Initialize transitive dependencies
load("@rules_java//java:rules_java_deps.bzl", "rules_java_dependencies")
rules_java_dependencies()

load("@rules_java//java:repositories.bzl", "rules_java_toolchains")
rules_java_toolchains()

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
bazel_skylib_workspace()

# Configure scala
load("@rules_scala//:scala_config.bzl", "scala_config")
scala_config(scala_version = "2.12.20")

# Setup scala toolchains
load("@rules_scala//scala:toolchains.bzl", "scala_register_toolchains", "scala_toolchains")
scala_toolchains()
scala_register_toolchains()

# rules_jvm_external for Maven dependencies
http_archive(
    name = "rules_jvm_external",
    sha256 = "3a4d56357851cf5b0dae538b3f3e0612a4f58925dfb3cadb2e0c4e87d51e629e",
    strip_prefix = "rules_jvm_external-6.5",
    url = "https://github.com/bazelbuild/rules_jvm_external/releases/download/6.5/rules_jvm_external-6.5.tar.gz",
)

# Maven dependencies
load("@rules_jvm_external//:defs.bzl", "maven_install")

# Scala 2.12 dependencies
maven_install(
    name = "maven_2_12",
    artifacts = [
        # Scala standard library and compiler
        "org.scala-lang:scala-library:2.12.20",
        "org.scala-lang:scala-compiler:2.12.20",
        "org.scala-lang:scala-reflect:2.12.20",
        "org.scala-lang:scalap:2.12.20",

        # ScalaPB for protobuf support
        "com.thesamet.scalapb:scalapb-runtime_2.12:1.0.0",
        "com.thesamet.scalapb:lenses_2.12:1.0.0",
        "com.thesamet.scalapb:scalapb-runtime-grpc_2.12:1.0.0",
        "com.thesamet.scalapb:compilerplugin_2.12:1.0.0",

        # Common dependencies
        "com.lihaoyi:sourcecode_2.12:0.1.4",
        "com.lihaoyi:fastparse_2.12:1.0.0",
        "com.lihaoyi:fansi_2.12:0.2.5",
        "com.lihaoyi:geny_2.12:0.1.2",

        # Test dependencies
        "org.scalatest:scalatest_2.12:3.2.0-SNAP10",
        "org.scalacheck:scalacheck_2.12:1.13.5",

        # For testkit (Java libraries - no Scala version suffix)
        "org.rauschig:jarchivelib:0.7.1",
        "commons-io:commons-io:2.5",
        "com.googlecode.java-diff-utils:diffutils:1.3.0",

        # Coursier for tests
        "io.get-coursier:coursier_2.12:2.1.0",
        "io.get-coursier:coursier-cache_2.12:2.1.0",

        # Protobuf (Java library - no Scala version suffix)
        "com.google.protobuf:protobuf-java:3.25.5",
    ],
    fetch_sources = True,
    repositories = [
        "https://repo1.maven.org/maven2",
    ],
)

# Scala 2.13 dependencies
maven_install(
    name = "maven_2_13",
    artifacts = [
        # Scala standard library and compiler
        "org.scala-lang:scala-library:2.13.16",
        "org.scala-lang:scala-compiler:2.13.16",
        "org.scala-lang:scala-reflect:2.13.16",
        "org.scala-lang:scalap:2.13.16",

        # ScalaPB for protobuf support
        "com.thesamet.scalapb:scalapb-runtime_2.13:1.0.0",
        "com.thesamet.scalapb:lenses_2.13:1.0.0",
        "com.thesamet.scalapb:scalapb-runtime-grpc_2.13:1.0.0",
        "com.thesamet.scalapb:compilerplugin_2.13:1.0.0",

        # Common dependencies
        "com.lihaoyi:sourcecode_2.13:0.1.4",
        "com.lihaoyi:fastparse_2.13:1.0.0",
        "com.lihaoyi:fansi_2.13:0.2.5",
        "com.lihaoyi:geny_2.13:0.1.2",

        # Test dependencies
        "org.scalatest:scalatest_2.13:3.2.0-SNAP10",
        "org.scalacheck:scalacheck_2.13:1.13.5",

        # For testkit (Java libraries - no Scala version suffix)
        "org.rauschig:jarchivelib:0.7.1",
        "commons-io:commons-io:2.5",
        "com.googlecode.java-diff-utils:diffutils:1.3.0",

        # Coursier for tests
        "io.get-coursier:coursier_2.13:2.1.0",
        "io.get-coursier:coursier-cache_2.13:2.1.0",

        # Protobuf (Java library - no Scala version suffix)
        "com.google.protobuf:protobuf-java:3.25.5",
    ],
    fetch_sources = True,
    repositories = [
        "https://repo1.maven.org/maven2",
    ],
)
