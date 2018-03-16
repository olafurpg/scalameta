### Java

In this section, we exhaustively map Java language features onto SemanticDB.
As a reference, we use the [JVM Specification][jvms] (referred to as "JVMS" in the text below)
and [Java Language Specification][jls] (referred to as "JLS" in the text below).

<a name="java-symbol"></a>
#### Symbol

In this section, we describe the Java symbol format.

<table>
  <tr>
    <td><b>Symbols</b></td>
    <td><b>Format</b></td>
  </tr>
  <tr>
    <td>
      Global symbols
      <a href="#symbol">↑</a>
    </td>
    <td>
      <ul>
        <li>
          For root package <a href="https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#package-references">[20]</a>,
          its descriptor.
        </li>
        <li>
          For empty package <a href="https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#packagings">[21]</a>,
          its descriptor.
        </li>
        <li>
          For top-level package, its descriptor.
        </li>
        <li>
          For other definition, concatenation of owner symbol and
          definition descriptor.
        </li>
      </ul>
    </td>
  </tr>
</table>

Note that Java symbols do not have [local symbols](#symbol), unlike the [Scala symbol format](#scala-symbol).

**Owner** is:
  * For root package, `None`.
  * For empty package, root package.
  * For top-level package, root package.
  * For other package, parent package.
  * For other top-level definition, its package.
  * For other other global definition, the innermost enclosing definition.
  * For other definitions, `None`.

**Descriptor** is:
  * For `LOCAL`, `MACRO`, `OBJECT`, `TRAIT`, `SELF_PARAMETER`, `TYPE` or `PACKAGE_OBJECT`, unsupported.
  * For `FIELD` or `PACKAGE`, concatenation of its simple name and a dot (`.`).
  * For `METHOD` or `CONSTRUCTOR`, concatenation of its simple name, a disambiguator, and a dot (`.`).
  * For `CLASS` or `INTERFACE`, concatenation of its simple name and a pound sign (`#`).
  * For `PARAMETER`, concatenation of a left parenthesis (`(`), its simple name and a right parenthesis (`)`).
  * For `TYPE_PARAMETER`, concatenation of a left bracket (`[`), its simple name and a right bracket (`]`).
  * See [SymbolInformation](#java-symbolinformation) for details on
    which Scala definitions are modelled by which symbols.

**Disambiguator** is:
  * Concatenation of a left parenthesis (`(`), a type descriptor
    and a right parenthesis (`)`).
    In the case when multiple definitions have the same kind, name and
    type descriptor, the type descriptor is appended with `+N`,
    with `+1` going to the method that is defined first in the source code,
    `+2` going to the method that is defined second, etc.

**Name** is:
  * For root package, `_root_`.
  * For empty package, `_empty_`.
  * For constructor, `<init>`.
  * For fields, methods, classes and interfaces, the simple name of the binding introduced by the definition.

**Type descriptor** is:
  * For `TYPE_REF`, encoded name of `symbol`.
  * For `REPEATED_TYPE`, concatenation of type descriptor of `tpe` and a star (`*`).
  * For other types, unsupported.
  * See [Type](#java-type) for details on which Java types are modelled by which `Type` entities.

  For example, this is how some of the definitions from the Java standard library must be modeled.

  * The `java` package: `java.`
  * The `Integer` class: `java.lang.Integer#`
  * The `int` primitive: `scala.Int#`
  * The `Arrays.asList` method: `java.util.Arrays#asList(T*).`
  * The `a` parameter of that method: `java.util.Arrays#asList(T*).(a)`
  * The `T` type parameter of that method: `java.util.Arrays#asList(T*).[T]`

<a name="java-type"></a>
#### Type

```protobuf
message Type {
  enum Tag {
    reserved 2, 3, 4, 5;
    UNKNOWN_TYPE = 0;
    TYPE_REF = 1;
    SINGLETON_TYPE = 15;
    INTERSECTION_TYPE = 16;
    UNION_TYPE = 17;
    WITH_TYPE = 18;
    STRUCTURAL_TYPE = 6;
    ANNOTATED_TYPE = 7;
    EXISTENTIAL_TYPE = 8;
    UNIVERSAL_TYPE = 9;
    CLASS_INFO_TYPE = 10;
    METHOD_TYPE = 11;
    BY_NAME_TYPE = 12;
    REPEATED_TYPE = 13;
    TYPE_TYPE = 14;
  }
  reserved 3, 4, 5, 6;
  Tag tag = 1;
  TypeRef typeRef = 2;
  SingletonType singletonType = 16;
  IntersectionType intersectionType = 17;
  UnionType unionType = 18;
  WithType withType = 19;
  StructuralType structuralType = 7;
  AnnotatedType annotatedType = 8;
  ExistentialType existentialType = 9;
  UniversalType universalType = 10;
  ClassInfoType classInfoType = 11;
  MethodType methodType = 12;
  ByNameType byNameType = 13;
  RepeatedType repeatedType = 14;
  TypeType typeType = 15;
}
```

In Java, [Type](#type) represents types [\[74\]][74].

<table>
  <tr>
    <td width="220px"><b>Category</b></td>
    <td><b>Examples</b></td>
  </tr>
  <tr>
    <td valign="top">Primitive types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.2">75</a>]</td>
    <td>
      <ul>
        <li><code>byte</code> ~ <code>TypeRef(None, &lt;scala.Byte#&gt;, List())</code>.</li>
        <li><code>short</code> ~ <code>TypeRef(None, &lt;scala.Short#&gt;, List())</code>.</li>
        <li><code>int</code> ~ <code>TypeRef(None, &lt;scala.Int#&gt;, List())</code>.</li>
        <li><code>long</code> ~ <code>TypeRef(None, &lt;scala.Long#&gt;, List())</code>.</li>
        <li><code>char</code> ~ <code>TypeRef(None, &lt;scala.Char#&gt;, List())</code>.</li>
        <li><code>float</code> ~ <code>TypeRef(None, &lt;scala.Float#&gt;, List())</code>.</li>
        <li><code>double</code> ~ <code>TypeRef(None, &lt;scala.Double#&gt;, List())</code>.</li>
        <li><code>boolean</code> ~ <code>TypeRef(None, &lt;scala.Boolean#&gt;, List())</code>.</li>
        <li><code>void</code> ~ <code>TypeRef(None, &lt;scala.Unit#&gt;, List())</code>.</li>
      </ul>
    </td> </tr>
  <tr>
    <td valign="top">Type variable [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.4">78</a>]</td>
    <td>
      <ul>
        <li><code>T</code> ~ <code>TypeType(List(), None, None)</code>.</li>
        <li><code>T extends S</code> ~ <code>TypeType(List(), None, Some(&lt;S&gt;))</code>.</li>
        <li><code>T extends S1 &amp; ... &amp; Sn </code> ~ <code>TypeType(List(), None, Some(IntersectionType(&lt;S1&gt;, ..., &lt;Sn&gt;)))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Parameterized types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.5">79</a>]</td>
    <td>
      <ul>
        <li><code>C&lt;T1, ..., Tn&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>C&lt;?&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;local_wildcard&gt;))</code>.</li>
        <li><code>C&lt;? extends T&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;local_wildcard&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Array types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-10.html#jls-10.1">80</a>]</td>
    <td>
      <ul>
        <li><code>T[]</code> ~ <code>TypeRef(None, &lt;scala.Array#&gt;, List(&lt;T&gt;))</code>.</li>
        <li><code>int[]</code> ~ <code>TypeRef(None, &lt;scala.Array#&gt;, List(&lt;scala.Int#&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Intersection types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.9">81</a>]</td>
    <td>
      <ul>
        <li><code>T1 &amp; ... &amp; Tn</code> ~ <code>IntersectionType(&lt;T1&gt;, ..., &lt;Tn&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
</table>

Notes:
* Primitive and array types are converted to their equivalent [Scala type](#scala-type) representations.
* Wildcard type arguments of parameterized types are currently incorrectly represented as `local_wildcard`,
  this behavior is expected to change in the future and should not be relied upon.
* Method throws clauses are discarded.

<a name="java-symbolinformation"></a>
#### SymbolInformation

```protobuf
message SymbolInformation {
  reserved 2, 6, 7, 8, 9, 10, 12;
  string symbol = 1;
  Language language = 16;
  Kind kind = 3;
  int32 properties = 4;
  string name = 5;
  Type tpe = 11;
  repeated Annotation annotations = 13;
  Accessibility accessibility = 14;
  string owner = 15;
}
```

<table>
  <tr>
    <td><b>Field</b></td> <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>symbol</code></td>
    <td>See <a href="#java-symbol">Symbol</a>.</td>
  </tr>
  <tr>
    <td><code>language</code></td>
    <td><code>JAVA</code>.</td>
  </tr>
  <tr>
    <td><code>kind</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>properties</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>name</code></td>
    <td>See <a href="#java-symbol">Symbol</a>.</td>
  </tr>
  <tr>
    <td><code>tpe</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>annotations</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>accessibility</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>owner</code></td>
    <td>See <a href="#java-symbol">Symbol</a>.</td>
  </tr>
</table>

**Class declarations** [\[76\]][76] are represented by a single symbol with the `CLASS` kind.

```java
package a;
class C extends S1 implements I {
  String m1;
  static unit m2();
  int m3(String e) throws E;
  static class D1<T1 extends S2 & S3, T2> { }
  class D2 { }
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>C</code></td>
    <td><code>a.C#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(&lt;S1&gt;, &lt;I&gt;), List(&lt;a.C#m1&gt;, &lt;a.C#m3(String).&gt;, &lt;a.C#D2#&gt;, &lt;a.C#m2().&gt;, &lt;a.C#D1#&gt;))</code></td>
  </tr>
  <tr>
    <td><code>m1</code></td>
    <td><code>a.C#m1.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;java.lang.String&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>m2</code></td>
    <td><code>a.C#m2().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List() TypeRef(None, &lt;scala.Unit#&gt;))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a.C#m3(String).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(&lt;a.C#m3(String).(e)&gt;) TypeRef(None, &lt;scala.Int#&gt;))</code></td>
  </tr>
  <tr>
    <td><code>e</code></td>
    <td><code>a.C#m3(String).(e)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;java.lang.String#&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>e</code></td>
    <td><code>a.C#D1#[T1]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, Some(IntersectionType(List(&lt;S2&gt;, &lt;S2&gt;))))</code></td>
  </tr>
  <tr>
    <td><code>e</code></td>
    <td><code>a.C#D1#[T2]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>D1</code></td>
    <td><code>a.C#D1#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(&lt;a.C#D1#[T1]&gt;, &lt;a.C#D1#[T2]&gt;), List(&lt;java.lang.Object#&gt;), List())</code></td>
  </tr>
  <tr>
    <td><code>D2</code></td>
    <td><code>a.C#D2#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(), List())</code></td>
  </tr>
</table>

Notes:
* Class members must appear in the following order:
  * non-static members first, following the same order as they appear in the original source
  * static members secondly, following the same order as they appear in the original source
* A Java class maps to a single symbol with type `ClassInfoType` including all static and non-static members.
  This departs from the Scala compiler internal representation of Java classes where non-static members
  are grouped under a `CLASS` symbol and static members are grouped under an `OBJECT` symbol.
* Supported properties for `CLASS` symbols are
  * `FINAL` set for all final classes
  * `ABSTRACT` set for all final classes
  * `STATIC` set for static classes
  * `ENUM` set for enum declarations

**Enum types** [\[84\]][84] are represented by a single symbol with the `CLASS` kind.

```java
package a;

public enum Coin {
  PENNY, NICKEL
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>Coin</code></td>
    <td><code>a.Coin#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(&lt;Enum&lt;Coin&gt;&gt;), List(a.Coin#PENNY., a.Coin#NICKEL., ))</code></td>
  </tr>
  <tr>
    <td><code>Coin</code></td>
    <td><code>a.Coin#PENNY.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;a.Coin#&gt;), List())</code></td>
  </tr>
  <tr>
    <td><code>Coin</code></td>
    <td><code>a.Coin#NICKEL.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;a.Coin#&gt;), List())</code></td>
  </tr>
</table>

Notes:
* Enum types follow the same rules as class declarations.
* Enum type classes and enum fields must have the property `ENUM` set.
* Enum fields have the type of their enclosing class.

**Interface declarations** [\[77\]][77] are represented by a single symbol like classes but with the `INTERFACE` kind.
Concretely, the differences between interface symbols and class symbols are:
* Interface symbols don't any properties.
* Traits don't have symbols.

**Method declarations** [\[82\]][82] are represented by a single symbol with the `METHOD` kind and one symbol for each method parameter with kind `PARAMETER`.
Notes:
* When compiled with `-parameters` , the name of method parameters matches their name written in source. Otherwise, parameters have the name `paramN` where `N` is the index of that given parameter starting at index 0.

<a name="java-accessibility"></a>
#### Accessibility

```protobuf
message Accessibility {
  enum Tag {
    UNKNOWN_ACCESSIBILITY = 0;
    PRIVATE = 1;
    PRIVATE_THIS = 2;
    PRIVATE_WITHIN = 3;
    PROTECTED = 4;
    PROTECTED_THIS = 5;
    PROTECTED_WITHIN = 6;
    PUBLIC = 7;
  }
  Tag tag = 1;
  string symbol = 2;
}
```

In Java, [Accessibility](#accessibility) represents accessibility of definitions.
<table>
  <tr>
    <td><b>Accessibility</b></td>
    <td><b>Code</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>PRIVATE</code></td>
    <td><code>private unit m() {}</code></td>
    <td>
      Can be accessed only from within the directly enclosing class.
    </td>
  </tr>
  <tr>
    <td><code>PRIVATE_WITHIN</code></td>
    <td><code>package x; class A {}</code></td>
    <td>
      Class `A` can be accessed only from within the package <code>x</code>.
    </td>
  </tr>
  <tr>
    <td><code>PROTECTED</code></td>
    <td><code>protected unit m() {}</code></td>
    <td>
      Can be accessed from within: 1) the enclosing class, 2) all classes that have the enclosing class as a base class.
    </td>
  </tr>
  <tr>
    <td><code>PUBLIC</code></td>
    <td><code>public void m() {}</code></td>
    <td>
      Can be accessed from anywhere.
    </td>
  </tr>
</table>

Notes:
* `PRIVATE_THIS`, `PROTECTED_THIS`, `PROTECTED_WITHIN` are not supported in the Java language.

<a name="java-symboloccurrence"></a>
#### SymbolOccurrence

At this moment, there is no tool that supports SymbolOccurrences for the Java language.

[72]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-10.html#jls-10.1
[73]: https://docs.oracle.com/javase/specs/jls/kkkkse8/html/jls-4.html#jls-PrimitiveType
[74]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html
[75]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.2
[76]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.1
[77]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.1
[78]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.4
[79]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.5
[80]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-10.html#jls-10.1
[81]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.9
[82]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4
[83]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.3
[84]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.9