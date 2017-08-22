package scala.meta

import org.scalameta.invariants._
import scala.meta.classifiers._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.prettyprinters._
import scala.meta.internal.trees._

@root trait Tree extends InternalTree {
  def parent: Option[Tree]
  def children: List[Tree]

  def pos: Position
  def tokens(implicit dialect: Dialect): Tokens

  final override def canEqual(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  final override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  final override def hashCode: Int = System.identityHashCode(this)
  final override def toString = scala.meta.internal.prettyprinters.TreeToString(this)
}

object Tree extends InternalTreeXtensions {
  implicit def classifiable[T <: Tree]: Classifiable[T] = null
  implicit def showStructure[T <: Tree]: Structure[T] = scala.meta.internal.prettyprinters.TreeStructure.apply[T]
  implicit def showSyntax[T <: Tree](implicit dialect: Dialect): Syntax[T] = scala.meta.internal.prettyprinters.TreeSyntax.apply[T](dialect)
}

@branch trait Ref extends Tree
@branch trait Stat extends Tree

@branch trait Name extends Ref { def value: String }
object Name {
  def apply(value: String): Name = if (value == "") Name.Anonymous() else Name.Indeterminate(value)
  def unapply(name: Name): Option[String] = Some(name.value)
  @ast case class Anonymous() extends Name {
    def value = ""
    checkParent(ParentChecks.NameAnonymous)
  }
  @ast case class Indeterminate(value: Predef.String @nonEmpty) extends Name
}

@branch trait Lit extends Term with Pat with Type {
  def value: Any
}
object Lit {
  def unapply(arg: Lit): Option[Any] = Some(arg.value)
  @ast case class Null() extends Lit { def value: Any = null }
  @ast case class Int(value: scala.Int) extends Lit
  // NOTE: Lit.Double/Float are strings to work the same across JS/JVM. Example:
  // 1.4f.toString == "1.399999976158142" // in JS
  // 1.4f.toString == "1.4"               // in JVM
  // See https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
  @ast case class Double(format: scala.Predef.String) extends Lit { val value = format.toDouble }
  object Double { def apply(double: scala.Double): Double = Lit.Double(double.toString)  }
  @ast case class Float(format: scala.Predef.String) extends Lit { val value = format.toFloat }
  object Float { def apply(float: scala.Float): Float = Lit.Float(float.toString)  }
  @ast case class Byte(value: scala.Byte) extends Lit
  @ast case class Short(value: scala.Short) extends Lit
  @ast case class Char(value: scala.Char) extends Lit
  @ast case class Long(value: scala.Long) extends Lit
  @ast case class Boolean(value: scala.Boolean) extends Lit
  @ast case class Unit() extends Lit { def value: Any = () }
  @ast case class String(value: scala.Predef.String) extends Lit
  @ast case class Symbol(value: scala.Symbol) extends Lit
}

@branch trait Term extends Stat
object Term {
  @branch trait Ref extends Term with scala.meta.Ref
  @ast case class This(qual: scala.meta.Name) extends Term.Ref
  @ast case class Super(thisp: scala.meta.Name, superp: scala.meta.Name) extends Term.Ref
  @ast case class Name(value: Predef.String @nonEmpty) extends scala.meta.Name with Term.Ref with Pat
  @ast case class Select(qual: Term, name: Term.Name) extends Term.Ref with Pat
  @ast case class Interpolate(prefix: Name, parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
    checkFields(parts.length == args.length + 1)
  }
  @ast case class Xml(parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
    checkFields(parts.length == args.length + 1)
  }
  @ast case class Apply(fun: Term, args: List[Term]) extends Term
  @ast case class ApplyType(fun: Term, targs: List[Type] @nonEmpty) extends Term
  @ast case class ApplyInfix(lhs: Term, op: Name, targs: List[Type], args: List[Term]) extends Term
  @ast case class ApplyUnary(op: Name, arg: Term) extends Term.Ref {
    checkFields(op.isUnaryOp)
  }
  @ast case class Assign(lhs: Term, rhs: Term) extends Term {
    checkFields(lhs.is[Term.Quasi] || lhs.is[Term.Ref] || lhs.is[Term.Apply])
    checkParent(ParentChecks.TermAssign)
  }
  @ast case class Return(expr: Term) extends Term
  @ast case class Throw(expr: Term) extends Term
  @ast case class Ascribe(expr: Term, tpe: Type) extends Term
  @ast case class Annotate(expr: Term, annots: List[Mod.Annot] @nonEmpty) extends Term
  @ast case class Tuple(args: List[Term] @nonEmpty) extends Term {
    // tuple must have more than one element
    // however, this element may be Quasi with "hidden" list of elements inside
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Term.Quasi]))
  }
  @ast case class Block(stats: List[Stat]) extends Term {
    checkFields(stats.forall(_.isBlockStat))
  }
  @ast case class If(cond: Term, thenp: Term, elsep: Term) extends Term
  @ast case class Match(expr: Term, cases: List[Case] @nonEmpty) extends Term
  @ast case class Try(expr: Term, catchp: List[Case], finallyp: Option[Term]) extends Term
  @ast case class TryWithHandler(expr: Term, catchp: Term, finallyp: Option[Term]) extends Term
  @ast case class Function(params: List[Term.Param], body: Term) extends Term {
    checkFields(params.forall(param => param.is[Term.Param.Quasi] || (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)))
    checkFields(params.exists(_.is[Term.Param.Quasi]) || params.exists(_.mods.exists(_.is[Mod.Implicit])) ==> (params.length == 1))
  }
  @ast case class PartialFunction(cases: List[Case] @nonEmpty) extends Term
  @ast case class While(expr: Term, body: Term) extends Term
  @ast case class Do(body: Term, expr: Term) extends Term
  @ast case class For(enums: List[Enumerator] @nonEmpty, body: Term) extends Term {
    checkFields(enums.head.is[Enumerator.Generator] || enums.head.is[Enumerator.Quasi])
  }
  @ast case class ForYield(enums: List[Enumerator] @nonEmpty, body: Term) extends Term
  @ast case class New(init: Init) extends Term
  @ast case class NewAnonymous(templ: Template) extends Term
  @ast case class Placeholder() extends Term
  @ast case class Eta(expr: Term) extends Term
  @ast case class Repeated(expr: Term) extends Term {
    checkParent(ParentChecks.TermRepeated)
  }
  @ast case class Param(mods: List[Mod], name: meta.Name, decltpe: Option[Type], default: Option[Term]) extends Member
  def fresh(): Term.Name = fresh("fresh")
  def fresh(prefix: String): Term.Name = Term.Name(prefix + Fresh.nextId())
}

@branch trait Type extends Tree
object Type {
  @branch trait Ref extends Type with scala.meta.Ref
  @ast case class Name(value: String @nonEmpty) extends scala.meta.Name with Type.Ref
  @ast case class Select(qual: Term.Ref, name: Type.Name) extends Type.Ref {
    checkFields(qual.isPath || qual.is[Term.Super] || qual.is[Term.Ref.Quasi])
  }
  @ast case class Project(qual: Type, name: Type.Name) extends Type.Ref
  @ast case class Singleton(ref: Term.Ref) extends Type.Ref {
    checkFields(ref.isPath || ref.is[Term.Super])
  }
  @ast case class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
  @ast case class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
  @ast case class Function(params: List[Type], res: Type) extends Type
  @ast case class ImplicitFunction(params: List[Type], res: Type) extends Type
  @ast case class Tuple(args: List[Type] @nonEmpty) extends Type {
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Type.Quasi]))
  }
  @ast case class With(lhs: Type, rhs: Type) extends Type
  @ast case class And(lhs: Type, rhs: Type) extends Type
  @ast case class Or(lhs: Type, rhs: Type) extends Type
  @ast case class Refine(tpe: Option[Type], stats: List[Stat]) extends Type {
    checkFields(stats.forall(_.isRefineStat))
  }
  @ast case class Existential(tpe: Type, stats: List[Stat] @nonEmpty) extends Type {
    checkFields(stats.forall(_.isExistentialStat))
  }
  @ast case class Annotate(tpe: Type, annots: List[Mod.Annot] @nonEmpty) extends Type
  @ast case class Placeholder(bounds: Bounds) extends Type
  @ast case class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
  @ast case class ByName(tpe: Type) extends Type {
    checkParent(ParentChecks.TypeByName)
  }
  @ast case class Repeated(tpe: Type) extends Type {
    checkParent(ParentChecks.TypeRepeated)
  }
  @ast case class Var(name: Name) extends Type with Member.Type {
    checkFields(name.value(0).isLower)
    checkParent(ParentChecks.TypeVar)
  }
  @ast case class Param(mods: List[Mod],
                        name: meta.Name,
                        tparams: List[Type.Param],
                        tbounds: Type.Bounds,
                        vbounds: List[Type],
                        cbounds: List[Type]) extends Member
  def fresh(): Type.Name = fresh("fresh")
  def fresh(prefix: String): Type.Name = Type.Name(prefix + Fresh.nextId())
}

@branch trait Pat extends Tree
object Pat {
  @ast case class Var(name: scala.meta.Term.Name) extends Pat with Member.Term {
    // NOTE: can't do this check here because of things like `val X = 2`
    // checkFields(name.value(0).isLower)
    checkParent(ParentChecks.PatVar)
  }
  @ast case class Wildcard() extends Pat
  @ast case class SeqWildcard() extends Pat {
    checkParent(ParentChecks.PatSeqWildcard)
  }
  @ast case class Bind(lhs: Pat, rhs: Pat) extends Pat {
    checkFields(lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
  }
  @ast case class Alternative(lhs: Pat, rhs: Pat) extends Pat
  @ast case class Tuple(args: List[Pat] @nonEmpty) extends Pat {
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat.Quasi]))
  }
  @ast case class Extract(fun: Term, args: List[Pat]) extends Pat {
    checkFields(fun.isExtractor)
  }
  @ast case class ExtractInfix(lhs: Pat, op: Term.Name, rhs: List[Pat]) extends Pat
  @ast case class Interpolate(prefix: Term.Name, parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
    checkFields(parts.length == args.length + 1)
  }
  @ast case class Xml(parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
    checkFields(parts.length == args.length + 1)
  }
  @ast case class Typed(lhs: Pat, rhs: Type) extends Pat {
    checkFields(lhs.is[Pat.Wildcard] || lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
    checkFields(!rhs.is[Type.Var] && !rhs.is[Type.Placeholder])
  }
  def fresh(): Pat.Var = Pat.Var(Term.fresh())
  def fresh(prefix: String): Pat.Var = Pat.Var(Term.fresh(prefix))
}

@branch trait Member extends Tree {
  def name: Name
}
object Member {
  @branch trait Term extends Member {
    def name: scala.meta.Term.Name
  }
  @branch trait Type extends Member {
    def name: scala.meta.Type.Name
  }
}

@branch trait Decl extends Stat
object Decl {
  @ast case class Val(mods: List[Mod],
                      pats: List[Pat] @nonEmpty,
                      decltpe: scala.meta.Type) extends Decl
  @ast case class Var(mods: List[Mod],
                      pats: List[Pat] @nonEmpty,
                      decltpe: scala.meta.Type) extends Decl
  @ast case class Def(mods: List[Mod],
                      name: Term.Name,
                      tparams: List[scala.meta.Type.Param],
                      paramss: List[List[Term.Param]],
                      decltpe: scala.meta.Type) extends Decl with Member.Term
  @ast case class Type(mods: List[Mod],
                       name: scala.meta.Type.Name,
                       tparams: List[scala.meta.Type.Param],
                       bounds: scala.meta.Type.Bounds) extends Decl with Member.Type
}

@branch trait Defn extends Stat
object Defn {
  @ast case class Val(mods: List[Mod],
                      pats: List[Pat] @nonEmpty,
                      decltpe: Option[scala.meta.Type],
                      rhs: Term) extends Defn {
    checkFields(pats.forall(!_.is[Term.Name]))
  }
  @ast case class Var(mods: List[Mod],
                      pats: List[Pat] @nonEmpty,
                      decltpe: Option[scala.meta.Type],
                      rhs: Option[Term]) extends Defn {
    checkFields(pats.forall(!_.is[Term.Name]))
    checkFields(decltpe.nonEmpty || rhs.nonEmpty)
    checkFields(rhs.isEmpty ==> pats.forall(_.is[Pat.Var]))
  }
  @ast case class Def(mods: List[Mod],
                      name: Term.Name,
                      tparams: List[scala.meta.Type.Param],
                      paramss: List[List[Term.Param]],
                      decltpe: Option[scala.meta.Type],
                      body: Term) extends Defn with Member.Term
  @ast case class Macro(mods: List[Mod],
                        name: Term.Name,
                        tparams: List[scala.meta.Type.Param],
                        paramss: List[List[Term.Param]],
                        decltpe: Option[scala.meta.Type],
                        body: Term) extends Defn with Member.Term
  @ast case class Type(mods: List[Mod],
                       name: scala.meta.Type.Name,
                       tparams: List[scala.meta.Type.Param],
                       body: scala.meta.Type) extends Defn with Member.Type
  @ast case class Class(mods: List[Mod],
                        name: scala.meta.Type.Name,
                        tparams: List[scala.meta.Type.Param],
                        ctor: Ctor.Primary,
                        templ: Template) extends Defn with Member.Type
  @ast case class Trait(mods: List[Mod],
                        name: scala.meta.Type.Name,
                        tparams: List[scala.meta.Type.Param],
                        ctor: Ctor.Primary,
                        templ: Template) extends Defn with Member.Type {
    checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
  }
  @ast case class Object(mods: List[Mod],
                         name: Term.Name,
                         templ: Template) extends Defn with Member.Term {
    checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
  }
}

@ast case class Pkg(ref: Term.Ref, stats: List[Stat])
     extends Member.Term with Stat {
  checkFields(ref.isQualId)
  checkFields(stats.forall(_.isTopLevelStat))
  def name: Term.Name = ref match {
    case name: Term.Name => name
    case Term.Select(_, name: Term.Name) => name
  }
}
object Pkg {
  @ast case class Object(mods: List[Mod], name: Term.Name, templ: Template)
       extends Member.Term with Stat {
    checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
  }
}

// NOTE: The names of Ctor.Primary and Ctor.Secondary here is always Name.Anonymous.
// While seemingly useless, this name is crucial to one of the key principles behind the semantic API:
// "every definition and every reference should carry a name".
@branch trait Ctor extends Tree with Member
object Ctor {
  @ast case class Primary(mods: List[Mod],
                          name: Name,
                          paramss: List[List[Term.Param]]) extends Ctor
  @ast case class Secondary(mods: List[Mod],
                            name: Name,
                            paramss: List[List[Term.Param]] @nonEmpty,
                            init: Init,
                            stats: List[Stat]) extends Ctor with Stat {
    checkFields(stats.forall(_.isBlockStat))
  }
}

// NOTE: The name here is always Name.Anonymous.
// See comments to Ctor.Primary and Ctor.Secondary for justification.
@ast case class Init(tpe: Type, name: Name, argss: List[List[Term]]) extends Ref {
  checkFields(tpe.isConstructable)
  checkParent(ParentChecks.Init)
}

@ast case class Self(name: Name, decltpe: Option[Type]) extends Member

@ast case class Template(early: List[Stat],
                         inits: List[Init],
                         self: Self,
                         stats: List[Stat]) extends Tree {
  checkFields(early.forall(_.isEarlyStat && inits.nonEmpty))
  checkFields(stats.forall(_.isTemplateStat))
}

@branch trait Mod extends Tree
object Mod {
  @ast case class Annot(init: Init) extends Mod {
    @deprecated("Use init instead", "1.9.0")
    def body = init
  }
  @ast case class Private(within: Ref) extends Mod {
    checkFields(within.isWithin)
  }
  @ast case class Protected(within: Ref) extends Mod {
    checkFields(within.isWithin)
  }
  @ast case class Implicit() extends Mod
  @ast case class Final() extends Mod
  @ast case class Sealed() extends Mod
  @ast case class Override() extends Mod
  @ast case class Case() extends Mod
  @ast case class Abstract() extends Mod
  @ast case class Covariant() extends Mod
  @ast case class Contravariant() extends Mod
  @ast case class Lazy() extends Mod
  @ast case class ValParam() extends Mod
  @ast case class VarParam() extends Mod
  @ast case class Inline() extends Mod
}

@branch trait Enumerator extends Tree
object Enumerator {
  @ast case class Generator(pat: Pat, rhs: Term) extends Enumerator
  @ast case class Val(pat: Pat, rhs: Term) extends Enumerator
  @ast case class Guard(cond: Term) extends Enumerator
}

@ast case class Import(importers: List[Importer] @nonEmpty) extends Stat

@ast case class Importer(ref: Term.Ref, importees: List[Importee] @nonEmpty) extends Tree {
  checkFields(ref.isStableId)
}

@branch trait Importee extends Tree with Ref
object Importee {
  @ast case class Wildcard() extends Importee
  @ast case class Name(name: scala.meta.Name) extends Importee {
    checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
  }
  @ast case class Rename(name: scala.meta.Name, rename: scala.meta.Name) extends Importee {
    checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
    checkFields(rename.is[scala.meta.Name.Quasi] || rename.is[scala.meta.Name.Indeterminate])
  }
  @ast case class Unimport(name: scala.meta.Name) extends Importee {
    checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
  }
}

@ast case class Case(pat: Pat, cond: Option[Term], body: Term) extends Tree

@ast case class Source(stats: List[Stat]) extends Tree {
  // NOTE: This validation has been removed to allow dialects with top-level terms.
  // Ideally, we should push the validation into a dialect-specific prettyprinter when #220 is fixed.
  // checkFields(stats.forall(_.isTopLevelStat))
}

package internal.trees {
  // NOTE: Quasi is a base trait for a whole bunch of classes.
  // Every root, branch and ast trait/class among scala.meta trees (except for quasis themselves)
  // has a corresponding quasi, e.g. Term.Quasi or Type.Quasi.
  //
  // Here's how quasis represent unquotes
  // (XXX below depends on the position where the unquote occurs, e.g. q"$x" will result in Term.Quasi):
  //   * $x => XXX.Quasi(0, XXX.Name("x"))
  //   * ..$xs => XXX.Quasi(1, XXX.Quasi(0, XXX.Name("xs"))
  //   * ...$xss => XXX.Quasi(2, XXX.Quasi(0, XXX.Name("xss"))
  //   * ..{$fs($args)} => Complex ellipses aren't supported yet
  @branch trait Quasi extends Tree {
    def rank: Int
    def tree: Tree
    def pt: Class[_]
    def become[T <: Quasi : AstInfo]: T
  }

  @registry object All
}
