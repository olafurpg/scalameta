package scala.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.classifiers._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.prettyprinters._
import scala.meta.internal.ast._
import scala.meta.internal.ast.Helpers._

@root trait Tree extends InternalTree {
  def parent: Option[Tree]
  def children: Seq[Tree]

  def pos: Position
  def tokens(implicit dialect: Dialect): Tokens

  final override def canEqual(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  final override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  final override def hashCode: Int = System.identityHashCode(this)
  final override def toString = scala.meta.internal.prettyprinters.TreeToString(this)
}

object Tree extends InternalTreeXtensions {
  implicit def classifiable[T <: Tree]: Classifiable[T] = null
  implicit def showStructure[T <: Tree](implicit options: Options): Structure[T] = scala.meta.internal.prettyprinters.TreeStructure.apply[T](options)
  implicit def showSyntax[T <: Tree](implicit dialect: Dialect, options: Options): Syntax[T] = scala.meta.internal.prettyprinters.TreeSyntax.apply[T](dialect, options)
}

@branch trait Ref extends Tree
@branch trait Stat extends Tree

@branch trait Name extends Ref { def value: String }
object Name {
  def unapply(name: Name): Option[String] = Some(name.value)
  @ast class Anonymous extends Name with Term.Param.Name with Type.Param.Name with Qualifier { def value = "_" }
  @ast class Indeterminate(value: Predef.String @nonEmpty) extends Name with Qualifier
  @branch trait Qualifier extends Ref
}

@branch trait Term extends Stat
object Term {
  @branch trait Ref extends Term with scala.meta.Ref
  @ast class This(qual: scala.meta.Name.Qualifier) extends Term.Ref with scala.meta.Name.Qualifier {
    checkFields(!qual.is[Term.This])
  }
  @ast class Super(thisp: scala.meta.Name.Qualifier, superp: scala.meta.Name.Qualifier) extends Term.Ref {
    checkFields(!thisp.is[Term.This] && !superp.is[Term.This])
  }
  @ast class Name(value: Predef.String @nonEmpty) extends scala.meta.Name with Term.Ref with Pat with Param.Name with scala.meta.Name.Qualifier
  @ast class Select(qual: Term, name: Term.Name) extends Term.Ref with Pat
  @ast class Interpolate(prefix: Name, parts: Seq[Lit] @nonEmpty, args: Seq[Term]) extends Term {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Xml(parts: Seq[Lit] @nonEmpty, args: Seq[Term]) extends Term {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Apply(fun: Term, args: Seq[Term]) extends Term with Ctor.Call
  @ast class ApplyType(fun: Term, targs: Seq[Type] @nonEmpty) extends Term with Ctor.Call
  @ast class ApplyInfix(lhs: Term, op: Name, targs: Seq[Type], args: Seq[Term]) extends Term
  @ast class ApplyUnary(op: Name, arg: Term) extends Term.Ref {
    checkFields(op.isUnaryOp)
  }
  @ast class Assign(lhs: Term.Ref, rhs: Term) extends Term {
    checkParent(ParentChecks.TermAssign)
  }
  @ast class Update(fun: Term, argss: Seq[Seq[Term]] @nonEmpty, rhs: Term) extends Term
  @ast class Return(expr: Term) extends Term
  @ast class Throw(expr: Term) extends Term
  @ast class Ascribe(expr: Term, tpe: Type) extends Term
  @ast class Annotate(expr: Term, annots: Seq[Mod.Annot] @nonEmpty) extends Term with Ctor.Call
  @ast class Tuple(args: Seq[Term] @nonEmpty) extends Term {
    // tuple must have more than one element
    // however, this element may be Quasi with "hidden" list of elements inside
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Term.Quasi]))
  }
  @ast class Block(stats: Seq[Stat]) extends Term {
    checkFields(stats.forall(_.isBlockStat))
  }
  @ast class If(cond: Term, thenp: Term, elsep: Term) extends Term
  @ast class Match(expr: Term, cases: Seq[Case] @nonEmpty) extends Term
  @ast class Try(expr: Term, catchp: Seq[Case], finallyp: Option[Term]) extends Term
  @ast class TryWithHandler(expr: Term, catchp: Term, finallyp: Option[Term]) extends Term
  @ast class Function(params: Seq[Term.Param], body: Term) extends Term {
    checkFields(params.forall(param => param.is[Term.Param.Quasi] || (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)))
    checkFields(params.exists(_.is[Term.Param.Quasi]) || params.exists(_.mods.exists(_.is[Mod.Implicit])) ==> (params.length == 1))
  }
  @ast class PartialFunction(cases: Seq[Case] @nonEmpty) extends Term
  @ast class While(expr: Term, body: Term) extends Term
  @ast class Do(body: Term, expr: Term) extends Term
  @ast class For(enums: Seq[Enumerator] @nonEmpty, body: Term) extends Term {
    checkFields(enums.head.is[Enumerator.Generator])
  }
  @ast class ForYield(enums: Seq[Enumerator] @nonEmpty, body: Term) extends Term
  @ast class New(templ: Template) extends Term
  @ast class Placeholder() extends Term
  @ast class Eta(expr: Term) extends Term
  @ast class Repeated(expr: Term) extends Term {
    checkParent(ParentChecks.TermRepeated)
  }
  @ast class Param(mods: Seq[Mod], name: Param.Name, decltpe: Option[Type], default: Option[Term]) extends Member
  object Param {
    @branch trait Name extends scala.meta.Name
  }
  def fresh(): Term.Name = fresh("fresh")
  def fresh(prefix: String): Term.Name = Term.Name(prefix + Fresh.nextId())
}

@branch trait Type extends Tree
object Type {
  @branch trait Ref extends Type with scala.meta.Ref
  @ast class Name(value: String @nonEmpty) extends scala.meta.Name with Type.Ref with Pat.Type.Ref with Param.Name with scala.meta.Name.Qualifier
  @ast class Select(qual: Term.Ref, name: Type.Name) extends Type.Ref with Pat.Type.Ref {
    checkFields(qual.isPath || qual.is[Term.Super] || qual.is[Term.Ref.Quasi])
  }
  @ast class Project(qual: Type, name: Type.Name) extends Type.Ref
  @ast class Singleton(ref: Term.Ref) extends Type.Ref with Pat.Type.Ref {
    checkFields(ref.isPath || ref.is[Term.Super])
  }
  @ast class Apply(tpe: Type, args: Seq[Type] @nonEmpty) extends Type
  @ast class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
  @ast class Function(params: Seq[Type], res: Type) extends Type
  @ast class Tuple(args: Seq[Type] @nonEmpty) extends Type {
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Type.Quasi]))
  }
  @ast class With(lhs: Type, rhs: Type) extends Type
  @ast class And(lhs: Type, rhs: Type) extends Type
  @ast class Or(lhs: Type, rhs: Type) extends Type
  @ast class Refine(tpe: Option[Type], stats: Seq[Stat]) extends Type {
    checkFields(stats.forall(_.isRefineStat))
  }
  @ast class Existential(tpe: Type, stats: Seq[Stat] @nonEmpty) extends Type {
    checkFields(stats.forall(_.isExistentialStat))
  }
  @ast class Annotate(tpe: Type, annots: Seq[Mod.Annot] @nonEmpty) extends Type
  @ast class Placeholder(bounds: Bounds) extends Type
  @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
  @ast class ByName(tpe: Type) extends Type {
    checkParent(ParentChecks.TypeByName)
  }
  @ast class Repeated(tpe: Type) extends Type {
    checkParent(ParentChecks.TypeRepeated)
  }
  @ast class Param(mods: Seq[Mod],
                   name: Param.Name,
                   tparams: Seq[Type.Param],
                   tbounds: Type.Bounds,
                   vbounds: Seq[Type],
                   cbounds: Seq[Type]) extends Member
  object Param {
    @branch trait Name extends scala.meta.Name
  }
  def fresh(): Type.Name = fresh("fresh")
  def fresh(prefix: String): Type.Name = Type.Name(prefix + Fresh.nextId())
  implicit class XtensionType(tpe: Type) {
    def pat: Pat.Type = Helpers.tpeToPattpe(tpe)
    def ctorRef(ctor: Ctor.Name): Ctor.Call = Helpers.tpeToCtorref(tpe, ctor)
  }
}

@branch trait Pat extends Tree with Pat.Arg
object Pat {
  // TODO: Introduction of Pat.Var.Term and Pat.Var.Type is a very far-reaching design decision.
  //
  // Here we would like to model Scala's extravagant binding rules for term and type variables in patterns,
  // according to which both `x` and ``x`` might mean same or different things depending on whether they are used
  // a) in normal contexts, b) in patterns, c) in special parts of patterns.
  // Concretely, `X` in `X + 2` and `case X` means the same, whereas `x` in `x + 2` and `x` does not.
  // Also, `T` in `val x: T = ...` and `case x: T => ...` means the same, whereas `t` in the same conditions does not.
  //
  // The two approaches to this are as follows:
  // 1) Model names in both bindee and binder roles as the same AST node (i.e. Term.Name for terms and Type.Name for types)
  // 2) Model bindees as Term.Name/Type.Name and binders as Pat.Var.Term and Pat.Var.Type
  //
  // Benefits of the first approach:
  // + One less AST node for terms
  // + A lot less AST nodes for types (type vars are viral in type patterns, so we have to replicate almost the entire type hierarchy!)
  //
  // Benefits of the second approach:
  // + Impossible to mix up bindee/binder roles when unquoting
  // + The aforementioned safety guarantee is static
  // + Trivial to figure out whether a name is a bindee or binder
  // + Does not conflate Name and Member (in the first approach, Name has to be Member, because names in binder role are members)
  //
  // Here are the arguments that attempt to adjust the first approach to address the lack of benefits of the second approach:
  // + Term.Name and Type.Name are names, so they have denotations, and that's where we can keep track of the role.
  //   We have to make sure that we unquote binder names correctly (not $x, but ${x.name}).
  //   We also have to make sure that we unquote pattern types correctly, and that should be done in a deep fashion!
  // + On multiple occasions, we have sacrificed static safety guarantees in favor of more compact solutions.
  // + In the first approach, it's also possible to figure out the role of a given name, even though in a bit trickier fashion.
  // + This conflation might be unfortunate, but it doesn't create soundness holes.
  //
  // After a lot of deliberation, I've picked the second approach.
  // However the benefits of the first approach are definitely tangible, and we will need to revisit this choice later.
  @branch trait Var extends Tree
  object Var {
    @ast class Term(name: scala.meta.Term.Name) extends Var with Pat with Member.Term {
      // NOTE: Unlike in Pat.Var.Term, we can't say `checkFields(name.value(0).isLower)` here,
      // because we model things like `val X = 2` with abstract syntax nodes
      // like `Defn.Val(Nil, List(Pat.Var.Term(Term.Name("X"))), None, Lit.Int(2))`.
      //
      // This is a very annoying inconsistency, because capitalized Pat.Var.Terms
      // are only allowed at the top level of left-hand sides in vals and vars.
      // As a result, we have to externalize this checkFields to every use site of Pat.Var.Terms
      // except for those vals/vars.
      //
      // TODO: It might be nice to avoid this inconsistency by redesigning this whole Pat.Var.XXX approach.
      // However, that will imply significant changes in our tree structure, and I'd like to avoid that before 0.1.
      checkParent(ParentChecks.PatVarTerm)
    }
    @ast class Type(name: scala.meta.Type.Name) extends Var with Pat.Type with Member.Type {
      checkFields(name.value(0).isLower)
    }
  }
  @ast class Wildcard() extends Pat
  @ast class Bind(lhs: Pat.Var.Term, rhs: Pat.Arg) extends Pat
  @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
  @ast class Tuple(args: Seq[Pat] @nonEmpty) extends Pat {
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat.Quasi]))
  }
  @ast class Extract(ref: Term.Ref, targs: Seq[scala.meta.Pat.Type], args: Seq[Pat.Arg]) extends Pat {
    checkFields(ref.isStableId)
  }
  @ast class ExtractInfix(lhs: Pat, op: Term.Name, rhs: Seq[Pat.Arg] @nonEmpty) extends Pat
  @ast class Interpolate(prefix: Term.Name, parts: Seq[Lit] @nonEmpty, args: Seq[Pat]) extends Pat {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Xml(parts: Seq[Lit] @nonEmpty, args: Seq[Pat.Arg]) extends Pat {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Typed(lhs: Pat, rhs: Pat.Type) extends Pat {
    checkFields(lhs.is[Pat.Wildcard] || lhs.is[Pat.Var.Term] || lhs.is[Pat.Quasi])
    checkFields(!rhs.is[Pat.Var.Type] && !rhs.is[Pat.Type.Wildcard])
  }
  @branch trait Arg extends Tree
  object Arg {
    @ast class SeqWildcard() extends Arg
  }
  @branch trait Type extends Tree
  object Type {
    @branch trait Ref extends Pat.Type with scala.meta.Ref
    @ast class Wildcard() extends Pat.Type
    @ast class Project(qual: Pat.Type, name: scala.meta.Type.Name) extends Pat.Type with Pat.Type.Ref {
      checkFields(!qual.is[Pat.Var.Type] && !qual.is[Pat.Type.Wildcard])
    }
    @ast class Apply(tpe: Pat.Type, args: Seq[Pat.Type] @nonEmpty) extends Pat.Type {
      checkFields(!tpe.is[Pat.Var.Type] && !tpe.is[Pat.Type.Wildcard])
    }
    @ast class ApplyInfix(lhs: Pat.Type, op: scala.meta.Type.Name, rhs: Pat.Type) extends Pat.Type
    @ast class Function(params: Seq[Pat.Type], res: Pat.Type) extends Pat.Type
    @ast class Tuple(args: Seq[Pat.Type] @nonEmpty) extends Pat.Type {
      checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat.Type.Quasi]))
    }
    @ast class With(lhs: Pat.Type, rhs: Pat.Type) extends Pat.Type
    @ast class And(lhs: Pat.Type, rhs: Pat.Type) extends Pat.Type
    @ast class Or(lhs: Pat.Type, rhs: Pat.Type) extends Pat.Type
    @ast class Refine(tpe: Option[Pat.Type], stats: Seq[Stat]) extends Pat.Type {
      checkFields(tpe.isEmpty || (!tpe.get.is[Pat.Var.Type] && !tpe.get.is[Pat.Type.Wildcard]))
      checkFields(stats.forall(_.isRefineStat))
    }
    @ast class Existential(tpe: Pat.Type, stats: Seq[Stat] @nonEmpty) extends Pat.Type {
      checkFields(!tpe.is[Pat.Var.Type] && !tpe.is[Pat.Type.Wildcard])
      checkFields(stats.forall(_.isExistentialStat))
    }
    @ast class Annotate(tpe: Pat.Type, annots: Seq[Mod.Annot] @nonEmpty) extends Pat.Type {
      checkFields(!tpe.is[Pat.Var.Type] && !tpe.is[Pat.Type.Wildcard])
    }
    @ast class Placeholder(bounds: scala.meta.Type.Bounds) extends Pat.Type {
      checkFields(bounds.lo.nonEmpty || bounds.hi.nonEmpty)
    }
    def fresh(): Pat.Var.Type = Pat.Var.Type(scala.meta.Type.fresh())
    def fresh(prefix: String): Pat.Var.Type = Pat.Var.Type(scala.meta.Type.fresh(prefix))
    implicit class XtensionPatType(pattpe: Pat.Type) {
      def tpe: scala.meta.Type = Helpers.pattpeToTpe(pattpe)
    }
  }
  def fresh(): Pat.Var.Term = Pat.Var.Term(Term.fresh())
  def fresh(prefix: String): Pat.Var.Term = Pat.Var.Term(Term.fresh(prefix))
}


@branch trait Lit extends Term with Pat with Type with Pat.Type {
  def value: Any
}
object Lit {
  def unapply(arg: Lit): Option[scala.Any] = Some(arg.value)
  @ast class Null(value: scala.Any) extends Lit
  @ast class Int(value: scala.Int) extends Lit
  // NOTE: Lit.Double/Float are strings to work the same across JS/JVM. Example:
  // 1.4f.toString == "1.399999976158142" // in JS
  // 1.4f.toString == "1.4"               // in JVM
  // See https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
  @ast class Double(format: scala.Predef.String) extends Lit { val value = format.toDouble }
  object Double { def apply(double: scala.Double): Double = Lit.Double(double.toString)  }
  @ast class Float(format: scala.Predef.String) extends Lit { val value = format.toFloat }
  object Float { def apply(float: scala.Float): Float = Lit.Float(float.toString)  }
  @ast class Byte(value: scala.Byte) extends Lit
  @ast class Short(value: scala.Short) extends Lit
  @ast class Char(value: scala.Char) extends Lit
  @ast class Long(value: scala.Long) extends Lit
  @ast class Boolean(value: scala.Boolean) extends Lit
  @ast class Unit(value: scala.Unit) extends Lit
  @ast class String(value: scala.Predef.String) extends Lit
  @ast class Symbol(value: scala.Symbol) extends Lit
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
  @ast class Val(mods: Seq[Mod],
                 pats: Seq[Pat.Var.Term] @nonEmpty,
                 decltpe: scala.meta.Type) extends Decl
  @ast class Var(mods: Seq[Mod],
                 pats: Seq[Pat.Var.Term] @nonEmpty,
                 decltpe: scala.meta.Type) extends Decl
  @ast class Def(mods: Seq[Mod],
                 name: Term.Name,
                 tparams: Seq[scala.meta.Type.Param],
                 paramss: Seq[Seq[Term.Param]],
                 decltpe: scala.meta.Type) extends Decl with Member.Term
  @ast class Type(mods: Seq[Mod],
                  name: scala.meta.Type.Name,
                  tparams: Seq[scala.meta.Type.Param],
                  bounds: scala.meta.Type.Bounds) extends Decl with Member.Type
}

@branch trait Defn extends Stat
object Defn {
  @ast class Val(mods: Seq[Mod],
                 pats: Seq[Pat] @nonEmpty,
                 decltpe: Option[scala.meta.Type],
                 rhs: Term) extends Defn {
    checkFields(pats.forall(!_.is[Term.Name]))
  }
  @ast class Var(mods: Seq[Mod],
                 pats: Seq[Pat] @nonEmpty,
                 decltpe: Option[scala.meta.Type],
                 rhs: Option[Term]) extends Defn {
    checkFields(pats.forall(!_.is[Term.Name]))
    checkFields(decltpe.nonEmpty || rhs.nonEmpty)
    checkFields(rhs.isEmpty ==> pats.forall(_.is[Pat.Var.Term]))
  }
  @ast class Def(mods: Seq[Mod],
                 name: Term.Name,
                 tparams: Seq[scala.meta.Type.Param],
                 paramss: Seq[Seq[Term.Param]],
                 decltpe: Option[scala.meta.Type],
                 body: Term) extends Defn with Member.Term
  @ast class Macro(mods: Seq[Mod],
                   name: Term.Name,
                   tparams: Seq[scala.meta.Type.Param],
                   paramss: Seq[Seq[Term.Param]],
                   decltpe: Option[scala.meta.Type],
                   body: Term) extends Defn with Member.Term
  @ast class Type(mods: Seq[Mod],
                  name: scala.meta.Type.Name,
                  tparams: Seq[scala.meta.Type.Param],
                  body: scala.meta.Type) extends Defn with Member.Type
  @ast class Class(mods: Seq[Mod],
                   name: scala.meta.Type.Name,
                   tparams: Seq[scala.meta.Type.Param],
                   ctor: Ctor.Primary,
                   templ: Template) extends Defn with Member.Type
  @ast class Trait(mods: Seq[Mod],
                   name: scala.meta.Type.Name,
                   tparams: Seq[scala.meta.Type.Param],
                   ctor: Ctor.Primary,
                   templ: Template) extends Defn with Member.Type {
    checkFields(templ.is[Template.Quasi] || templ.stats.getOrElse(Nil).forall(!_.is[Ctor]))

    // TODO this doesn't work because the Dialect in implicit
    // scope is the dialect of the host Scala environment
    // (i.e. Scala211), not the parser's dialect.
    //checkFields (
    //  implicitly[Dialect].allowTraitParameters ||
    //    (ctor.mods.isEmpty && ctor.paramss.isEmpty)
    //)
  }
  @ast class Object(mods: Seq[Mod],
                    name: Term.Name,
                    templ: Template) extends Defn with Member.Term {
    checkFields(templ.is[Template.Quasi] || templ.stats.getOrElse(Nil).forall(!_.is[Ctor]))
  }
}

@ast class Pkg(ref: Term.Ref, stats: Seq[Stat])
     extends Member.Term with Stat {
  checkFields(ref.isQualId)
  checkFields(stats.forall(_.isTopLevelStat))
  def name: Term.Name = ref match {
    case name: Term.Name => name
    case Term.Select(_, name: Term.Name) => name
  }
}
object Pkg {
  @ast class Object(mods: Seq[Mod], name: Term.Name, templ: Template)
       extends Member.Term with Stat {
    checkFields(templ.is[Template.Quasi] || templ.stats.getOrElse(Nil).forall(!_.is[Ctor]))
  }
}

@branch trait Ctor extends Tree with Member
object Ctor {
  @branch trait Call extends Term
  @ast class Primary(mods: Seq[Mod],
                     name: Ctor.Name,
                     paramss: Seq[Seq[Term.Param]]) extends Ctor
  @ast class Secondary(mods: Seq[Mod],
                       name: Ctor.Name,
                       paramss: Seq[Seq[Term.Param]] @nonEmpty,
                       body: Term) extends Ctor with Stat {
    checkFields(body.isCtorBody)
  }
  @branch trait Ref extends scala.meta.Term.Ref with Ctor.Call
  val Name = Ref.Name
  type Name = Ref.Name
  object Ref {
    // TODO: current design with Ctor.Name(value) has a problem of sometimes meaningless `value`
    // for example, q"def this() = ..." is going to have Ctor.Name("this"), because we're parsing
    // this constructor outside of any enclosure, so we can't call it Ctor.Name("C") or Ctor.Name("D")
    // an alternative design might reintroduce the Ctor.Ref ast node that would have the structure of:
    // Ctor.Ref(tpe: Type, ctor: Ctor.Name), where Ctor.Name would be Ctor.Name()
    // in that design, we also won't have to convert between Type and Ctor.Ref hierarchies, which is a definite plus
    @ast class Name(value: String @nonEmpty) extends scala.meta.Name with Ref
    @ast class Select(qual: Term.Ref, name: Name) extends Ref
    @ast class Project(qual: Type, name: Name) extends Ref
    @ast class Function(name: Name) extends Ref
  }
  def fresh(): Ctor.Name = fresh("fresh")
  def fresh(prefix: String): Ctor.Name = Ctor.Name(prefix + Fresh.nextId())
}

@ast class Template(early: Seq[Stat],
                    parents: Seq[Ctor.Call],
                    self: Term.Param,
                    stats: Option[Seq[Stat]]) extends Tree {
  checkFields(parents.forall(_.isCtorCall))
  checkFields(early.forall(_.isEarlyStat && parents.nonEmpty))
  checkFields(stats.getOrElse(Nil).forall(_.isTemplateStat))
}

@branch trait Mod extends Tree
object Mod {
  @ast class Annot(body: Term) extends Mod {
    checkFields(body.isCtorCall)
  }
  @ast class Private(within: Name.Qualifier) extends Mod
  @ast class Protected(within: Name.Qualifier) extends Mod
  @ast class Implicit() extends Mod
  @ast class Final() extends Mod
  @ast class Sealed() extends Mod
  @ast class Override() extends Mod
  @ast class Case() extends Mod
  @ast class Abstract() extends Mod
  @ast class Covariant() extends Mod
  @ast class Contravariant() extends Mod
  @ast class Lazy() extends Mod
  // TODO: ValParam and VarParam being mods might not be the best idea ever
  // however the alternative is to have a dedicated XXX.Param type for class/trait parameters
  // and that has proven to be very clunky (e.g. such XXX.Param type has to be a supertype for Term.Param)
  @ast class ValParam() extends Mod
  @ast class VarParam() extends Mod
  @ast class Inline() extends Mod
}

@branch trait Enumerator extends Tree
object Enumerator {
  @ast class Generator(pat: Pat, rhs: Term) extends Enumerator
  @ast class Val(pat: Pat, rhs: Term) extends Enumerator
  @ast class Guard(cond: Term) extends Enumerator
}

@ast class Import(importers: Seq[Importer] @nonEmpty) extends Stat

@ast class Importer(ref: Term.Ref, importees: Seq[Importee] @nonEmpty) extends Tree {
  checkFields(ref.isStableId)
}

@branch trait Importee extends Tree with Ref
object Importee {
  @ast class Wildcard() extends Importee
  @ast class Name(name: scala.meta.Name.Indeterminate) extends Importee
  @ast class Rename(name: scala.meta.Name.Indeterminate, rename: scala.meta.Name.Indeterminate) extends Importee
  @ast class Unimport(name: scala.meta.Name.Indeterminate) extends Importee
}

@ast class Case(pat: Pat, cond: Option[Term], body: Term) extends Tree

@ast class Source(stats: Seq[Stat]) extends Tree {
  // NOTE: This validation has been removed to allow dialects with top-level terms.
  // Ideally, we should push the validation into a dialect-specific prettyprinter when #220 is fixed.
  // checkFields(stats.forall(_.isTopLevelStat))
}

package internal.ast {
  // NOTE: Quasi is a base trait for a whole bunch of classes.
  // Every root, branch and ast trait/class among scala.meta trees (except for quasis themselves)
  // has a corresponding quasi, e.g. Term.Quasi or Type.Quasi.
  //
  // Here's how quasis represent unquotes
  // (XXX below depends on the position where the unquote occurs, e.g. q"$x" will result in Term.Quasi):
  //   * $x => XXX.Quasi(0, Term.Name("x"))
  //   * ..$xs => XXX.Quasi(1, XXX.Quasi(0, Term.Name("xs"))
  //   * ...$xss => XXX.Quasi(2, XXX.Quasi(0, Term.Name("xss"))
  //   * ..{$fs($args)} => Complex ellipses aren't supported yet
  @branch trait Quasi extends Tree {
    def rank: Int
    def tree: Tree
    def pt: Class[_]
    def become[T <: Quasi : AstInfo]: T
  }

  // TODO: since trees are no longer sealed, we need a mechanism that would keep track of all of them
  @registry object All
}
