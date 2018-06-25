package scala.meta.internal.scalacp

import java.util.{HashMap, HashSet}
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.{Names => n}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.tools.scalap.scalax.rules.scalasig._

trait SymbolOps { self: Scalacp =>
  lazy val symbolCache = new HashMap[Symbol, String]
  implicit class XtensionSymbolExternalSymbol(sym: ExternalSymbol) {
    def isJavaDefined: Boolean = {
      sym.path.startsWith("java")
    }
    def isExternalJavaStaticInnerClass: Boolean = {
      def loop(s: Symbol): Unit = s match {
        case e: ExternalSymbol =>
          val hasTermName = {
            val idx = e.entry.index + 1
            if (e.entry.scalaSig.hasEntry(idx)) {
              val nameEntryType = e.entry.scalaSig.table(idx)._1
              nameEntryType == 1
            } else {
              false
            }
          }
          pprint.log(e.name)
          pprint.log(hasTermName)
          s.parent.foreach(loop)
        case _ =>
      }
      loop(sym)
      sym.entry.entryType == 10 && {
        val parent = sym.parent.get
//        parent
        true
      }
    }
  }
  implicit class XtensionSymbolSSymbol(sym: Symbol) {
    def toSemantic: String = {
      def uncached(sym: Symbol): String = {
        if (sym.isSemanticdbGlobal) Symbols.Global(sym.owner, sym.descriptor)
        else freshSymbol()
      }
      val ssym = symbolCache.get(sym)
      if (ssym != null) {
        ssym
      } else {
        val ssym = uncached(sym)
        symbolCache.put(sym, ssym)
        ssym
      }
    }
  }

  implicit class XtensionSymbolSSpec(sym: Symbol) {
    def isSemanticdbGlobal: Boolean = !isSemanticdbLocal
    def isSemanticdbLocal: Boolean = {
      val owner = sym.parent.getOrElse(NoSymbol)
      def definitelyGlobal = sym.isPackage
      def definitelyLocal =
        sym == NoSymbol ||
          (owner.isInstanceOf[MethodSymbol] && !sym.isParam) ||
          ((owner.isAlias || (owner.isType && owner.isDeferred)) && !sym.isParam) ||
          // NOTE: Scalap doesn't expose locals.
          // sym.isSelfParameter ||
          // sym.isLocalDummy ||
          sym.isRefinementClass ||
          sym.isAnonymousClass ||
          sym.isAnonymousFunction ||
          (sym.isInstanceOf[TypeSymbol] && sym.isExistential)
      def ownerLocal = sym.parent.map(_.isSemanticdbLocal).getOrElse(false)
      !definitelyGlobal && (definitelyLocal || ownerLocal)
    }
    def owner: String = {
      if (sym.isRootPackage) Symbols.None
      else if (sym.isEmptyPackage) Symbols.RootPackage
      else if (sym.isToplevelPackage) Symbols.RootPackage
      else {
        sym.parent match {
          case Some(NoSymbol) => ""
          case Some(parent) => parent.ssym
          case None => sys.error(s"unsupported symbol $sym")
        }
      }
    }
    def descriptor: Descriptor = {
      val name = sym.name.toSemantic
      sym.kind match {
        case k.LOCAL | k.OBJECT | k.PACKAGE | k.PACKAGE_OBJECT =>
          d.Term(name)
        case k.METHOD | k.CONSTRUCTOR | k.MACRO =>
          val overloads = {
            val peers = sym.parent.get.semanticdbDecls.syms
            peers.filter {
              case peer: ObjectSymbol => peer.name == sym.name
              case peer: MethodSymbol => peer.name == sym.name
              case _ => false
            }
          }
          val disambiguator = {
            if (overloads.lengthCompare(1) == 0) "()"
            else {
              val index = overloads.indexOf(sym)
              if (index <= 0) "()"
              else s"(+${index})"
            }
          }
          d.Method(name, disambiguator)
        case k.TYPE | k.CLASS | k.TRAIT =>
          d.Type(name)
        case k.PARAMETER =>
          d.Parameter(name)
        case k.TYPE_PARAMETER =>
          d.TypeParameter(name)
        case skind =>
          sys.error(s"unsupported kind $skind for symbol $sym")
      }
    }
    def semanticdbDecls: SemanticdbDecls = {
      val decls = sym.children.filter(decl => decl.isUseful && !decl.isTypeParam)
      SemanticdbDecls(decls.toList)
    }
  }

  implicit class XtensionSymbolsSSpec(syms: Seq[Symbol]) {
    def semanticdbDecls: SemanticdbDecls = {
      SemanticdbDecls(syms.filter(_.isUseful))
    }

    def sscope(linkMode: LinkMode): s.Scope = {
      linkMode match {
        case SymlinkChildren =>
          s.Scope(symlinks = syms.map(_.ssym))
        case HardlinkChildren =>
          syms.map(registerHardlink)
          s.Scope(hardlinks = syms.map(_.toSymbolInformation(HardlinkChildren)))
      }
    }
  }

  case class SemanticdbDecls(syms: Seq[Symbol]) {
    def sscope(linkMode: LinkMode): s.Scope = {
      linkMode match {
        case SymlinkChildren =>
          val sbuf = List.newBuilder[String]
          syms.foreach { sym =>
            val ssym = sym.ssym
            sbuf += ssym
            if (sym.isUsefulField && sym.isMutable) {
              val setterName = ssym.desc.name + "_="
              val setterSym = Symbols.Global(ssym.owner, d.Method(setterName, "()"))
              sbuf += setterSym
            }
          }
          s.Scope(sbuf.result)
        case HardlinkChildren =>
          val sbuf = List.newBuilder[s.SymbolInformation]
          syms.foreach { sym =>
            registerHardlink(sym)
            val sinfo = sym.toSymbolInformation(HardlinkChildren)
            sbuf += sinfo
            if (sym.isUsefulField && sym.isMutable) {
              Synthetics.setterInfos(sinfo, HardlinkChildren).foreach(sbuf.+=)
            }
          }
          s.Scope(hardlinks = sbuf.result)
      }
    }
  }

  implicit class XtensionSymbol(sym: Symbol) {
    def ssym: String = sym.toSemantic
    def self: Type = sym match {
      case sym: ClassSymbol =>
        sym.selfType
          .map {
            case RefinedType(_, List(_, self)) => self
            case _ => NoType
          }
          .getOrElse(NoType)
      case _ =>
        NoType
    }
    def isRootPackage: Boolean = sym.path == "<root>"
    def isEmptyPackage: Boolean = sym.path == "<empty>"
    def isToplevelPackage: Boolean = sym.parent.isEmpty
    def isModuleClass: Boolean = sym.isInstanceOf[ClassSymbol] && sym.isModule
    def moduleClass: Symbol = sym match {
      case sym: SymbolInfoSymbol if sym.isModule =>
        sym.infoType match {
          case TypeRefType(_, moduleClass, _) => moduleClass
          case _ => NoSymbol
        }
      case _ =>
        NoSymbol
    }
    def isClass: Boolean = sym.isInstanceOf[ClassSymbol] && !sym.isModule
    def isObject: Boolean = sym.isInstanceOf[ObjectSymbol]
    def isType: Boolean = sym.isInstanceOf[TypeSymbol]
    def isAlias: Boolean = sym.isInstanceOf[AliasSymbol]
    def isMacro: Boolean = sym.isMethod && sym.hasFlag(0x00008000)
    def isConstructor: Boolean = sym.isMethod && (sym.name == "<init>" || sym.name == "$init$")
    def isPackageObject: Boolean = sym.name == "package"
    def isTypeParam = sym.isType && sym.isParam
    def isAnonymousClass = sym.name.contains("$anon")
    def isAnonymousFunction = sym.name.contains("$anonfun")
    def isSyntheticConstructor = sym match {
      case sym: SymbolInfoSymbol =>
        val owner = sym.symbolInfo.owner
        sym.isConstructor && (owner.isModuleClass || owner.isTrait)
      case _ =>
        false
    }
    def isLocalChild: Boolean = sym.name == "<local child>"
    def isExtensionMethod: Boolean = sym.name.contains("$extension")
    def isSyntheticValueClassCompanion: Boolean = {
      sym match {
        case sym: SymbolInfoSymbol =>
          if (sym.isModuleClass) {
            sym.infoType match {
              case ClassInfoType(_, List(TypeRefType(_, anyRef, _))) =>
                sym.isSynthetic && sym.semanticdbDecls.syms.isEmpty
              case _ =>
                false
            }
          } else if (sym.isModule) {
            sym.moduleClass.isSyntheticValueClassCompanion
          } else {
            false
          }
        case _ =>
          false
      }
    }
    def isScalacField: Boolean = {
      val isField = sym.isInstanceOf[MethodSymbol] && !sym.isMethod && !sym.isParam
      val isJavaDefined = sym.isJava
      isField && !isJavaDefined
    }
    def isUselessField: Boolean = {
      val peers = sym.parent.map(_.children.toList).getOrElse(Nil)
      val getter = peers.find(m => m.isAccessor && m.name == sym.name.stripSuffix(" "))
      sym.isScalacField && getter.nonEmpty
    }
    def isUsefulField: Boolean = {
      sym.isScalacField && !sym.isUselessField
    }
    def isSyntheticCaseAccessor: Boolean = {
      sym.isCaseAccessor && sym.name.contains("$")
    }
    def isRefinementClass: Boolean = {
      sym.name == "<refinement>"
    }
    def isUseless: Boolean = {
      sym == NoSymbol ||
      sym.isAnonymousClass ||
      sym.isSyntheticConstructor ||
      sym.isModuleClass ||
      sym.isLocalChild ||
      sym.isExtensionMethod ||
      sym.isSyntheticValueClassCompanion ||
      sym.isUselessField ||
      sym.isSyntheticCaseAccessor ||
      sym.isRefinementClass
    }
    def isUseful: Boolean = !sym.isUseless
  }

  private var nextId = 0
  private def freshSymbol(): String = {
    val result = Symbols.Local(nextId)
    nextId += 1
    result
  }

  lazy val hardlinks = new HashSet[String]
  private def registerHardlink(sym: Symbol): Unit = {
    hardlinks.add(sym.ssym)
  }
}
