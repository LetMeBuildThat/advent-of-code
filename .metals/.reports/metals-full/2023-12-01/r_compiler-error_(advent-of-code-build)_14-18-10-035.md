jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.12.18/scala-library-2.12.18-sources.jar!/scala/collection/generic/GenericCompanion.scala
### jar%3Afile%3A%2F%2F%2FUsers%2Fewanhardingham%2FLibrary%2FCaches%2FCoursier%2Fv1%2Fhttps%2Frepo1.maven.org%2Fmaven2%2Forg%2Fscala-lang%2Fscala-library%2F2.12.18%2Fscala-library-2.12.18-sources.jar%21%2Fscala%2Fcollection%2Fgeneric%2FGenericCompanion.scala:20: error: ; expected but package found
package collection
^

occurred in the presentation compiler.

action parameters:
uri: jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.12.18/scala-library-2.12.18-sources.jar!/scala/collection/generic/GenericCompanion.scala
text:
```scala
import _root_.scala.xml.{TopScope=>$scope}
import _root_.sbt._
import _root_.sbt.Keys._
import _root_.sbt.nio.Keys._
import _root_.sbt.ScriptedPlugin.autoImport._, _root_.sbt.plugins.JUnitXmlReportPlugin.autoImport._, _root_.sbt.plugins.MiniDependencyTreePlugin.autoImport._, _root_.bloop.integrations.sbt.BloopPlugin.autoImport._
import _root_.sbt.plugins.IvyPlugin, _root_.sbt.plugins.JvmPlugin, _root_.sbt.plugins.CorePlugin, _root_.sbt.ScriptedPlugin, _root_.sbt.plugins.SbtPlugin, _root_.sbt.plugins.SemanticdbPlugin, _root_.sbt.plugins.JUnitXmlReportPlugin, _root_.sbt.plugins.Giter8TemplatePlugin, _root_.sbt.plugins.MiniDependencyTreePlugin, _root_.bloop.integrations.sbt.BloopPlugin
/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package generic

import mutable.Builder
import scala.language.higherKinds

/** A template class for companion objects of "regular" collection classes
 *  represent an unconstrained higher-kinded type. Typically
 *  such classes inherit from trait `GenericTraversableTemplate`.
 *  @tparam  CC   The type constructor representing the collection class.
 *  @see [[scala.collection.generic.GenericTraversableTemplate]]
 *  @author Martin Odersky
 *  @since 2.8
 *  @define coll  collection
 *  @define Coll  `CC`
 */
abstract class GenericCompanion[+CC[X] <: GenTraversable[X]] {
  /** The underlying collection type with unknown element type */
  protected[this] type Coll = CC[_]

  /** The default builder for `$Coll` objects.
   *  @tparam A      the type of the ${coll}'s elements
   */
  def newBuilder[A]: Builder[A, CC[A]]

  /** An empty collection of type `$Coll[A]`
   *  @tparam A      the type of the ${coll}'s elements
   */
  def empty[A]: CC[A] = {
    if ((this eq immutable.Seq) || (this eq collection.Seq)) Nil.asInstanceOf[CC[A]]
    else newBuilder[A].result()
  }

  /** Creates a $coll with the specified elements.
   *  @tparam A      the type of the ${coll}'s elements
   *  @param elems  the elements of the created $coll
   *  @return a new $coll with elements `elems`
   */
  def apply[A](elems: A*): CC[A] = {
    if (elems.isEmpty) empty[A]
    else {
      val b = newBuilder[A]
      b ++= elems
      b.result()
    }
  }
}

```



#### Error stacktrace:

```
scala.meta.internal.parsers.Reporter.syntaxError(Reporter.scala:16)
	scala.meta.internal.parsers.Reporter.syntaxError$(Reporter.scala:16)
	scala.meta.internal.parsers.Reporter$$anon$1.syntaxError(Reporter.scala:22)
	scala.meta.internal.parsers.Reporter.syntaxError(Reporter.scala:17)
	scala.meta.internal.parsers.Reporter.syntaxError$(Reporter.scala:17)
	scala.meta.internal.parsers.Reporter$$anon$1.syntaxError(Reporter.scala:22)
	scala.meta.internal.parsers.ScalametaParser.syntaxErrorExpected(ScalametaParser.scala:421)
	scala.meta.internal.parsers.ScalametaParser.expect(ScalametaParser.scala:423)
	scala.meta.internal.parsers.ScalametaParser.accept(ScalametaParser.scala:427)
	scala.meta.internal.parsers.ScalametaParser.acceptStatSep(ScalametaParser.scala:447)
	scala.meta.internal.parsers.ScalametaParser.acceptStatSepOpt(ScalametaParser.scala:451)
	scala.meta.internal.parsers.ScalametaParser.statSeqBuf(ScalametaParser.scala:4462)
	scala.meta.internal.parsers.ScalametaParser.$anonfun$batchSource$13(ScalametaParser.scala:4696)
	scala.Option.getOrElse(Option.scala:189)
	scala.meta.internal.parsers.ScalametaParser.$anonfun$batchSource$1(ScalametaParser.scala:4696)
	scala.meta.internal.parsers.ScalametaParser.atPos(ScalametaParser.scala:319)
	scala.meta.internal.parsers.ScalametaParser.autoPos(ScalametaParser.scala:365)
	scala.meta.internal.parsers.ScalametaParser.batchSource(ScalametaParser.scala:4652)
	scala.meta.internal.parsers.ScalametaParser.$anonfun$source$1(ScalametaParser.scala:4645)
	scala.meta.internal.parsers.ScalametaParser.atPos(ScalametaParser.scala:319)
	scala.meta.internal.parsers.ScalametaParser.autoPos(ScalametaParser.scala:365)
	scala.meta.internal.parsers.ScalametaParser.source(ScalametaParser.scala:4645)
	scala.meta.internal.parsers.ScalametaParser.entrypointSource(ScalametaParser.scala:4650)
	scala.meta.internal.parsers.ScalametaParser.parseSourceImpl(ScalametaParser.scala:135)
	scala.meta.internal.parsers.ScalametaParser.$anonfun$parseSource$1(ScalametaParser.scala:132)
	scala.meta.internal.parsers.ScalametaParser.parseRuleAfterBOF(ScalametaParser.scala:59)
	scala.meta.internal.parsers.ScalametaParser.parseRule(ScalametaParser.scala:54)
	scala.meta.internal.parsers.ScalametaParser.parseSource(ScalametaParser.scala:132)
	scala.meta.parsers.Parse$.$anonfun$parseSource$1(Parse.scala:29)
	scala.meta.parsers.Parse$$anon$1.apply(Parse.scala:36)
	scala.meta.parsers.Api$XtensionParseDialectInput.parse(Api.scala:25)
	scala.meta.internal.semanticdb.scalac.ParseOps$XtensionCompilationUnitSource.toSource(ParseOps.scala:17)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument.toTextDocument(TextDocumentOps.scala:206)
	scala.meta.internal.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:54)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$semanticdbTextDocument$1(ScalaPresentationCompiler.scala:356)
```
#### Short summary: 

jar%3Afile%3A%2F%2F%2FUsers%2Fewanhardingham%2FLibrary%2FCaches%2FCoursier%2Fv1%2Fhttps%2Frepo1.maven.org%2Fmaven2%2Forg%2Fscala-lang%2Fscala-library%2F2.12.18%2Fscala-library-2.12.18-sources.jar%21%2Fscala%2Fcollection%2Fgeneric%2FGenericCompanion.scala:20: error: ; expected but package found
package collection
^