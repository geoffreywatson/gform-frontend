/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers

sealed trait BooleanExpr
final case class Equals(left: Expr, right: Expr) extends BooleanExpr
final case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case object IsTrue extends BooleanExpr
final case object IsFalse extends BooleanExpr

object BooleanExpr {
  implicit val format: OFormat[BooleanExpr] = derived.oformat

  def isTrue(expr: BooleanExpr, data: Map[FormComponentId, Seq[String]], retrievals: Retrievals): Boolean = {
    expr match {
      case Equals(FormCtx(fieldId), Constant(value)) if FormDataHelpers.get(data, FormComponentId(fieldId)).contains(value) => true
      case Equals(UserCtx(_), Constant(value)) if retrievals.affinityGroupName == value => true
      case Or(expr1, expr2) => isTrue(expr1, data, retrievals) | isTrue(expr2, data, retrievals)
      case And(expr1, expr2) => isTrue(expr1, data, retrievals) & isTrue(expr2, data, retrievals)
      case IsTrue => true
      case _ => false
    }
  }
}

sealed trait Comparison
final case object Equality extends Comparison

sealed trait BooleanOperation
final case object OrOperation extends BooleanOperation
final case object AndOperation extends BooleanOperation

