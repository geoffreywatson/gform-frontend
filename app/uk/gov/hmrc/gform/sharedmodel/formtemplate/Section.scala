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

import play.api.libs.json._
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormField

import scala.collection.immutable.List

sealed trait BaseSection {
  def title: String
  def shortName: Option[String]
  def fields: List[FormComponent]
}

case class Section(
  title: String,
  description: Option[String],
  shortName: Option[String],
  progressIndicator: Option[String] = None,
  includeIf: Option[IncludeIf],
  repeatsMax: Option[TextExpression],
  repeatsMin: Option[TextExpression],
  validators: Option[SectionValidator], //TODO List instead of Option
  fields: List[FormComponent]
) extends BaseSection

object Section {
  implicit val format = Json.format[Section]
}

case class DeclarationSection(
  title: String,
  description: Option[String],
  shortName: Option[String],
  fields: List[FormComponent]
) extends BaseSection

object DeclarationSection {
  implicit val format = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: String,
  description: Option[String],
  shortName: Option[String],
  fields: List[FormComponent]
) extends BaseSection

object AcknowledgementSection {
  implicit val format = Json.format[AcknowledgementSection]
}

case class EnrolmentSection(
  title: String,
  shortName: Option[String],
  fields: List[FormComponent]
) extends BaseSection

object EnrolmentSection {
  implicit val format = Json.format[EnrolmentSection]
}

case class SectionFormField(
  title: String,
  fields: List[(List[FormField], FormComponent)]
)
