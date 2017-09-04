/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform
package controllers

import javax.inject.Inject

import cats._
import cats.data.{ EitherT, Validated }
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.mvc._
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadModule }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.prepop.PrepopModule
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SectionRenderingService }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FormController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    configModule: ConfigModule,
    repeatService: RepeatingComponentService,
    fileUploadModule: FileUploadModule,
    authModule: AuthModule,
    validationModule: ValidationModule,
    prePopModule: PrepopModule,
    renderer: SectionRenderingService
) extends FrontendController {

  import controllersModule.i18nSupport._

  def newForm(formTemplateId: FormTemplateId, welsh: Option[String]) = authentication.async(formTemplateId) { implicit request => cache =>
    result(cache.formTemplate, UserId(cache.retrievals.userDetails.groupIdentifier), welsh)
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(formTemplateId: FormTemplateId, userId: UserId)(implicit hc: HeaderCarrier): Future[(Form, Boolean)] = {
    val formId = FormId(userId, formTemplateId)

    def startForm: Future[Form] = for {
      formId <- gformConnector.newForm(formTemplateId, userId)
      form <- gformConnector.getForm(formId)
    } yield form

    for {
      maybeForm <- gformConnector.maybeForm(formId)
      form <- maybeForm.map(Future.successful).getOrElse(startForm)
    } yield (form, maybeForm.isDefined)
  }

  private def result(formTemplate: FormTemplate, userId: UserId, welsh: Option[String])(implicit hc: HeaderCarrier, request: Request[_]) = {
    for {
      (form, wasFormFound) <- getOrStartForm(formTemplate._id, userId)
    } yield {
      if (wasFormFound) {
        Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.continue_form_page(formTemplate._id, form._id, welsh))
      } else {
        Redirect(routes.FormController.form(form._id, formTemplate._id, SectionNumber.firstSection, formTemplate.sections.size, welsh))
      }
    }
  }

  def form(formId: FormId, formTemplateId: FormTemplateId, sectionNumber: SectionNumber, totalSections: Int, welsh: Option[String]) = authentication.async(formId) { implicit request => cache =>
    val fieldData = getFormData(cache.form)

    for {// format: OFF
      _               <- repeatService.loadData(cache.form.repeatingGroupStructure)
      envelopeF       =  fileUploadService.getEnvelope(cache.form.envelopeId)
      envelope        <- envelopeF
      dynamicSections <- repeatService.getAllSections(cache.formTemplate, fieldData)
      html            <- renderer.renderSection(formId, sectionNumber, fieldData, cache.formTemplate, None, envelope, cache.form.envelopeId, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, welsh)
      // format: ON
    } yield Ok(html)
  }

  def formError(formId: FormId, formTemplateId: FormTemplateId, sectionNumber: SectionNumber, totalPage: Int, welsh: Option[String]) = authentication.async(formId) { implicit request => cache =>

    val fieldData = getFormData(cache.form)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, fieldData)

    for {// format: OFF
      envelope        <- envelopeF
      sections        <- sectionsF
      errors          <- getFormFieldValidationResults(sections, sectionNumber, fieldData, envelope, cache.form.envelopeId)
      html            <- renderer.renderSection(formId, sectionNumber, fieldData, cache.formTemplate, Some(errors.get), envelope, cache.form.envelopeId, sections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, welsh)
      // format: ON
    } yield Ok(html)
  }

  def fileUploadPage(formId: FormId, formTemplateId: FormTemplateId, sectionNumber: SectionNumber, fId: String, totalSection: Int, welsh: Option[String]) = authentication.async(formId) { implicit request => cache =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, formTemplateId, sectionNumber, totalSection, welsh)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, formTemplateId, sectionNumber, totalSection, welsh)

    def actionUrl(envelopeId: EnvelopeId) = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${`redirect-success-url`}&redirect-error-url=${`redirect-error-url`}"

    Future.successful(Ok(
      uk.gov.hmrc.gform.views.html.file_upload_page(formId, sectionNumber, fileId, cache.formTemplate, actionUrl(cache.form.envelopeId), totalSection, welsh)
    ))
  }

  private def getFormData(form: Form): Map[FieldId, List[String]] = form.formData.fields.map(fd => fd.id -> List(fd.value)).toMap

  val choice = play.api.data.Form(play.api.data.Forms.single(
    "decision" -> play.api.data.Forms.nonEmptyText
  ))

  def decision(formTemplateId: FormTemplateId, formId: FormId, welsh: Option[String]): Action[AnyContent] = authentication.async(formId) { implicit request => cache =>
    choice.bindFromRequest.fold(
      _ => Future.successful(BadRequest(uk.gov.hmrc.gform.views.html.hardcoded.pages.continue_form_page(formTemplateId, formId, welsh))),
      {
        case "continue" => Future.successful(Redirect(routes.FormController.form(formId, formTemplateId, firstSection, cache.formTemplate.sections.size, welsh))) //TODO get dyanmic sections in here ???
        case "delete" => Future.successful(Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.confirm_delete(formTemplateId, formId, welsh)))
        case _ => Future.successful(Redirect(routes.FormController.newForm(formTemplateId, welsh)))
      }
    )
  }

  def delete(formTemplateId: FormTemplateId, formId: FormId, welsh: Option[String]): Action[AnyContent] = authentication.async(formTemplateId) { implicit request => cache =>
    gformConnector.deleteForm(formId).map { x =>
      Redirect(routes.FormController.newForm(formTemplateId, welsh))
    }
  }

  def updateFormData(formId: FormId, sectionNumber: SectionNumber, welsh: Option[String]) = authentication.async(formId) { implicit request => cache =>

    val envelopeF = for {
      envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
    } yield envelope

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

      val formFieldValidationResultsF: Future[Map[FieldValue, FormFieldValidationResult]] = for {
        sections <- sectionsF
        envelope <- envelopeF
        ffvr <- getFormFieldValidationResults(sections, sectionNumber, data, envelope, cache.form.envelopeId)
      } yield ffvr

      val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(!_.values.view.exists(!_.isOk))
      val fieldsF: Future[Seq[FormField]] = formFieldValidationResultsF.map(_.values.toSeq.flatMap(_.toFormField))
      val formDataF: Future[FormData] = fieldsF.map(FormData(_))

      def processSaveAndContinue(userId: UserId, form: Form, nextPage: Result)(implicit hc: HeaderCarrier): Future[Result] =
        for {
          formData <- formDataF
          keystore <- repeatService.getData()
          section <- sectionsF
          userData = UserData(formData, keystore)
          _ <- gformConnector.updateUserData(formId, userData)
          isFormValid <- isFormValidF
        } yield if (isFormValid) nextPage else Redirect(uk.gov.hmrc.gform.controllers.routes.FormController.formError(formId, cache.formTemplate._id, sectionNumber, section.size, welsh))

      def processSaveAndExit(userId: UserId, form: Form, envelopeId: EnvelopeId): Future[Result] = {

        for {
          section <- sectionsF
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore)

          result <- gformConnector.updateUserData(formId, userData).map(response => Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.save_acknowledgement(formId, form.formTemplateId, section.size, welsh)))
        } yield result
      }

      def processBack(userId: UserId, form: Form)(continue: Future[Result]): Future[Result] = {

        for {
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore)
          result <- gformConnector.updateUserData(formId, userData).flatMap(response => continue)
        } yield result

      }

      def processAddGroup(groupId: String): Future[Result] = for {
        _ <- repeatService.appendNewGroup(groupId)
        envelope <- envelopeF
        dynamicSections <- sectionsF
        html <- renderer.renderSection(formId, sectionNumber, data, cache.formTemplate, None, envelope, cache.form.envelopeId, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, welsh)
      } yield Ok(html)

      def processRemoveGroup(groupId: String): Future[Result] = for {
        _ <- repeatService.removeGroup(groupId, data)
        envelope <- envelopeF
        dynamicSections <- sectionsF
        html <- renderer.renderSection(formId, sectionNumber, data, cache.formTemplate, None, envelope, cache.form.envelopeId, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, welsh)
      } yield Ok(html)

      val userId = UserId(cache.retrievals.userDetails.groupIdentifier)
      val navigationF: Future[Direction] = sectionsF.map(sections => new Navigator(sectionNumber, sections, data).navigate)

      def redirection(call: Int => Call): Future[Result] = {
        for {
          section <- sectionsF
        } yield Redirect(call(section.size))
      }

      navigationF.flatMap {
        // format: OFF
        case SaveAndContinue(sn)            => redirection(uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, cache.formTemplate._id, sn, _, welsh)).flatMap(x => processSaveAndContinue(userId, cache.form, x))
        case SaveAndExit                    => processSaveAndExit(userId, cache.form, cache.form.envelopeId)
        case Back(sn)                       => processBack(userId, cache.form)(redirection(uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, cache.formTemplate._id, sn, _, welsh)))
        case SaveAndSummary                 => processSaveAndContinue(userId, cache.form, Redirect(routes.SummaryGen.summaryById(formId, cache.formTemplate._id, welsh)))
        case AddGroup(groupId)              => processAddGroup(groupId)
        case RemoveGroup(groupId)           => processRemoveGroup(groupId)
        // format: ON
      }

    }
  }

  private def getFormFieldValidationResults(sections: List[Section], sectionNumber: SectionNumber, data: Map[FieldId, Seq[String]], envelope: Envelope, envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Map[FieldValue, FormFieldValidationResult]] = {
    val section = sections(sectionNumber.value)
    val sectionFields: List[FieldValue] = repeatService.atomicFields(section)
    val allFieldsInTemplate: List[FieldValue] = sections.flatMap(repeatService.atomicFields)

    //Leave it's lazy, we don't want to spawn this computation if we got validation other validation errors
    lazy val validatorsValidationResultF: Future[ValidatedType] =
      sectionFields
        .map(fv => validationService.validateComponents(fv, data, envelopeId))
        .sequenceU
        .map(Monoid[ValidatedType].combineAll)

    val sectionValidationResultF: Future[ValidatedType] =
      section
        .validators
        .map(validationService.validateUsingSectionValidators(_, data))
        .getOrElse(().valid.pure[Future])

    val validationResultF: Future[ValidatedType] = {
      val eT = for {
        _ <- EitherT(sectionValidationResultF.map(_.toEither))
        _ <- EitherT(validatorsValidationResultF.map(_.toEither))
      } yield ()
      eT.value.map(Validated.fromEither)
    }

    val formFieldValidationResultsF: Future[Map[FieldValue, FormFieldValidationResult]] =
      validationResultF.map { validated =>
        ValidationUtil.evaluateValidationResult(allFieldsInTemplate, validated, data, envelope)
          .fold(identity, identity)
          .map(v => ValidationUtil.extractedFieldValue(v) -> v)
          .toMap
      }

    formFieldValidationResultsF
  }

  private lazy val authentication = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val firstSection = SectionNumber(0)
  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val validationService = validationModule.validationService
  private lazy val appConfig = configModule.appConfig
  private lazy val formMaxAttachmentSizeMB = configModule.appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = configModule.appConfig.contentTypes
}
