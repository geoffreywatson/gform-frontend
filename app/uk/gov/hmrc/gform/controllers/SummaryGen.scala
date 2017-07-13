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

package uk.gov.hmrc.gform.controllers

import javax.inject.{ Inject, Singleton }

import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.libs.json.Json
import play.api.mvc.AnyContent
import play.twirl.api.Html
import uk.gov.hmrc.gform.connectors.IsEncrypt
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadModule }
import uk.gov.hmrc.gform.gformbackend.model.{ EnvelopeId, FormId, FormTypeId, Version }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components.FieldId
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SaveService }
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class SummaryGen @Inject() (val messagesApi: MessagesApi, val sec: SecuredActions, repeatService: RepeatingComponentService, fileUploadModule: FileUploadModule, authConnector: AuthConnector)(implicit ec: ExecutionContext)
    extends FrontendController with I18nSupport {
  import GformSession._

  def summaryById(formTypeId: FormTypeId, version: Version, formId: FormId) =
    sec.SecureWithTemplateAsync(formTypeId, version) { authContext => implicit request =>
      val envelopeId = request.session.getEnvelopeId.get
      val envelope = fileUploadService.getEnvelope(envelopeId)
      for {
        envelope <- envelope
        formData <- SaveService.getFormById(formTypeId, version, formId)
      } yield Summary(request.formTemplate)
        .renderSummary(formDataMap(formData.formData), formId, repeatService, envelope)
    }

  def summaryByIdCache(formTypeId: FormTypeId, version: Version, userId: UserId) =
    sec.SecureWithTemplateAsync(formTypeId, version) { authContext => implicit request =>
      val envelopeId = request.session.getEnvelopeId.get
      val envelope = fileUploadService.getEnvelope(envelopeId)
      for {
        envelope <- envelope
        formData <- SaveService.getFormByIdCache(formTypeId, version, userId)
      } yield Summary(request.formTemplate)
        .renderSummary(formDataMap(formData.formData), formData._id, repeatService, envelope)
    }

  def submit(formTypeId: FormTypeId, version: Version) = sec.SecureWithTemplateAsync(formTypeId, version) { authContext => implicit request =>
    processResponseDataFromBody(request) { data =>
      get(data, FieldId("save")) match {
        case "Exit" :: Nil =>
          Future.successful(Ok)
        case "Continue" :: Nil =>
          anyFormId(data) match {
            case Some(formId) =>
              getHtmlForPDFGeneration(formId, version, formTypeId).flatMap { html =>
                if (IsEncrypt.is) {
                  authConnector.getUserDetails[UserId](authContext).flatMap { x =>
                    SaveService.sendSubmission(formTypeId, x, version, html).
                      map(r => Ok(Json.obj("envelope" -> r.body, "formId" -> Json.toJson(formId))))
                  }
                } else {
                  SaveService.sendSubmission(formTypeId, formId, html).
                    map(r => Ok(Json.obj("envelope" -> r.body, "formId" -> Json.toJson(formId))))
                }
              }
            case None =>
              Future.successful(BadRequest("No formId"))
          }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private def getHtmlForPDFGeneration(formId: FormId, version: Version, formTypeId: FormTypeId)
                                     (implicit request: RequestWithTemplate[AnyContent]) = {
    val summary = Summary(request.formTemplate)
    val envelopeId = request.session.getEnvelopeId.get
    for {
      form <- SaveService.getFormById(formTypeId, version, formId)
      envelop <- fileUploadService.getEnvelope(envelopeId)
    } yield uk.gov.hmrc.gform.views.html.summary_pdf(
      request.formTemplate,
      summary.summaryForRender(formDataMap(form.formData), formId, repeatService, envelop), formId
    )
  }

  private lazy val fileUploadService = fileUploadModule.fileUploadService

}
