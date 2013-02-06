package controllers

import play.api.i18n.Lang.defaultLang
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.Form
import play.api.data.validation.Constraints._
import play.api.libs.json.Json
import play.api.data.FormError
import views.html.helper.FieldConstructor
import play.api.data.validation.Constraint
import play.api.data.validation.Invalid
import play.api.data.validation.Valid

object Application extends Controller {

  def formWithErrorsFromFlash[A](form: Form[A])(implicit request: Request[_]): Form[A] = {
    val flashErrors = request.flash.get("formErrors")
    val formErrors = {
      flashErrors.map { string =>
        val jsonErrors = Json.parse(string)
        val formErrors = for {
          mapping <- form.mapping.mappings
          errorStrings <- (jsonErrors \ mapping.key).asOpt[Seq[String]]
          if (!errorStrings.isEmpty)
        } yield {
          FormError(mapping.key, errorStrings.head)
        }

        formErrors
      }
    }.getOrElse(Seq[FormError]())
    form.copy(errors = formErrors)
  }

  def formWithDataFromFlash[A](form: Form[A])(implicit request: Request[_]): Form[A] = {
    val flashData = Flash.serialize(request.flash)
    val boundForm = form.bind(flashData)
    boundForm
    //    boundForm.fold(
    //      bad => bad,
    //      good => boundForm
    //    )
  }

  case class SimpleResultWithFormErrors(result: SimpleResult[_]) {
    def flashingFormErrors(badForm: Form[_]) = {
      result.flashing("formErrors" -> badForm.errorsAsJson(defaultLang).toString)
    }
    def flashingFormData(badForm: Form[_]) = {
      result.flashing(badForm.data.toSeq: _*)
    }
  }

  implicit def result2SimpleResultWithFormErrors(result: SimpleResult[_]) =
    SimpleResultWithFormErrors(result)

  def index = Action { implicit request =>
    println(formWithDataFromFlash(form))
    Ok(views.html.index("Your new application is ready.", formWithDataFromFlash(form)))
  }

  def post = Action { implicit request =>
    val filled = form.bindFromRequest()
    filled.fold(
      bad => {
        Redirect(controllers.routes.Application.index).flashingFormData(bad)
      },
      good => Ok(views.html.index("You POSTed well " + good, filled)))

  }

  def form = Form(
    single(
      "name" -> nonEmptyText.verifying(isNameOk)
     ))

  private def isNameOk: Constraint[String] = {
    Constraint { name: String =>
      name.length match {
        case 4 => Invalid("That's a gross 4 letter name")
        case _ => Valid
      }
    }
  }
}

object MyHelpers {

  implicit val myFields = FieldConstructor(views.html.myFieldConstructor.f)

}
