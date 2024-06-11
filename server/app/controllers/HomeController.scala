package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import ru.sanskrit.frontend.Main
import scala.util.{Failure, Success, Try}



@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def runMain() = Action {
    val output: String = Main.main()
    Ok(output)
  }

  def compile() = Action { implicit request: Request[AnyContent] =>
    request.body.asText match {
      case Some(code) =>
        val output: Unit = Main.saveWorkingFiles(code)
        Ok(code)
      case None => 
        BadRequest("No code provided")
    }
  }

  def saveFiles() = Action {
    Ok("saved")
  }

  def getFiles() = Action {
    val output: List[String] = Main.getSavedWorkingFiles()
    Ok(output(0))
  }

  def runParser() = Action {
    val output: String = Main.parse()
    Ok(output)
  }

  def runTypecheck() = Action {
    val output: String = Main.typechecking()
    Ok(output)
  }

  def runHoles() = Action {
    val output: String = Main.holes()
    Ok(output)
  }

  def runDesugar() = Action {
    val output: String = Main.desugaring()
    Ok(output)
  }

  def runBack() = Action {
    val output: String = Main.backending()
    Ok(output)
  }
}
