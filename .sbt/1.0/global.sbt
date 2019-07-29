scalacOptions += "-Xlog-implicits"

// libraryDependencies += "org.scala-debugger" %% "scala-debugger-api" % "1.1.0-M3"

shellPrompt := { state =>
  import scala.sys.process.Process
  def textColor(color: Int)      = { s"\033[38;5;${color}m" }
  def backgroundColor(color:Int) = { s"\033[48;5;${color}m" }
  def reset                      = { s"\033[0m" }
  def formatText(str: String)(txtColor: Int, backColor: Int) = {
    s"${textColor(txtColor)}${backgroundColor(backColor)}${str}${reset}"
  }
  def gitBranch = Process("git rev-parse --abbrev-ref HEAD").lines.head
  val red    = 1
  val green  = 2
  val yellow = 11
  val white  = 15
  val black  = 16
  val orange = 166

  formatText(s"${Project.extract(state).currentProject.id}")(white, black) +
  formatText(s" [${gitBranch}]")(green, black) +
  formatText(" >> ")(orange, black)
}
