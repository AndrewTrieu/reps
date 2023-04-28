import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Paths}
import scala.util.Try
import scala.io.StdIn.readLine
import scala.io.Source

val API_KEY = Source.fromFile("apiKey.txt").getLines().mkString

object Main {
  def collectData(
      dataSources: Map[Int, String]
  ): Unit = {
    val now = (java.time.LocalDateTime.now)
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
    val startTime = now.minusMonths(3).toString().concat("Z")
    val endTime = now.toString.concat("Z")

    println("Collecting data...")
    for ((k, v) <- dataSources) {
      val url = new URL(
        s"https://api.fingrid.fi/v1/variable/$k/events/csv?start_time=$startTime&end_time=$endTime"
      )

      Try(url.openConnection().asInstanceOf[HttpURLConnection]) match {
        case util.Success(connection) =>
          connection.setRequestMethod("GET")
          connection.setRequestProperty("Accept", "text/csv")
          connection.setRequestProperty("x-api-key", API_KEY)
          connection.connect()

          Try(
            scala.io.Source.fromInputStream(connection.getInputStream).mkString
          ) match {
            case util.Success(response) =>
              Files.write(Paths.get(v), response.getBytes("UTF-8"))
            case util.Failure(e) =>
              System.err.println(
                s"An error occurred while reading the response: ${e.getMessage}"
              )
          }
        case util.Failure(e) =>
          System.err.println(
            s"An error occurred while connecting to the API endpoint: ${e.getMessage}"
          )
      }
    }
    println("Collection completed")
  }

  def readData(filePath: String): Unit = {
    println("Reading data...")
    val bufferedSource = io.Source.fromFile(filePath)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      println(s"${cols(0)}\t${cols(1)}\t${cols(2)}")
    }
    bufferedSource.close
  }

  def modifySources(filePath: String): Unit = {
    readData(filePath)

    print("Enter your choice:\n1) Modify\n2) Exit\n")
    val choice = readLine()
    choice match {
      case "1" =>
        print(
          "Choose the source to modify:\n1) Wind\n2) Hydro\n3) Nuclear\n4) Exit\n"
        )
        val choice2 = readLine()
        print("Enter the new value: ")
        val newValue = readLine()
        val lines = Source.fromFile(filePath).getLines.toList
        val temp = lines(1).split(",")
        val pw = new java.io.PrintWriter(filePath)
        choice2 match {
          case "1" =>
            val newLines = lines.updated(1, s"$newValue,${temp(1)},${temp(2)}")
            newLines.foreach(pw.println)
          case "2" =>
            val newLines = lines.updated(1, s"${temp(0)},$newValue,${temp(2)}")
            newLines.foreach(pw.println)
          case "3" =>
            val newLines = lines.updated(1, s"${temp(0)},${temp(1)},$newValue")
            newLines.foreach(pw.println)
          case "4" =>
            return
          case _ =>
            println("Invalid choice")
        }
        pw.close()
        println("Value updated")
      case "2" =>
        return
      case _ =>
        println("Invalid choice")
    }
  }

  def main(args: Array[String]): Unit = {
    val now = (java.time.LocalDateTime.now)
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
    while (true) {
      print(
        "REPS management system:\n1) Check energy sources\n2) Collect data\n3) View data\n4) Analyze data\n5) Exit\nEnter your choice: "
      )
      val choice = readLine()

      choice match {
        case "1" =>
          modifySources("sources.csv")
        case "2" =>
          collectData(
            Map(
              188 -> "nuclear.csv",
              191 -> "hydro.csv",
              181 -> "wind.csv",
              192 -> "data.csv"
            )
          )
        case "3" =>
          readData("data.csv")
        case "4" =>
          println("Analyzing data...")
        case "5" =>
          println("Exiting...")
          System.exit(0)
        case _ =>
          println("Invalid choice")
      }
    }
  }
}
