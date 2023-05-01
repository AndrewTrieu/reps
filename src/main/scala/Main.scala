import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Paths}
import scala.util.Try
import scala.io.StdIn.readLine
import scala.io.Source

val API_KEY = Source.fromFile("apiKey.txt").getLines().mkString

object Main {
  // Collect data from the API
  def collectData(
      dataSources: Map[Int, String]
  ): Unit = {
    // Get the current time and truncate it to seconds
    val now = (java.time.LocalDateTime.now)
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)

    // Collect data from the API
    println("Collecting data...")
    for ((k, v) <- dataSources) {
      val url = new URL(
        s"https://api.fingrid.fi/v1/variable/$k/events/csv?start_time=${now
            .minusMonths(3)
            .toString()
            .concat("Z")}&end_time=${now.toString.concat("Z")}"
      )
      // Try to connect to the API endpoint
      Try(url.openConnection().asInstanceOf[HttpURLConnection]) match {
        case util.Success(connection) =>
          connection.setRequestMethod("GET")
          connection.setRequestProperty("Accept", "text/csv")
          connection.setRequestProperty("x-api-key", API_KEY)
          connection.connect()
          // Try to read the response
          Try(
            scala.io.Source.fromInputStream(connection.getInputStream).mkString
          ) match {
            case util.Success(response) =>
              // Write the response to a file
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

  // Read data from a file
  def readData(filePath: String): Unit = {
    println("Reading data...")
    val bufferedSource = io.Source.fromFile(filePath)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      println(s"${cols(0)}\t${cols(1)}\t${cols(2)}")
    }
    bufferedSource.close
  }

  // Modify the energy sources
  def modifySources(filePath: String): Unit = {
    // Read the energy sources
    readData(filePath)

    // Modify the energy sources
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

  // Calculate the mean, median, mode, range and midrange of the data
  def calculateData(filePath: String): Unit = {
    var data = List[Double]()
    val bufferedSource = io.Source.fromFile(filePath)
    for (line <- bufferedSource.getLines.drop(1)) {
      val cols = line.split(",").map(_.trim)
      data = data :+ cols(2).toDouble
    }
    bufferedSource.close

    // Calculate the mean
    val mean = data.sum / data.length
    println(s"Mean: $mean")

    // Calculate the median
    val median = {
      val (lower, upper) = data.sortWith(_ < _).splitAt(data.length / 2)
      if (data.length % 2 == 0) (lower.last + upper.head) / 2.0
      else upper.head
    }
    println(s"Median: $median")

    // Calculate the mode
    val mode = data
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .maxBy(_._2)
      ._1
    println(s"Mode: $mode")

    // Calculate the range
    val range = data.max - data.min
    println(s"Range: $range")

    // Calculate the midrange
    val midrange = (data.max + data.min) / 2
    println(s"Midrange: $midrange")
  }

  // Analyze the data
  def analyzeData(): Unit = {
    print(
      "Enter source to analyze:\n1) Wind\n2) Hydro\n3) Nuclear\n4) All\n5) Exit\n"
    )
    val choice = readLine()
    choice match {
      case "1" =>
        println("Analyzing wind data...")
        calculateData("wind.csv")
      case "2" =>
        println("Analyzing hydro data...")
        calculateData("hydro.csv")
      case "3" =>
        println("Analyzing nuclear data...")
        calculateData("nuclear.csv")
      case "4" =>
        println("Analyzing all data...")
        calculateData("data.csv")
      case "5" =>
        return
      case _ =>
        println("Invalid choice")
    }
  }

  // Main function
  def main(args: Array[String]): Unit = {
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
          analyzeData()
        case "5" =>
          println("Exiting...")
          System.exit(0)
        case _ =>
          println("Invalid choice")
      }
    }
  }
}
