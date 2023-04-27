import java.io._
import java.net._
import scala.io.StdIn.readLine
import scala.io.Source

val API_KEY = Source.fromFile("apiKey.txt").getLines().mkString

def collectData(
    variableId: Int,
    startTime: String,
    endTime: String,
    filePath: String
): Unit = {

  try {
    // Set the URL for the API endpoint
    val url = new URL(
      s"https://api.fingrid.fi/v1/variable/$variableId/events/csv?start_time=$startTime&end_time=$endTime"
    )

    // Open a connection to the API endpoint
    val connection =
      url.openConnection().asInstanceOf[HttpURLConnection]

    // Set the request method to GET
    connection.setRequestMethod("GET")

    // Set the request headers
    connection.setRequestProperty("Accept", "text/csv")
    connection.setRequestProperty("x-api-key", API_KEY)

    // Send the GET request to the API endpoint
    connection.connect()

    // Read the response from the API endpoint
    val inputStream = connection.getInputStream
    val inputStreamReader = new InputStreamReader(inputStream)
    val reader = new BufferedReader(inputStreamReader)
    val response = new StringBuilder
    var line: String = reader.readLine
    while (line != null) {
      response.append(line + "\n")
      line = reader.readLine
    }
    reader.close()
    // Save response to a CSV file
    val file = new File(filePath)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(response.toString)
    bw.close()

  } catch {
    case e: IOException =>
      System.err.println(
        "An error occurred while fetching the details: " + e.getMessage
      )
  }
}

def readData(filePath: String): Unit = {
  val bufferedSource = io.Source.fromFile(filePath)
  for (line <- bufferedSource.getLines) {
    val cols = line.split(",").map(_.trim)
    println(s"${cols(0)}|${cols(1)}|${cols(2)}")
  }
  bufferedSource.close
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
        println("Energy sources:")
        println("1) Wind\n2) Solar\n3) Hydro\n4) Nuclear\n5) Fossil\n6) All")
        print("Enter your choice: ")
        val choice2 = readLine()
        choice2 match {
          case "1" =>
            println("Wind")
          case "2" =>
            println("Solar")
          case "3" =>
            println("Hydro")
          case "4" =>
            println("Nuclear")
          case "5" =>
            println("Fossil")
          case "6" =>
            println("All")
          case _ =>
            println("Invalid choice")
        }
      case "2" =>
        println("Collecting data...")
        collectData(
          188,
          now.minusMonths(3).toString().concat("Z"),
          now.toString.concat("Z"),
          "nuclear.csv"
        )
        collectData(
          191,
          now.minusMonths(3).toString().concat("Z"),
          now.toString.concat("Z"),
          "hydro.csv"
        )
        collectData(
          181,
          now.minusMonths(3).toString().concat("Z"),
          now.toString.concat("Z"),
          "wind.csv"
        )
        collectData(
          192,
          now.minusMonths(3).toString().concat("Z"),
          now.toString.concat("Z"),
          "data.csv"
        )
        println("Collection completed")
      case "3" =>
        println("Viewing data...")
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
