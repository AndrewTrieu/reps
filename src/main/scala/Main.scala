import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Paths}
import java.util.Date
import java.text.SimpleDateFormat
import scala.util.Try
import scala.io.StdIn.readLine
import scala.io.Source
import scala.collection.immutable.ListMap

val API_KEY = Source.fromFile("apiKey.txt").getLines().mkString

val dataSources = Map(
  188 -> "nuclear.csv",
  191 -> "hydro.csv",
  181 -> "wind.csv",
  192 -> "data.csv"
)

val energySources = "sources.csv"

val alertThreshold = 1000.0

object Main {
  // Collect data from the API
  def collectData(): Unit = {
    // Get the current time and truncate it to seconds
    val now = (java.time.LocalDateTime.now)
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)

    // Collect data from the API
    println("Collecting data...")

    // Iterate over the data sources, not following functional programming paradigm
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

  // Modify the energy sources
  def modifySources(
      lines: List[String],
      newValue: String,
      sourceType: String
  ): List[String] = {
    val temp = lines(1).split(",")
    // Match the source type and create a line with the new value
    val newLine = sourceType match {
      case "Wind"    => s"$newValue,${temp(1)},${temp(2)}"
      case "Hydro"   => s"${temp(0)},$newValue,${temp(2)}"
      case "Nuclear" => s"${temp(0)},${temp(1)},$newValue"
    }
    lines.updated(1, newLine)
  }

  // Read the energy sources
  def readSources(
      modificationFn: (List[String], String, String) => List[String]
  ): Unit = {
    // Read the energy sources
    println("Reading sources...")
    try {
      val bufferedSource = Source.fromFile(energySources)
      def readSourcesHelper(lines: Iterator[String]): Unit = {
        if (lines.hasNext) {
          val line = lines.next()
          val cols = line.split(",").map(_.trim)
          println(s"${cols(0)}\t${cols(1)}\t${cols(2)}")
          readSourcesHelper(lines)
        }
      }
      readSourcesHelper(bufferedSource.getLines)
      bufferedSource.close
    } catch {
      case e: Exception =>
        println("No sources found")
        return
    }

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
        val lines = Source.fromFile(energySources).getLines.toList
        val pw = new java.io.PrintWriter(energySources)
        val newLines = choice2 match {
          case "1" =>
            modificationFn(lines, newValue, "Wind")
          case "2" =>
            modificationFn(lines, newValue, "Hydro")
          case "3" =>
            modificationFn(lines, newValue, "Nuclear")
          case "4" =>
            return List.empty[String]
          case _ =>
            println("Invalid choice")
            List.empty[String]
        }
        // Write the new lines to the file
        if (newLines.nonEmpty) {
          newLines.foreach(pw.println)
          pw.close()
          println("Value updated")
        }
      case "2" =>
        return
      case _ =>
        println("Invalid choice")
    }
  }

  // Calculate the mean, median, mode, range and midrange of the data
  def analyzeData(data: Map[Date, Double]): Unit = {
    if (data == null) {
      println("No data available")
      return
    }

    // Convert the data to a list
    val convertedData = data.values.toList

    // Calculate the mean
    val mean = convertedData.sum / convertedData.length
    println(s"Mean: $mean")

    // Calculate the median
    val median = {
      val (lower, upper) =
        convertedData.sortWith(_ < _).splitAt(convertedData.length / 2)
      if (convertedData.length % 2 == 0) (lower.last + upper.head) / 2.0
      else upper.head
    }
    println(s"Median: $median")

    // Calculate the mode
    val mode = convertedData
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .maxBy(_._2)
      ._1
    println(s"Mode: $mode")

    // Calculate the range
    val range = convertedData.max - convertedData.min
    println(s"Range: $range")

    // Calculate the midrange
    val midrange = (convertedData.max + convertedData.min) / 2
    println(s"Midrange: $midrange")
  }

  // Compare timestamps
  def compareTime(
      lastTimestamp: Date,
      data: List[String],
      filter: Int,
      format: SimpleDateFormat
  ): Map[Date, Double] = {
    // Helper function
    def compareTimeHelper(
        data: List[String],
        interval: Date,
        acc: Map[Date, Double]
    ): Map[Date, Double] = {
      if (data.isEmpty) {
        acc
      } else {
        val cols = data.head.split(",").map(_.trim)
        val timestamp = cols(0)
        val date = format.parse(timestamp)
        if (date.compareTo(interval) >= 0) {
          compareTimeHelper(
            data.tail,
            interval,
            acc + (date -> cols(2).toDouble)
          )
        } else {
          compareTimeHelper(data.tail, interval, acc)
        }
      }
    }

    // Calculate the interval for last hour, last 24-hour, last week, last month or all time
    val interval = filter match {
      case 1 => new Date(lastTimestamp.getTime - 60 * 60 * 1000)
      case 2 => new Date(lastTimestamp.getTime - 24 * 60 * 60 * 1000)
      case 3 => new Date(lastTimestamp.getTime - 7 * 24 * 60 * 60 * 1000)
      case 4 => new Date(lastTimestamp.getTime - 30 * 24 * 60 * 60 * 1000)
      case 5 => new Date(0)
    }

    compareTimeHelper(data.drop(1), interval, Map.empty)
  }

  // Filter the data
  def filterData(
      data: List[String],
      compareFn: (
          Date,
          List[String],
          Int,
          SimpleDateFormat
      ) => Map[Date, Double]
  ): Map[Date, Double] = {
    if (data == null) {
      return null
    }
    // Get last timestamp
    val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
    val lastTimestamp = format.parse(data.last)
    print(
      "Filter by:\n1) Last hour\n2) Last 24 hours\n3) Last week\n4) Last month\n5) All\n6) Exit\nEnter choice: "
    )
    val choice = readLine()
    val filteredTimestamps = choice match {
      case "1" =>
        compareFn(lastTimestamp, data, 1, format)
      case "2" =>
        compareFn(lastTimestamp, data, 2, format)
      case "3" =>
        compareFn(lastTimestamp, data, 3, format)
      case "4" =>
        compareFn(lastTimestamp, data, 4, format)
      case "5" =>
        compareFn(lastTimestamp, data, 5, format)
      case "6" =>
        return Map.empty[Date, Double]
      case _ =>
        println("Invalid choice")
        Map.empty[Date, Double]
    }
    filteredTimestamps
  }

  // Read data from a file
  def readData(): List[String] = {
    print(
      "Sources:\n1) Wind\n2) Hydro\n3) Nuclear\n4) All\n5) Exit\nEnter your choice: "
    )
    val choice = readLine()
    try {
      val bufferedSource = choice match {
        case "1" =>
          Source.fromFile("wind.csv")
        case "2" =>
          Source.fromFile("hydro.csv")
        case "3" =>
          Source.fromFile("nuclear.csv")
        case "4" =>
          Source.fromFile("data.csv")
        case "5" =>
          return null
        case _ =>
          println("Invalid choice")
          null
      }
      val data = bufferedSource.getLines.toList
      bufferedSource.close
      data
    } catch {
      case e: Exception =>
        null
    }
  }

  // Sort the data by timestamp or value
  def sortData(data: Map[Date, Double]): Map[Date, Double] = {
    print(
      "Sort by:\n1) Timestamp (Ascending)\n2) Timestamp (Descending)\n3) Value (Ascending)\n4) Value (Descending)\n5) Exit\nEnter choice: "
    )
    if (data == null) {
      return null
    }
    val choice = readLine()
    val sortedData = choice match {
      case "1" =>
        // Sort data by ascending timestamp
        println("Sorting data by ascending timestamp...")
        ListMap(data.toSeq.sortBy(_._1): _*)
      case "2" =>
        // Sort data by descending timestamp
        println("Sorting data by descending timestamp...")
        ListMap(data.toSeq.sortBy(_._1).reverse: _*)
      case "3" =>
        // Sort data by ascending value
        println("Sorting data by ascending value...")
        ListMap(data.toSeq.sortBy(_._2): _*)
      case "4" =>
        // Sort data by descending value
        println("Sorting data by descending value...")
        ListMap(data.toSeq.sortBy(_._2).reverse: _*)
      case "5" =>
        return Map.empty[Date, Double]
      case _ =>
        println("Invalid choice")
        Map.empty[Date, Double]
    }
    sortedData
  }

  // Print data
  def printData(data: Map[Date, Double]): Unit = {
    def printDataHelper(data: Map[Date, Double]): Unit = {
      if (data.nonEmpty) {
        val (k, v) = data.head
        println(s"${k}: \t${v}MW")
        printDataHelper(data.tail)
      }
    }

    if (data == null) {
      println("No data available")
    } else {
      println("\tTimestamp\t\tValue")
      printDataHelper(data)
    }
  }

  // Search data
  def searchData(data: List[String]): Map[Date, Double] = {
    if (data == null) {
      return null
    }
    val format = new SimpleDateFormat("yyyy-MM-dd")
    print("Enter the date (yyyy-MM-dd) to search for: ")
    val date = readLine()
    val searchDate = format.parse(date)
    // Iterate through the data and add the data to the map if the date matches
    val newData =
      data.drop(1).foldLeft(Map.empty[Date, Double]) { (acc, line) =>
        val cols = line.split(",").map(_.trim)
        val timestamp = cols(0)
        val value = cols(2).toDouble
        if (value < alertThreshold) {
          acc + ((new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss"))
            .parse(timestamp) -> value)
        } else {
          acc
        }
      }
    newData
  }

  // Alert user if energy production is below the threshold
  def alertUser(): Unit = {
    try {
      val sources = dataSources.values.toList.dropRight(1)
      sources.map { source =>
        val bufferedSource = Source.fromFile(source)
        val data = bufferedSource.getLines.toList
        bufferedSource.close
        val alert = data.drop(1).foldLeft(false) { (acc, line) =>
          val cols = line.split(",").map(_.trim)
          val value = cols(2).toDouble
          if (value < alertThreshold) {
            true
          } else {
            acc
          }
        }
        if (alert) {
          println(
            s"ALERT: ${source} has production values below the threshold of ${alertThreshold}MW. Please scan system for details!"
          )
        }
      }
    } catch {
      case e: Exception =>
        return
    }
  }

  // Scan data for anomalies
  def scanSystem(data: List[String]): Map[Date, Double] = {
    if (data == null) {
      return null
    }
    val newData =
      data.drop(1).foldLeft(Map.empty[Date, Double]) { (acc, line) =>
        val cols = line.split(",").map(_.trim)
        val timestamp = cols(0)
        val value = cols(2).toDouble
        if (value < alertThreshold) {
          acc + ((new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss"))
            .parse(timestamp) -> value)
        } else {
          acc
        }
      }
    newData
  }

// Main function
  def main(args: Array[String]): Unit = {
    while (true) {
      alertUser()
      print(
        "REPS management system:\n1) Check energy sources\n2) Collect data\n3) View data\n4) Analyze data\n5) Search data\n6) Scan system\n0) Exit\nEnter your choice: "
      )
      val choice = readLine()

      choice match {
        case "1" =>
          readSources(modifySources)
        case "2" =>
          collectData()
        case "3" =>
          printData(sortData(filterData(readData(), compareTime)))
        case "4" =>
          analyzeData(filterData(readData(), compareTime))
        case "5" =>
          printData(sortData(searchData(readData())))
        case "6" =>
          printData(sortData(scanSystem(readData())))
        case "0" =>
          println("Exiting...")
          System.exit(0)
        case _ =>
          println("Invalid choice")
      }
    }
  }
}
