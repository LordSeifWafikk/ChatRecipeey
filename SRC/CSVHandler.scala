package chatbot
import scala.io.Source // for the reading the csv file
import scala.concurrent.duration._

object CSVHandler{
  
  /************************************ Dataset Handling *************************************/

  def readCSV(): List[String] = { //reading the csv file
    val source = Source.fromFile("Recipes.csv")
    val lines = source.getLines().toList // Convert iterator to list
    lines // Close the source after usage
  }

  def formatToList(input: String): List[String] = {
    // Define the regex pattern to extract elements enclosed in single quotes and separated by hashes
    val pattern = """'([^']+)'""".r
    // Find all matches of the pattern in the input string and extract the captured groups
    val elements = pattern.findAllMatchIn(input).map(_.group(1)).toList
    elements
  }    

  def formatData(data: String): List[String] = { // only for the ingredients data form
    data.replaceAll("[\\[\\]\"]", "").split("#").toList.map(_.trim)
  }


  def extractColumn(columnIndex:Int): List[List[String]] = {     
    val lines = readCSV()
    // Skip the header row (assuming it's the first row)
    val dataLines = lines.tail
      
    val keywordLists = dataLines.map(line => {
      val columns = line.split(",").map(_.trim)
      if (columns.length > columnIndex) {
      // Assuming the keywords are in the format "keyword1- keyword2- ..."
        val keywordsStr = columns(columnIndex)
        val keywords = formatToList(keywordsStr)
        keywords
      } else {
        List.empty[String]
      }
    })      
    // Filter out any empty keyword lists
    keywordLists.filter(_.nonEmpty)
  }

    
  def getNamesColumn(): List[List[String]] = {
      val columnIndex = 1
      val lines = readCSV()
      // Skip the header row (assuming it's the first row)
      val dataLines = lines.tail
      val nameLists = dataLines.map(line => {
      val columns = line.split(",").map(_.trim)
      if (columns.length > columnIndex) {
          // Extracting names, assuming names may contain spaces
          val name = columns(columnIndex)
          // If name contains spaces, split and return as separate names
          if (name.contains(" ")) {
          name.split(" ").filterNot(_.isEmpty).filterNot(_ == "&").toList
          } else {
          // Filter out "&" if it's present as a single word
          List(name).filterNot(_ == "&")
          }
      } else {
          List.empty[String]
      }
      })
      // Filter out any empty name lists
      nameLists.filter(_.nonEmpty)
  }

  def mergeLists(list1: List[List[String]], list2: List[List[String]]): List[List[String]] = {
    list1.zip(list2).map { case (row1, row2) =>
      val mergedRow = row1 ++ row2
      mergedRow.distinct // Drop duplicates from the merged row
    }
  }
  def getKeywords(): List[List[String]] = {
    val tags = extractColumn(4)
    val search = extractColumn(5)
    val names = getNamesColumn()
    mergeLists(mergeLists(tags, search), names)
  }

    
}