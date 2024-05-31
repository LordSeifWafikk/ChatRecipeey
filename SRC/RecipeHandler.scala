package chatbot
import scala.util.Random       // for selecting random matching lines from the csv file
import scala.concurrent.duration._ // for delaying the printing

import CSVHandler._

object RecipeHandler{

  def findMatchingRow(parsedQuery: List[String], csvKeywords: List[List[String]]): Int = {
    // Initialize variables to track the best match
    var bestMatchIndex = -1
    var bestMatchCount = 0
    var matches: List[Int] = Nil

    // Iterate through each row in the CSV using foreach
    csvKeywords.zipWithIndex.foreach { case (rowKeywords, rowIndex) =>
      // Convert the query keywords to lowercase
      val parsedQueryLower = parsedQuery.map(_.toLowerCase)
      // Convert the row keywords to lowercase
      val rowKeywordsLower = rowKeywords.map(_.toLowerCase)
      // Count the number of matching keywords between the query and the row
      val matchCount = parsedQueryLower.count(queryKeyword => rowKeywordsLower.exists(rowKeyword => rowKeyword == queryKeyword))

      // Update the best match information if applicable
      if (matchCount > bestMatchCount) {
        bestMatchIndex = rowIndex
        bestMatchCount = matchCount
        matches = List(rowIndex)
      } else if (matchCount == bestMatchCount && matchCount > 0) {
        matches = rowIndex :: matches
      }
    }

    // If there are no matches found, return -1
    if (bestMatchCount == 0) {
      -1
    } else {
      // If there are matches found, return a random match from the matches list
      Random.shuffle(matches).head
    }
  }

  def typeWithDelay(text: String): Unit = {
    text.foreach { char =>
      print(char)
      Thread.sleep(30) // Adjust the delay time as needed
    }
    println()
  }


  def print_recipe(matching_rowIndex : Int): Unit =  {
    val lines = readCSV()  
    val matchingline = lines.tail(matching_rowIndex).split(",").toList //splitting the matching line by "," that comes from source.io and make it a List()
    val nameIndex = 1 // name is column no 1
    val ingredientsIndex = 2
    val stepsIndex = 3

    val name =  matchingline(nameIndex) 
    val ingredients =  formatData(matchingline(ingredientsIndex))
    val steps =  formatToList(matchingline(stepsIndex))

    println()
    typeWithDelay(s"Recipe: $name")
    println()
    typeWithDelay("Ingredients:")
    ingredients.foreach(ingredient => typeWithDelay(s"- $ingredient"))
    println()
    typeWithDelay("Steps:")
    steps.zipWithIndex.foreach { case (step, index) =>
      typeWithDelay(s"${index + 1}. $step")
    }
  }




  

}