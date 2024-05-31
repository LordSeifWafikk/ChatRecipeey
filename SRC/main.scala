package chatbot

import scala.io.StdIn.readLine //for taking the input from the user
import java.io.PrintWriter 
import java.io.File
import java.io.FileReader

//Modularization//
import CSVHandler._
import RecipeHandler._
import UserInputHandler._
import PreferencesManager._


  
object Main{
  @main def run: Unit = {
    typeWithDelay(greetUser())
    val userName = askForUserName() // Obtain userName
    val userFile = new File(s"$userName.txt")

    if (!userFile.exists()) {
      typeWithDelay(s"Welcome, $userName! Let's start a new chat.")
      userFile.createNewFile()
    } else {
      typeWithDelay(s"Welcome back, $userName! You already have a chat history.")
      println("Do you want to continue the existing chat or start a new one?")
      val response = scala.io.StdIn.readLine()
      if (response.equalsIgnoreCase("continue")) {
        // If the user wants to continue the existing chat, check if there are old preferences
        val oldPreferences = getUserPreference(userName)
        if (oldPreferences.nonEmpty) {
          typeWithDelay("Do you want to continue with your old preferences? (yes/no)")
          val continueWithOldPreferences = scala.io.StdIn.readLine()
          if (continueWithOldPreferences.equalsIgnoreCase("yes")) {
            // Get the user's old preference and apply the same process
            val oldPreference = getUserPreference(userName)
            val matchingIndex = findMatchingRow(oldPreference, getKeywords())
            if (matchingIndex != -1) {
              print_recipe(matchingIndex)
              return
            }
          }
        }
      } else if (response.equalsIgnoreCase("new")) {
        typeWithDelay("Starting a new chat...")
        userFile.delete()
        userFile.createNewFile()
      }
    }
    generateResponse(userName, "Hello") // Start the chat with initial input "Hello"
  }

//   //----------------------------------------------------------------------------------//

}
