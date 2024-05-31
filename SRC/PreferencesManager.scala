package chatbot

import scala.io.Source
import java.io.PrintWriter //  for writing to a new text file
import scala.util.control.Breaks._  // for using break statement in loops
import java.io.File
import java.io.FileReader

object PreferencesManager{
    // Function to store user preferences
  def storeUserPreference(userName: String, input: String): Unit = {
    val userFile = new File(s"$userName.txt")
    val writer = new PrintWriter(new java.io.FileWriter(userFile, true))
    writer.println(input)
    writer.close()
  }

// Function to retrieve user preferences
  def getUserPreference(userName: String): List[String] = {
    val userFile = new File(s"$userName.txt")
    if (userFile.exists() && userFile.isFile) {
      val preferences = Source.fromFile(userFile).getLines().toList
      preferences
    } else {
      println(s"Preferences file for user $userName does not exist.")
      List.empty[String]
    }
  }
}