package chatbot

import CSVHandler._
import RecipeHandler._
import PreferencesManager._


object UserInputHandler{

    def greetUser():String = {
        return "Hello its ChatRecipeey with you , how can I help you? ;)"
    }

    def parseInput(input : String) : List[String] = { // This function is used to break down the user input into words.
        val words = input.split(" ").toList 
        words
    }

    def generateResponse(userName: String, query: String): Unit = {
        val str = "User : "
        var lastResponse = ""
        var exitRequested = false

        while (!exitRequested) {
            print(str)
            val query = scala.io.StdIn.readLine()

            if (query.equalsIgnoreCase("exit") || query.equalsIgnoreCase("quit")) {
            exitRequested = true
            } else {
            lastResponse = handleUserInput(userName, query)
            typeWithDelay(lastResponse)
            }
        }
    }
    def containsKeywords(query: String, keywords: List[List[String]]): Boolean = {
        val lowercaseQuery = query.toLowerCase()
        keywords.exists(keywordList => keywordList.exists(keyword => lowercaseQuery.contains(keyword)))
    }

    def extractKeyword(query: String, keywords: List[List[String]]): String = {
        val lowercaseQuery = query.toLowerCase()
        val queryWords = lowercaseQuery.split("\\s+")
        keywords.flatten.find { keyword =>
            queryWords.exists(_.equalsIgnoreCase(keyword))
        }.getOrElse("")
    }

    def handleUserInput(userName: String, input: String): String = {
        input.toLowerCase() match {
            case "hello" | "hi" | "hey" | "ezayak" => "Hey there! What can I do for you?"
            case "goodbye" | "bye" | "goodnight" => "Goodbye! Have a great day!"
            case str if str.contains("ask") || str.contains("something") =>
            "Sure, I'd be happy to help you with that! "
            case str if str.contains("thank") => "You're welcome! How can I help you further?"
            case str if str.length > 0 =>
            val parsedQuery = parseInput(str)
            val keywords = getKeywords()
            val hasKeywords = containsKeywords(str, keywords)
            if (hasKeywords) {
                val keyword = extractKeyword(str, keywords)
                val existingPreference = getUserPreference(userName).find(_.equalsIgnoreCase(keyword))
                if (existingPreference.isDefined) {
                val preference = getUserPreference(userName)
                typeWithDelay(s"You already have a preference for $preference. Do you want a recipe for it?")
                val userChoice = scala.io.StdIn.readLine()
                if (userChoice.equalsIgnoreCase("yes")) {
                    val newParsedQuery = parsedQuery ++ List(preference).flatten
                    val matchingIndex = findMatchingRow(newParsedQuery, keywords)
                    if (matchingIndex == -1) typeWithDelay("No Recipes Found") else {
                    print_recipe(matchingIndex)
                    return "Here is your recipe! Anything else?."
                    }
                }
                } else {
                typeWithDelay(s"You don't have a preference for $keyword. Do you want to add it as your preference? (yes/no)")
                val addPreferenceChoice = scala.io.StdIn.readLine()
                if (addPreferenceChoice.equalsIgnoreCase("yes")) {
                    // Store the new preference
                    storeUserPreference(userName, keyword)
                }
                }
            }
            val matchingIndex = findMatchingRow(parsedQuery, keywords)
            if (matchingIndex == -1) println("No Recipes Found") else {
                print_recipe(matchingIndex)
                return "Here is your recipe! Anything else?."
            }
            "Okay, let's continue."
            case _ => "I'm sorry, I didn't understand that. Can you please try again?"
        }
    }
    def askForUserName(): String = {
        typeWithDelay("What is your name?")
        print("User : ")
        scala.io.StdIn.readLine()
    }
}

