/**
 * TaxEntryProcessor.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: edgarust
 * Person#: 50230866
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT: edgarust
 */
package cse250.pa0.objects

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

import scala.io.{BufferedSource, Source}
import cse250.assignments.objects.TaxEntry

import scala.collection.mutable.ArrayBuffer

object TaxEntryProcessor {
  def sanitizer(lines: Iterator[String]): ArrayBuffer[ArrayBuffer[String]] = {
    // Large Buffer Array to hold every line of the file
    var bigList: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer[ArrayBuffer[String]]()
    for (line <- lines){
      var smallList: ArrayBuffer[String] = ArrayBuffer[String]() // Smaller Buffer Array that holds individual lines of the file
      var smallString: String = ""
      var quotes: Boolean = false
      var quoteCounter: Int = 0 // The counter that acts like a T-Flop
      var index: Int = 0
      while (index < line.length){
        // Accounts for behavior of separators of quotes
        if (line(index) == '"'){
          if (quoteCounter == 0){
            quotes = true
            quoteCounter = 1
          }
          else if (quoteCounter == 1){
            quotes = false
            quoteCounter = 0
          }
        }
        // Accounts for the behavior of commas
        if (line(index) == ','){
          if (!quotes){
            smallList += smallString
            smallString = ""
          }
          if (quotes){
            smallString = smallString + line(index)
          }
        }
        else{
          smallString = smallString + line(index)
        }
        index += 1
      }
      smallList += smallString
      bigList += smallList
    }
    bigList
  }

  def sanitizeData(filename: String): Unit = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile: BufferedSource = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines: Iterator[String] = inputFile.getLines()

    val outputFile: BufferedWriter = new BufferedWriter(new FileWriter(new File(filename + "-updated")))

    // Without the '\n' character, all output will be written as one long line.
    // Process the lines.

    val bigList: ArrayBuffer[ArrayBuffer[String]] = sanitizer(lines)

    // Removing unnecessary columns using ArrayBuffer.remove function
    val fetusDeletus: List[Int] = List(0, 0, 5, 5, 5, 5, 5, 5, 5, 11, 11, 11, 12, 18, 18, 18, 23, 23, 23)
    for (smallList <- bigList){
      for (fetus <- fetusDeletus){
        smallList.remove(fetus)
      }
    }

    // Removing each row that does not contain a 5-DIGIT Zip code
    var index:Int = 0
    while(index < bigList.size){
      if (bigList(index)(10).isEmpty){
        bigList.remove(index)
        index -= 1
      }
      index += 1
    }

    // Writing each line to a new file
    for (line <- bigList){
      var saniString: String = ""
      var index: Int = 0
      while (index < line.size - 1){
        saniString = saniString + line(index) + ","
        index += 1
      }
      saniString = saniString + line(index)
      outputFile.write(saniString + "\n")
    }

    // Close the files at the end.
    inputFile.close()
    outputFile.close()
  }

  def computeMostExpensiveEntry(filename: String): TaxEntry = {
    val mostExpensiveProperty: TaxEntry = new TaxEntry
    val inputFile: BufferedSource = scala.io.Source.fromFile(filename)
    val lines: Iterator[String] = inputFile.getLines()
    val lysol: ArrayBuffer[ArrayBuffer[String]] = sanitizer(lines)
    var mostExpensiveCan: Int = 0
    var mostExpensiveRow: Int = 0
    for (spray <- 1 until lysol.size){
      if (lysol(spray)(13).toInt > mostExpensiveCan){
        mostExpensiveCan = lysol(spray)(13).toInt
        mostExpensiveRow = spray
      }
    }

    for (headings <- lysol.head.indices){
      mostExpensiveProperty.infoMap += (lysol.head(headings) -> lysol(mostExpensiveRow)(headings))
    }
//    println(mostExpensiveCan, mostExpensiveRow)

    inputFile.close()
    mostExpensiveProperty
  }

  def computeOldestEntry(filename: String): TaxEntry = {
    val oldestProperty: TaxEntry = new TaxEntry
    val inputFile: BufferedSource = scala.io.Source.fromFile(filename)
    val lines: Iterator[String] = inputFile.getLines()
    val lysol: ArrayBuffer[ArrayBuffer[String]] = sanitizer(lines)
    var oldestCan: Int = 2019
    var oldestRow: Int = 0
    for (spray <- 1 until lysol.size){
      if (lysol(spray)(15).nonEmpty && lysol(spray)(15).toInt < oldestCan && 1700 < lysol(spray)(15).toInt){
        oldestCan = lysol(spray)(15).toInt
        oldestRow = spray
      }
    }

    for (headings <- lysol.head.indices){
      oldestProperty.infoMap += (lysol.head(headings) -> lysol(oldestRow)(headings))
    }
    //    println(mostExpensiveCan, mostExpensiveRow)

    inputFile.close()
    oldestProperty
  }
}
