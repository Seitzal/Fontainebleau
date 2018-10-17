package eu.seitzal.funcsv

import scala.annotation.tailrec
import scala.io.Source
import java.io._

/**
 * Purely functional implementation of a CSV encoder/decoder in Scala
 */
object FunCSV {

    /**
     * Serialises the specified data using the CSV format
     * @param data A two-dimensional list containing the data
     * @param always_quote Whether to  surround all elements in quotation marks, not just those containing commas or quotation marks
     * @return The string containing the serialised data  
     */
    def encode(data : List[List[String]], always_quote : Boolean = false) : String = {

        def encodeLine(line : List[String]) : String = {

            def escaped(x : String) : String =
                if(x.contains("\"") || x.contains(",") || always_quote)
                    "\"" + x.replaceAll("\"", "\"\"") + "\""
                else x
            
            @tailrec def iter(remainder : List[String], current : String) : String = {
                if(remainder.isEmpty)
                    current
                else if(remainder.length > 1)
                    iter(remainder.tail, current + escaped(remainder.head) + ",")
                else
                    iter(remainder.tail, current + escaped(remainder.head))
            }

            iter(line, "")
        }

        @tailrec def iter(remainder : List[List[String]], current : String) : String = {
            if(remainder.isEmpty)
                current
            else if(remainder.length > 1)
                iter(remainder.tail, current + encodeLine(remainder.head) + "\n")
            else
                iter(remainder.tail, current + encodeLine(remainder.head))
        }

        iter(data, "")
    }

    /**
     * Replaces Windows-style and old Macintosh-style line endings with Linux style line endings.
     */
    def fixLineEndings(raw : String) : String =
        raw.replaceAll("\r\n", "\n").replaceAll("\r", "\n")

    /**
     * Deserialises the specified CSV-formatted data
     * @param raw The serialised data
     * @return The two-dimensional immutable list containing the deserialised data
     */
    def decode(raw : String) : List[List[String]] = {

        def parseLine(line : String) : List[String] = {

            val escapedCommas : List[Int] = {
                @tailrec def iter(i : Int, inQuotedBlock : Boolean, current : Vector[Int]) : Vector[Int] = {
                    if(i < line.length) {
                        val char = line.charAt(i)
                        if(char == '\"')
                            iter(i + 1, !inQuotedBlock, current)
                        else if(char == ',' && inQuotedBlock)
                            iter(i + 1, inQuotedBlock, current :+ i)
                        else
                            iter(i + 1, inQuotedBlock, current)

                    } else current
                }
                iter(0, false, Vector[Int]()).toList
            }

            @tailrec def split(i : Int, elementStart : Int, current : Vector[String]) : Vector[String] = {
                if(i < line.length) {
                    val char = line.charAt(i)
                    if(char == ',' && !escapedCommas.contains(i))
                        split(i + 1, i + 1, current :+ line.substring(elementStart, i))
                    else
                        split(i + 1, elementStart, current)
                } else 
                    current :+ line.substring(elementStart, i)
            }

            def cleanup(lines : List[String]) : List[String] = {
                def cleanFirst(x : String) = if(x.charAt(0) == '\"') x.substring(1) else x
                def cleanLast(x : String) = if(x.charAt(x.length - 1)  == '\"') x.substring(0, x.length - 1) else x
                def cleanQuotes(x : String) = x.replaceAll("\"\"", "\"")
                for(line <- lines) yield {
                    if(line.length > 0) cleanLast(cleanFirst(cleanQuotes(line)))
                    else line
                }
            }
            cleanup(split(0, 0, Vector[String]()).toList)
        }

        val lines = fixLineEndings(raw).split('\n').toList
        for (line <- lines) yield parseLine(line)
    }

    /**
     * Serialises the specified data using the CSV format, and saves it as a CSV file
     * @param data A two-dimensional list containing the data
     * @param path The path to the file where the data should be saved. The file will be created if it is missing, and overwritten if it exists
     * @param always_quote Whether to  surround all elements in quotation marks, not just those containing commas or quotation marks
     * @return The string containing the serialised data  
     */
    def encodeFile(data : List[List[String]], path : String, always_quote : Boolean = false) : String = {
        val encoded = encode(data, always_quote)
        val file = new File(path)
        if(file.exists) 
            file.delete()
        val writer = new PrintWriter(file)
        writer.write(encoded)
        writer.close()
        encoded
    }

    /**
     * Deserialises the CSV file at the specified path
     * @param path The path to the CSV file
     * @return The two-dimensional immutable list containing the deserialised data
     */
    def decodeFile(path : String) = decode(Source.fromFile(path).mkString)   
}
