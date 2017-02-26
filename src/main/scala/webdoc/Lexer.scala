package webdoc
import java.io.{InputStream, EOFException}
import scala.collection.immutable.{ListMap}

/**
* Object that tokenizes webdoc input
*/
case class Lexer(in:InputStream)
{
    // Converts in to a String
    private var text:String = ""

    // Constants
    private val escapeChar = '\\'
    private val whitespaces: Array[Char] = Array(' ', '\t', '\n')

    // State
    private var pos: Int = 0    // Current character index.

    // Types
    private type Assignment = (String, Any)
    private type Attempt = ()=>Any


    // ---------- Token reading methods ----------------
    

    /**
    * Reads next byte as a character.
    */
    private def readChar:Char =
    {
        // Checks index
        if(pos >= text.length)
            throw ParserException("Reached end of file.", currentLocation)

        // Reads character
        val c = text.charAt(pos)
        pos += 1

        // Returns.
        c
    }
    
    /**
     * Reads next byte as a character without advancing
     * internal pointer.
     */
    private def peekChar:Char =
    {
      // Checks index
      if(pos >= text.length)
        throw ParserException("Reached end of file.", currentLocation)
      
      // Reads character
      text.charAt(pos)
    }
    
    private def atEOF:Boolean = pos == text.length
    
    /**
    * Skips text past whitespace
    * @return true if reached end of file.
    */
    private def skipWhitespace():Boolean =
    {
      // Advances until past whitespace
      if(atEOF) true
      else
      {
        val c = readChar
        if(!c.isWhitespace)
        {
          pos -= 1
          false
        }
        else
        {
          skipWhitespace()
        }
      }
    }

    /**
    * @return true if c is a whitespace.
    */
    private def isWhitespace(b:Int):Boolean =
    {
        var i = 0
        while(i < whitespaces.length)
        {
            if(b == whitespaces(i))
                return true
            i += 1
        }
        false
    }

    /**
    * Determines if character is alphabetic
    */
    private def isAlpha(c:Int):Boolean =
        (c >= 65 && c <= 90) || (c >= 97 && c <= 122)

    /**
    * Determines if character is numeric
    */
    private def isNum(c:Int):Boolean =
        (c >= 48 && c <= 57)

    /**
    * determines if character is alpha-numeric
    */
    private def isAlphaNum(c:Int):Boolean =
        isAlpha(c) || isNum(c)

    /**
    * Determines if C is the start of a Block.
    */
    private def isBlockChar(c:Int):Boolean =
        c == '\'' || c == '"' ||c == '{' || c == '('

    /**
    * @param cseq CharacterSequence in question
    * @param index Character in cseq to check for escaping status.
    * @return true if character specified is escaped.
    */
    private def isEscaped(cseq:CharSequence, index:Int):Boolean = index match
    {
        case 0 => false
        case _ => cseq.charAt(index-1) == '\\' && !isEscaped(cseq, index-1)
    }

    /**
    * Reads contents of File into a String until a double quote is found.
    * Assumes already advanced passed starting quote.
    */
    private def readQuotedString():String =
    {
        // Initializes
        val builder = new StringBuilder()
        var c = ' '
        var done = false

        // Reads until end quote.
        do
        {
            c = readChar
            c match
            {
                case '"' => done = true
                case '\\' => builder.append(readChar)
                case _ => builder.append(c)
            }
        }
        while(!done)

        // Returns result
        builder.toString
    }
    
    
    
    /**
     * Attempts to run all functions in the sequence.
     * Stops at first one that succeeds, and returns result.
     * @throws Exception if the last function fails.
     */
    private def attemptAll(attempts:Seq[Attempt], index:Int=0):Any =
    {
      val fun:()=>Any = attempts(index)
      if(index == attempts.length-1)
        fun()
      else
      {
        // Tries to invoke current attempt
        val savePos:Int = pos
        try
        {
          fun()
        }
        
        // Recovers and tries the next
        catch
        {
          case t:Throwable =>
            pos = savePos
            attemptAll(attempts, index+1)
        }
      }
    }
    
    /**
     * Reads a number as a Double.
     */
    private def readNum():Double =
    {
      // Prepares
      val builder = new StringBuilder()
      var c = peekChar
      
      // Buffers characters
      while(!isWhitespace(c) && c != ')' && c != ']')
      {
        // Checks that character is numeric
        if(!isNum(c) && c != '.')
          throwUnexpected
        
        // Continues to read
        pos += 1
        builder.append(c)
        c = peekChar
      }
      
      // Parses and returns result
      val str = builder.toString
      try
      {
        str.toDouble
      }
      catch
      {
        case t:Throwable => throw ParserException("Could not parse decimal number", currentLocation)
      }
    }
    
    
    /**
     * Reads a value as a Boolean.
     */
    private def readBool():Boolean =
    {
      val str:String = readAlphaNumString
      try
      {
        str.toBoolean
      }
      catch
      {
        case t:Throwable => throw ParserException("Could not parse bool", currentLocation)
      }
    }

    /**
    * Reads into a String until a whitespace character is hit.
    */
    private def readAlphaNumString():String =
    {
        // Allocates and reads first
        val builder = new StringBuilder()
        var c:Char = peekChar
        if(!isAlpha(c))
          throwUnexpected

        // Continues reading until whitespace character is reached
        while(isAlphaNum(c))
        {
            // Adds to buffer
            pos += 1
            builder.append(c)
            c = peekChar
        }

        // Returns contents of buffer
        builder.toString
    }

    // ------------------- READER METHODS -------------------------
    /**
    * Possibly reads element
    */
    def parse():Element =
    {
        // Buffers contents of InputStream into a String
        text =
        {
            val builder = new StringBuilder
            var b:Int = in.read
            while(b != -1)
            {
                builder.append(b.toChar)
                b = in.read
            }
            builder.append('\n')
            builder.toString
        }
        
        // Reads Element
        readElement
    }
    
    
    /**
     * Current location in the file (line/column)
     */
    private def currentLocation:(Int, Int) =
    {
      var i = 0
      var line = 1
      var column = 1
      while(i < pos)
      {
        var c = text.charAt(i)
        if(c == '\n')
        {
          line += 1
          column = 0
        }
        column += 1
        
        i += 1
      }
      
      // Returns result
      (line, column)
    }

    // Stub
    private def currentLine = currentLocation._1

    // Stub
    private def currentColumn = currentLocation._2

    /**
    * Reads an assignment to an element.
    */
    private def readAssignment():Assignment =
    {
        // Skips whitespace
        skipWhitespace()

        // Reads name
        val key:String = readAlphaNumString

        // Reads value
        val value:Any = readValue

        // Returns
        (key, value)
    }

    /**
    * Reads a sequence of assignments
    * @param assignments Current seq of assignments created.
    * Defaults to empty Seq.
    */
    private def readAssignments(assignments:Seq[Assignment] = Seq.empty):Seq[Assignment] =
    {
        // Skips whitespace and reads character
        skipWhitespace()
        var c = peekChar

        // Handles content
        if(isAlpha(c))
        {
            // Key
            val key = readAlphaNumString

            // Expected equals symbol
            skipWhitespace()
            val nextC:Char = peekChar
            if(nextC != '=')
                throwUnexpected()
            pos += 1

            // Value
            val value:Any = readValue
            val assignment:Assignment = (key, value)

            // Returns recursive result
            val result = readAssignments(assignments :+ assignment)
            result
        }
        
        // End of assignments
        else if(c == ')')
        {
          // Advances
          pos += 1
          
          // Attempts to read following array
          val eof:Boolean = skipWhitespace()
          if(!eof && peekChar == '[')
          {
            pos += 1
            val children:Seq[Any] = readSequence()
            assignments :+ ("children", children)
          }
          else
          {
            assignments
          }
        }
        
        // Unexpected
        else
        {
          throwUnexpected
          Seq.empty
        }
    }
    
    /**
     * Reads a sequence of certain values
     * @param seq Sequence accumulated. Empty by default.
     * @return Sequence of elements.
     */
    private def readSequence(seq:Seq[Any] = Seq.empty):Seq[Any] =
    {
      // Skips whitespace
      skipWhitespace()
 
      // Peeks at character
      val c:Char = peekChar
      c match
      {
        case ']' =>
          pos += 1
          seq.toArray.toSeq
        case _ =>
          val value:Any = readValue()
          readSequence(seq :+ value)
      }
    }
    
    /**
     * Reads next content as an Element
     */
    private def readElement():Element =
    {
      // Skips whitespace just in case
      skipWhitespace()
      
      // Tries to buffer in name header
      val builder = new StringBuilder()
      var c:Char = peekChar
      
      // Only looks for name at start if first character is not the beginning of the object.
      if(c != '(')
      {
        if(!isAlpha(c))
          throwUnexpected
                
        // Reads the rest
        while(isAlphaNum(c))
        {
          pos += 1
          builder.append(c)
          c = peekChar
        }
        
        // Skips whitespace
        skipWhitespace()
      }
            
      // Checks if next character is valid
      c = peekChar
      if(c != '(')
        throwUnexpected
      pos += 1

      // Stores name and reads assignments
      val assignments:Seq[Assignment] = readAssignments()
      val meta:Map[String, Any] = ListMap(assignments:_*)
      val name:Any = meta.get("name") match
      {
        case Some(a:Any) => a
        case None => builder.toString
      }
      
      // Builds RawElement
      val start:Map[String, Any] = ListMap(("name", name))
      RawElement(start ++ meta)
    }

    /**
    * Reads some element
    */
    private def readValue():Any =
    {
        // Skips whitespace and peeks at the next character
        skipWhitespace()
        val c:Char = peekChar
        
        // Uses it to evaluate following text
        val value:Any =
        {
            if(c == '"')        // Start of a String
            {
              pos += 1
              readQuotedString()
            }
            else if(c == '(')  // Start of a list of assignments
            {
              pos += 1
              readAssignments()
            }
            else if(c == '[')  // Start of an array
            {
              pos += 1
              readSequence()
            }
            else if(isAlpha(c))  // Start of a a boolean or an element
            {
              attemptAll(Seq(
                  () => readBool(),
                  () => readElement()
              ))
            }
            else if(isNum(c))  // Start of a number
            {
              readNum()
            }
            else              // Unexpected
            {
                throwUnexpected
                "Unreachable"
            }
        }

        // Returns result
        value
    }

    /**
    * Throws an unexpected character exception for the previous character
    */
    private def throwUnexpected()
    {
        throw UnexpectedCharacterException(text(pos), currentLocation)
    }
}
