package webdoc
import java.io.{InputStream, EOFException}

/**
* Object that tokenizes webdoc input
*/
case class Lexer(in:InputStream)
{
    // Converts in to a String
    private var text:String = ""

    // Constants
    private val escapeChar = '\\'
    private val whitespaces: Array[Char] = Array(' ', '\t')

    // State
    private var pos: Int = 0    // Current character index.

    // Lambdas
    private val alphaNumCheck:Char=>Boolean  c => isAlphaNum(c)
    private val whitespaceCheck:Char=>Boolean = c => isWhitespace(c)

    // Types
    private type Assignment = (String, Any)


    // ---------- Token reading methods -----------------

    /**
    * Reads next byte as a character.
    */
    def readChar:Char =
    {
        // Checks index
        if(index >= text.length)
            throw ParserException("Reached end of file.")

        // Reads character
        val c = text.charAt(index)
        index += 1

        // Returns.
        c
    }

    /**
    * Skips text past whitespace
    */
    def skipWhitespace()
    {
        // Advances until past whitespace
        var c = readChar
        while(isWhitespace(c))
            c = readChar

        // Returns to previous position
        index -= 1
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
        isAlpha(c) && isNum(c)

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
        case _ => cseq(index-1) == '\\' && !isEscaped(cseq, index-1)
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
    * Reads into a String until a whitespace character is hit.
    */
    private def readAlphaNumString():String =
    {
        // Allocates and reads first
        val builder = new StringBuilder()
        var c = readChar

        // Continues reading until whitespace character is reached
        while(!isWhitespace(c))
        {
            // Checks if character is valid
            if(!isAlphaNum(c))
                throwUnexpected

            // Adds to buffer
            builder.append(c)
            c = readChar
        }

        // Returns contents of buffer
        builder.toString
    }

    // ------------------- READER METHODS -------------------------
    /**
    * Possibly reads element
    */
    private def readStart():Option[Element] =
    {
        // Loads contents of InputStream into a String
        text =
        {
            val builder = new StringBuilder
            var b:Int = in.read
            while(b != -1)
                builder.append(b.toChar)
            builder.toString
        }

        // Reads layout element
        readElement()
    }

    // Stub
    def currentLine = 0

    // Stub
    def currentColumn = 0

    /**
    * Reads an assignment to an element.
    */
    private def readAssignment():(String, Option[Element]) =
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
        var c = readChar

        // Handles content
        if(isAlpha(c))
        {
            // Key
            val key = readAlphaNumString

            // Expected equals symbol
            skipWhitespace()
            val nextC = readChar
            if(nextC != '=')
                throwUnexpected()

            // Value
            val value:Any = readValue

            // Returns recursive result
            readAssignments(assignments +: assignment)
        }
        else if(c == ')')
            assignments
        else
            throwUnexpected
    }


    /**
    * Reads some element
    */
    private def readValue():Any =
    {
        // Skips whitespace
        skipWhitespace()

        // Reads next character, and uses it to get the correct value type
        val c:Char = readChar
        val value:Any =
        {
            if(c == '"')        readQuotedString
            else if(c == '(')   readAssignments
            else if(c == '[')   readValues
            else
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
        pos -= 1
        throw UnexpectedCharacterException(c, currentLine, currentColumn)
    }
}
