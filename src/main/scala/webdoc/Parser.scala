package webdoc
import java.io.{BufferedInputStream, File, FileInputStream, InputStream, EOFException}
import com.google.common.io.{CountingInputStream}
import scala.collection.immutable.{Seq, Map}
import Parser._


/**
* Represents some Parser for a web document.
*/
case class Parser(escape:Char = '\\')
{
    // Hidden state, don't mind me ;)
    private var builder = new StringBuilder()
    private var line = 1
    private var column = 0
    private var in:InputStream = null
    private var escapeSwitch = -1
    private var lastRead:Char = 0

    /**
    * Parses a WebDoc from an InputStream
    * @param in InputStream to read from
    */
    def parse(stream:InputStream):Element =
    {
        // Secretly stores information for later usage
        in = stream
        line = 1
        escapeSwitch = -1

        // Begins
        readElement
    }

    /**
    * Reads an Element
    */
    def readElement():Element =
    {
        // Reads name of element
        val name:String = readUntilChar("({").trim()

        // Reads metadata within parenthesis.
        println(lastRead)
        val parenMeta:Map[String, Element] = if(lastRead == '(')
            readParenMeta
        else
            Map.empty

        // Reads children
        val metaAndChildren:(Map[String, Element], Seq[Element]) = if(lastRead == '{')
        {
            readMetaAndChildren
        }
        else
        {
            (Map.empty, Seq.empty)
        }

        // Resulting Element
        val meta = parenMeta ++ metaAndChildren._1
        val children = metaAndChildren._2
        Element(name, meta, children)
    }

    /**
    * parses a WebDoc from a File
    */
    def parseFile(file:File, bufferSize:Int = 1024):Element =
    {
        val in = new BufferedInputStream(new FileInputStream(file), bufferSize)
        try
        {
            parse(in)
        }
        finally
        {
            in.close
        }
    }


    /**
    * Whitespace checker
    */
    private def isWhitespace(c:Char):Boolean =
        c == ' ' || c == '\t' || c == '\t' || c == '\n'

    /**
    * @return true if c is alphabetic
    */
    private def isAlpha(c:Char):Boolean = c >= 65 && c <= 90 || c >= 97 && c <= 122

    /**
    * @return true if c is numeric
    */
    private def isNumeric(c:Char):Boolean = c >= 48 && c <= 57

    /**
    * @return true if c is alpha-numeric
    */
    private def isAlphaNumeric(c:Char):Boolean = isNumeric(c) || isAlpha(c)

    /**
    * Determines if current StringBuilder is escaping the next
    * character it will append.
    */
    private def isEscaping:Boolean = escapeSwitch == 1

    /**
    * Reads from InputStream stored.  Ignores whitespace.
    */
    private def read:Char =
    {
        // Reads character
        var b:Int = in.read
        b match
        {
            case '\n' =>
                column = 0
                line += 1
            case -1 =>
                throw new EOFException
            case _ => {}
        }
        column += 1

        // Returns result
        lastRead = b.toChar
        lastRead
    }

    /**
    * Reads a character.
    */
    private def readChar = read.toChar


    /**
    * Reads first character that isn't a whitespace
    */
    private def readCharIgnore:Char =
    {
        var c = readChar
        while(isWhitespace(c))
            c = readChar
        c
    }


    /**
    * Reads a character.  Throws exception if not
    * expecting a whitespace character.
    */
    private def readCharComplain:Char =
    {
        val c = readChar
        if(isWhitespace(c))
            throw new WhitespaceException(line, column)
        c
    }

    /**
    * Reads in next token assuming it's alphanmueric
    */
    private def readAlphaNum():String =
    {
        var c:Char = readChar
        ""
    }

    /**
    * Reads information within parenthesis.
    */
    private def readParenMeta():Map[String, Element] =
    {
        // Reads remaining data
        val data:String = readUntilChar(")")

        // Splits it on comma
        val split:Array[String] = data
            .replace('\n', ',')
            .split(",")


        // Interprets each
        null
    }

    private def readMetaAndChildren():(Map[String, Element], Seq[Element]) =
    {
        null

    }


    /**
    * Reads content until a certain character is reached.
    * @param in InputStream to read from
    * @param until Character to stop at.
    * @param complains Boolean that determines if the parser should complain if there is a whitespace character.
    */
    private def readUntilChar(until:String, complains:Boolean=false):String =
    {
        // Clears buffer
        builder.setLength(0)

        // Appends to buffer
        var b = 0
        var c = ' '
        var contains = false
        do
        {
            c = if(complains) readCharComplain else readCharIgnore
            contains = containsChar(until, c)
            if(!contains)
            builder.append(c)
        }
        while(!contains || isEscaping)

        // Returns as String
        builder.toString
    }

    /**
    * @return true if str contains the character c.
    */
    private def containsChar(str:String, c:Char):Boolean =
    {
        var i:Int = 0
        while(i < str.length)
        {
            if(c == str.charAt(i))
                return true
            i += 1
        }
        false
    }
}

/**
* Companion object to Parser
*/
object Parser
{
    private[webdoc] val regWhitespace = "[\n\r ]"
    private[webdoc] val emptyPair:(Map[String, Element], Seq[Element]) = (Map.empty, Seq.empty)
}
