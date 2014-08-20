//to read and write files.
package learn.io
import scala.io.Source
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object fileWriter{

def open(name: String): BufferedWriter = 
	new BufferedWriter(new FileWriter(new File(name) ))

def close(writer: BufferedWriter) {
	writer.flush()
	writer.close()
}

}

