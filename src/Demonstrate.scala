/**
 * @author Jamie Lawson
 * @author Gene Hubbard
 * @author Rajdeep Singh
 */
import java.util.UUID
import com.s3.cdm._

object ConceptualDataModel extends App {
  val myID = UUID.randomUUID
  
  val kb = new KB
  val people = new PersonIndex
  kb.addIndex(people)
  
  val e1 = UntypedEntry(myID, Nil, Map("First"->"Robin", "Last"->"Williams","Age"-> 62), List("Person"))
  kb.addEntry(e1)
  
  println("Try a general untyped query of the database")
  for (entry <- kb.findEntry("Williams")) println("   General match: " + entry)
  
  println("Try a match query")
  kb.findIndex("People") match {
    case Some(index) => index.matchQuery("Williams") match {
        case Some(person) => println("   " + person)
        case None => println("Williams NOT FOUND")     
      }
    case None => println("No People index")
  }
  
  val e2 = UntypedEntry(myID, Nil, Map("Contents"->"Alexander Hamilton 270"), Nil)
  val p2 = Person(e2)
  println("Age=" + p2.age)
}
