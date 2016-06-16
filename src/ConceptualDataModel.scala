/**
 * @author Jamie Lawson
 * @author Gene Hubbard
 * @author Rajdeep Singh
 */
import java.util.UUID
import scala.collection.immutable.{Map,List}
import scala.collection.Iterable

/**
 * Entries are the things that the compose the data model. Entries can be typed or untyped. 
 * 
 * Issues:
 * 1) We use a UUID here as a surrogate for a TrueNumber. We need to work through the TrueNumber
 *    stuff, but one of the things we know is that True talks about TrueNumbers in applications
 *    to observables (objects). Does any of that break down when we get to activities or other
 *    kinds of unobservable entities that we haven't yet thought of? There is still pedigree. We
 *    believe that there is an activity because of the apparent coordinated states of observables.
 *    But what makes that coordination "apparent" is the the action of some algorithms or human 
 *    insight. Can we assign a TrueNumber to that human insight?  
 */
trait Entry {
  // Metadata
  def id: UUID                                      //  Stand-in for the TrueNumber
  def source: UUID                                  //  Author of the entry
  def references: List[UUID]                        //  The ids of the things that went into this entry. More provenance.
  def typehints: List[String]                       //  Indication of how this entry might be cast. A hint, not a declaration.
  
  // Contents
  def value(name: String): Any                      //  A lot like JSON.
  
  // Syntactic sugar
  def apply(name: String): Any = value(name)
  
  // General matching
  def matchID(id:UUID) = this.id == id
  def matchType(typeName: String) = typehints contains typeName
  def matchContents(contents: String) = this.toString contains contents
}

/**
 * Untyped entries are building blocks. They can express just about anything, but are hard to index on.
 */
class UntypedEntry(sourcev: UUID, referencesv: List[UUID], values: Map[String,Any], hints: List[String]=Nil) extends Entry {
  val id = UUID.randomUUID
  val source = sourcev
  val references = referencesv
  val typehints = hints
  def value(name: String) = values(name)
  override def toString = "id:" + id.toString + " source:" + source.toString + " references:" + references.toString +
      " typehints:" + typehints.toString + " values:" + values.toString
} 
object UntypedEntry {
  def apply(source: UUID, references: List[UUID], values: Map[String,Any], hints: List[String]=Nil) = new UntypedEntry(source, references, values, hints)
}

/**
 * Typed entries are more rigid, but they have semantics, so indexing with them makes sense.
 * 
 * Issues:
 * 1) The typed entries are not equivalents to the untyped entries. There needs to be a way to 
 *    manage the chain so that the source of the typed entry is the untyped entity and not the 
 *    same as the source of the untyped entity.
 * 2) Identifying comparison semantics. Some entities can be different but not greater than or
 *    less than some other entry. Nominal data types come to mind: red, green, blue. We may need
 *    a way to distinguish ordered types from unordered types. Java's Comparable interface comes
 *    to mind. So maybe we have ComparableTypedEntry as a subtype.
 */
trait TypedEntry extends Entry {
  val id = UUID.randomUUID
  val typehints = Nil
}

/**
 * Indices are central to querying. In essence, an index anticipates computation that will
 * have to be done at some point, and does that computation in advance to reduce query latency.
 * Indices exploit type semantics regarding equivalence or proximity. Often that can be very
 * abstract, like sentiment, or an amorphous network (say, piracy in the Gulf of Adan; who
 * are the members of that network and what are their roles?), etc.
 * 
 * Issues:
 * 1) Indexing a set of entries (rather than just one)
 * 2) Removing an entry
 * 3) Updating an entry
 * 4) Getting a better sense of query. That is,
 *      a) Do range queries make sense in a particular index?
 *      b) Do fuzzy queries make sense in a particular index?
 *      c) For some graph-based indices, we probably want to be able to query for 
 *         things related to the key, a "six degrees of Kevin Bacon" query.
 *    Moreover, is there a way to abstract all of this so that we get the control it
 *    would provide without having to specify the query types beforehand. 
 */
trait Index {
  def id: UUID
  def add(entry: Entry)                         //  Incorporate an entry (typed) into the index.
  def upcast(entry: Entry): Option[TypedEntry]  //  Interpret an untyped entry as a typed entry
  def matchQuery(key: Any): Option[TypedEntry]  //  Artificially narrow query result.
  def name: String                              //  What we call the index.
  def description: String                       //  Partial solution to problem of introspecting on indices
}

/**
 * Mockup of a Knowledge Base which is a largely unstructured set of (untyped) entries
 * and a collection of indices. You can add new indices and when entries are added, the
 * type information is extracted and added to the indices.
 * 
 * Issues:
 * 1) When we add an index we need to be able to index from all of the existing entries. 
 * 2) When we remove an entry, we need to remove it from indices. 
 * 3) When we update an entry, we need to update it in all indices.
 * 4) All three of the prior issues raise other issues of consistency, i.e. making sure
 *    that everything gets added to a new index and that everything gets removed when
 *    called to remove.
 * 5) Need to be able to add entries to indices ex post facto when a user figures out 
 *    what the thing is; when an unknown unknown becomes a known unknown or a known known. 
 * 6) Greater ability to introspect on indices. 
 * 7) What about data types built from the basic data types? These will apply to activities
 *    as the activity isn't observable except through the objects that compose it.
 */
class KB {
  //  Things that store stuff
  private val database = new scala.collection.mutable.ListMap[UUID, Entry]
  private val indicesByID = new scala.collection.mutable.ListMap[UUID, Index]
  private val indicesByName = new scala.collection.mutable.ListMap[String, Index]
  
  //  Adding and removing entries and indices
  def addEntry(entry: Entry) = {
    database.put(entry.id, entry)
    index(entry)
  }
  def removeEntry(id: UUID) = database - id
  def index(entry: Entry) = {
    for (index <-indicesByID.values) {
      index.upcast(entry) match {
        case Some(typedEntry) => index.add(typedEntry)
        case None =>
      }
    }   
  }
   
  def addIndex(index: Index) = {
    indicesByID += (index.id -> index)
    indicesByName += (index.name -> index)
    database.values.map(index.upcast(_) match {
      case Some(typedEntry) => index.add(typedEntry)
      case None =>
    })
  }
  def removeIndex(id: UUID) = indicesByID - id
  
  //  Query and introspection
  def findEntry(key: String): List[Entry] = database.values.filter(_ matchContents key).toList
  def findEntry(key: UUID): Option[Entry] = database get key
  
  def indexNames: Iterable[String] = indicesByName.keys
  def findIndex(key: String): Option[Index] = indicesByName get key
  def findIndex(key: UUID): Option[Index] = indicesByID get key
  def searchIndices(key: String): List[Index] = indicesByName.values.filter(_.description contains key).toList
}


///////////////////////////////////////////////////////////////////////////////
// This stuff just exercises the structures.
//
// Person is a kind of typed entry and PersonIndex is an (overly simplified)
// way to index on people.
///////////////////////////////////////////////////////////////////////////////
class Person(firstv: String, lastv: String, agev: Int, sourcev: UUID, referencesv: List[UUID]) extends TypedEntry {
  def first = firstv
  def last = lastv
  def age = agev
  def source = sourcev
  def references = referencesv
  def value(name: String) = None
  override def toString = first + " " + last + " Age: " + age
}
object Person {
  def apply(entry: Entry) = {
    entry.typehints.contains("Person") match {
      case true =>
        val age = entry("Age").toString.toInt
        new Person(entry("First").toString, entry("Last").toString, entry("Age").toString.toInt, entry.id, List(entry.id))
      case false =>
        val tokens = entry("Contents").toString.split(" ")
        new Person(tokens(0), tokens(1), tokens(2).toInt, entry.id, List(entry.id))
    }
  }
}

class PersonIndex extends Index {
  private val elements = new scala.collection.mutable.HashMap[String, Person]
  val id = UUID.randomUUID
  
  def add(entry: Entry) = {
    entry.isInstanceOf[Person] match {
      case true =>
        val person = entry.asInstanceOf[Person]
        elements.put(person.last, person)
      case _ =>
    }   
  }
  
  def upcast(entry: Entry): Option[TypedEntry] = {
    entry.isInstanceOf[Person] match {
      case true => Some(entry.asInstanceOf[Person])
      case false => 
        entry.typehints.contains("Person") match {
          case true =>
            Some(new Person(entry("First").toString, entry("Last").toString, entry("Age").toString.toInt, entry.source, List(entry.id)))
          case false => None
        }
    }
  }  
  
  def matchQuery(key: Any): Option[TypedEntry] = {
    elements.get(key.toString) 
  }
  
  def description = "Organizes people by last name"
  def name = "People"
}


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
