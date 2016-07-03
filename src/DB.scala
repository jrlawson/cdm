import java.io.File
import org.neo4j.graphdb._
import org.neo4j.graphdb.index._

import org.neo4j.graphdb.factory._
import org.neo4j.tooling._
import scala.collection.JavaConverters._

object IsFriendOf extends RelationshipType {
  def name: String = "IS_FRIEND_OF"
}

object HasSeen extends RelationshipType {
  def name: String = "HAS_SEEN"
}

object MOVIE extends Label {
  def name = "Movie"
}
object USER extends Label {
  def name = "Movie"
}

trait Locator {
  def time: Long
  def lat: Double
  def lon: Double
}
trait SelfLocator extends Locator {
  def time: Long
  def lat: Double
  def lon: Double
  def speed: Double
  def heading: Double
  def vesselName: String
}
case class AIS(time: Long, lat: Double, lon: Double, speed: Double, heading: Double, vesselName: String, flag: String, purpose: String) extends SelfLocator
case class BlueforceSelfLocator(time: Long, lat: Double, lon: Double, speed: Double, heading: Double, vesselName: String) extends SelfLocator
case class TargetObservation(time: Long, lat: Double, lon: Double, standardError: Double)
case class Vessel(name: String, flag: String, displacement: Double)

/**
 * Issues: How to make adding to indices less painful. What if, post-deployment,
 * we add an index file? We've addressed that problem somewhat through use of 
 * the things we call "ingesters", which are basically functions that are called
 * when an observation node is added to the KB. But it's a bit sloppy.
 */
object FakeDB {
  private val DbLocation = "/tmp/neoDb4j/"
  private val ZuluTime = java.util.TimeZone.getTimeZone("Zulu")
  private val calendar = new java.util.GregorianCalendar(ZuluTime)
  private val ingesters = new scala.collection.mutable.HashMap[String, List[(Node=>Unit)]] 
  
  private def deleteFileOrDirectory(file: File): Unit = {
    if (file.exists) {
      if (file.isDirectory) file.listFiles.map(child => deleteFileOrDirectory(child))
      file.delete
    }
  }
  
  private def time(year: Int, month: Int, day: Int, hour: Int, min: Int, sec: Int, millis: Int): Long = {
    calendar.set(year, month, day, hour, min, sec)
    new java.util.Date(calendar.getTime.getTime + millis).getTime
  }
  
  /**
   * Issues: Must be run from within a transaction.
   */
  private def ingest(node: Node) = {
    val nodeType = node.getProperty("type").toString
    ingesters.get(nodeType) match {
      case Some(functionList) => functionList.map(_(node))
      case None =>
    }   
  }
  
  def addVessel(database: GraphDatabaseService, vessel: Vessel): Long = {
    var id = 0L
    val tx = database.beginTx; try {
      val newVessel = database.createNode
      id = newVessel.getId
      newVessel.setProperty("timestamp", new java.util.Date().getTime)
      newVessel.setProperty("type", "Vessel")
      newVessel.setProperty("name", vessel.name)
      newVessel.setProperty("flag", vessel.flag)
      newVessel.setProperty("displacement", vessel.displacement)
      
      ingest(newVessel)
      tx.success
      println("Added vessel " + vessel.name)
    } catch {
      case ex: Exception => println("Exception thrown: " + ex.getMessage)
    } finally {
      tx.close
    }
    id
  }
  
  def addAIS(database: GraphDatabaseService, report: AIS): Long = {
    var id = 0L
    val tx = database.beginTx; try {
      val observation = database.createNode
      id = observation.getId
      observation.setProperty("timestamp", new java.util.Date().getTime)
      observation.setProperty("type", "AIS")
      observation.setProperty("time", report.time)
      observation.setProperty("lat", report.lat)
      observation.setProperty("lon", report.lon)
      observation.setProperty("speed", report.speed)
      observation.setProperty("heading", report.heading)
      observation.setProperty("vesselName", report.vesselName)
      observation.setProperty("flag", report.flag)
      observation.setProperty("purpose", report.purpose)
      ingest(observation)
      tx.success
      println("Added AIS report for vessel " + report.vesselName)
    } catch {
      case ex: Exception => println("Exception thrown")
    } finally {
      tx.close
    }
    id
  }

  def addBlueforceSelfLocator(database: GraphDatabaseService, report: BlueforceSelfLocator):Long = {
    var id = 0L
    val tx = database.beginTx; try {
      val observation = database.createNode
      id = observation.getId
      observation.setProperty("timestamp", new java.util.Date().getTime)
      observation.setProperty("type", "AIS")
      observation.setProperty("time", report.time)
      observation.setProperty("lat", report.lat)
      observation.setProperty("lon", report.lon)
      observation.setProperty("speed", report.speed)
      observation.setProperty("heading", report.heading)
      observation.setProperty("vesselName", report.vesselName)
      ingest(observation)
      tx.success
      println("Added blue force self-report for vessel " + report.vesselName)
    } catch {
      case ex: Exception => println("Exception thrown")
    } finally {
      tx.close
    }
    id
  }
  
  def addTargetObservation(database: GraphDatabaseService, report: TargetObservation): Long = {
    var id = 0L
    val tx = database.beginTx; try {
      val observation = database.createNode
      id = observation.getId
      observation.setProperty("timestamp", new java.util.Date().getTime)      
      observation.setProperty("type", "TargetObservation")
      observation.setProperty("time", report.time)
      observation.setProperty("lat", report.lat)
      observation.setProperty("lon", report.lon)
      observation.setProperty("standardError", report.standardError)
      ingest(observation)
      tx.success
      println("Added target observation -- Lat:" + report.lat + "  Lon:" + report.lon + "  Time:" + new java.util.Date(report.time))
    } catch {
      case ex: Exception => println("Exception thrown")
    } finally {
      tx.close
    }
    id
  }
  
  import scala.collection.JavaConverters._
  /***
   * This is the main simulation. 
   */
  def main(args: Array[String]): Unit = {
    // For safety, let's make sure to purge any old copies of the database.
    deleteFileOrDirectory(new java.io.File(DbLocation))
    
    // Create (or open) the database, and establish the indices.
    val graphDb = new GraphDatabaseFactory().newEmbeddedDatabase(new java.io.File(DbLocation))
    val indexManager: IndexManager = graphDb.index
    lazy val vesselIndex: Index[Node] = {
        val txIndices = graphDb.beginTx; try {
          indexManager.forNodes("vessels")
        } catch {
          case ex: Exception => println("Couldn't create index for 'vessels'")
          System.exit(1)
          null
        } finally {
          txIndices.close
        }
      }
    val fIngestVessel = (node:Node) => indexManager.forNodes("vessels").add(node, "name", node.getProperty("name"))
    ingesters.get("Vessel") match {
        case Some(functions) => ingesters.put("Vessel", fIngestVessel::functions)
        case None            => ingesters.put("Vessel", List(fIngestVessel))     
      }
    
    // There are a couple of vessels in the scenario. So add them.
    val schotyID = addVessel(graphDb, Vessel("Schoty", "Russia", 74))
    val bunkerHillID = addVessel(graphDb, Vessel("Bunker Hill", "US", 27100))
    
    // We get a couple of AIS messages from the Schoty about a half hour apart
    addAIS(graphDb, AIS(time(2016,6,8,0,15,21,0), 14.624465, 50.164166, 10.1, 261.0, "Schoty", "Russia", "Fishing"))
    addAIS(graphDb, AIS(time(2016,6,8,0,45,21,0), 14.605866, 50.018592, 10.2, 260.0, "Schoty", "Russia", "Fishing"))
    
    // We get a message from an E-2D Hawkeye.
    addTargetObservation(graphDb, TargetObservation(time(2016,6,8,1,1,36,478), 14.613180, 49.956102, 131.2))   
    
    println("The neo4j id of the Schoty is: " + schotyID + "  and the id for BunkerHill is: " + bunkerHillID)
    
    //  Now let's find the Schoty using the vessel index.
    val tx = graphDb.beginTx; try {
      val index = indexManager.forNodes("vessels")
      println("--------------------------------------------")
      index.get("name", "Schoty").iterator.asScala.foreach(vessel => println(vessel.getProperty("flag")))
      println("--------------------------------------------")      
      tx.success
    } catch {
      case ex: Exception => println("Exception finding vessel")
    } finally {
      tx.close  
    }
    
    //addAIS(graphDb, AIS(time(2016,7,8,0,45,21,0), 14.624465, 50.164166, 10.1, 261.0, "Schoty", "Russia", "Fishing"))
    //addAIS(graphDb, AIS(time(2016,7,8,0,45,21,0), 14.605866, 50.018592, 10.2, 260.0, "Schoty", "Russia", "Fishing"))    
  }
}