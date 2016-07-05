import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.neo4j.graphdb._
import org.neo4j.graphdb.index._

import org.neo4j.graphdb.factory._
import scala.collection.JavaConverters._

object ObservedBy extends RelationshipType {
  def name: String = "OBSERVED_BY"
}

object MOVIE extends Label {
  def name = "Movie"
}
object USER extends Label {
  def name = "Movie"
}

trait Observation {
  def time: Long
}
trait Locator extends Observation {
  def lat: Double
  def lon: Double
}
trait SelfLocator extends Locator {
  def speed: Double
  def heading: Double
  def vesselName: String
}
case class DataSource(name: String)
case class AIS(time: Long, lat: Double, lon: Double, speed: Double, heading: Double, vesselName: String, flag: String, purpose: String) extends SelfLocator
case class BlueforceSelfLocator(time: Long, lat: Double, lon: Double, speed: Double, heading: Double, vesselName: String) extends SelfLocator
case class TargetObservation(time: Long, lat: Double, lon: Double, standardError: Double, source: String) extends Locator
case class Vessel(name: String, flag: String, displacement: Double)

/**
 * Ingesters are an important part of what's here. Whenever an element is added
 * to the KB, its "type" property is inspected. There is a set of ingesters that
 * are registered with the KB by type. Currently we only support a single string
 * in the type property, but it would not be hard to support arrays of string.
 * Associated with certain type strings is a list of functions that operate on 
 * the newly added node. These do things like add it to indices. 
 *
 *
 * Issues: How to make adding to indices less painful. What if, post-deployment,
 * we add an index file? We've addressed that problem somewhat through use of 
 * the things we call "ingesters", which are basically functions that are called
 * when an observation node is added to the KB. But it's a bit sloppy.
 */
object FakeDB extends LazyLogging {
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
  
  /**
   * Can throw exceptions.
   */
  def addDataSource(database: GraphDatabaseService, dataSource: DataSource): Long = {
    val tx = database.beginTx
    try {
      val newSource = database.createNode
      newSource.setProperty("timestamp", new java.util.Date().getTime)
      newSource.setProperty("type", "DataSource")
      newSource.setProperty("name", dataSource.name) 
      ingest(newSource)
      tx.success
      logger.info("Added data source " + dataSource.name)
      newSource.getId
    } finally {
      tx.close
    }
  }
  
  def addVessel(database: GraphDatabaseService, vessel: Vessel): Long = {
    val tx = database.beginTx
    try {
      val newVessel = database.createNode
      newVessel.setProperty("timestamp", new java.util.Date().getTime)
      newVessel.setProperty("type", "Vessel")
      newVessel.setProperty("name", vessel.name)
      newVessel.setProperty("flag", vessel.flag)
      newVessel.setProperty("displacement", vessel.displacement) 
      ingest(newVessel)
      tx.success
      logger.info("Added vessel " + vessel.name)
      newVessel.getId
    } finally {
      tx.close
    }
  }
  
  def addAIS(database: GraphDatabaseService, report: AIS): Long = {
    val tx = database.beginTx
    try {
      val observation = database.createNode
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
      logger.info("Added AIS report for vessel " + report.vesselName)
      observation.getId
    } finally {
      tx.close
    }
  }

  def addBlueforceSelfLocator(database: GraphDatabaseService, report: BlueforceSelfLocator):Long = {
    val tx = database.beginTx
    try {
      val observation = database.createNode
      //observation.setProperty("source", report.source)  //  No! not a property, a relation.
      observation.setProperty("timestamp", new java.util.Date().getTime)
      observation.setProperty("type", "BFR")
      observation.setProperty("time", report.time)
      observation.setProperty("lat", report.lat)
      observation.setProperty("lon", report.lon)
      observation.setProperty("speed", report.speed)
      observation.setProperty("heading", report.heading)
      observation.setProperty("vesselName", report.vesselName)
      ingest(observation)
      tx.success
      logger.info("Added blue force self-report for vessel " + report.vesselName)
      observation.getId
    } finally {
      tx.close
    }
  }
  
  def addTargetObservation(database: GraphDatabaseService, report: TargetObservation): Long = {
    val tx = database.beginTx
    try {
      val observation = database.createNode
      observation.setProperty("source", report.source)  //  No! Not a property, a relation.
      observation.setProperty("timestamp", new java.util.Date().getTime)      
      observation.setProperty("type", "TargetObservation")
      observation.setProperty("time", report.time)
      observation.setProperty("lat", report.lat)
      observation.setProperty("lon", report.lon)
      observation.setProperty("standardError", report.standardError)
      ingest(observation)
      tx.success
      logger.info("Added target observation -- Lat:" + report.lat + "  Lon:" + report.lon + "  Time:" + new java.util.Date(report.time))
      observation.getId
    } finally {
      tx.close
    }
  }
  
  import scala.collection.JavaConverters._
  /***
   * This is the main simulation. 
   */
  def main(args: Array[String]): Unit = {
    val EndedInError = -1
    
    // For safety, let's make sure to purge any old copies of the database.
    deleteFileOrDirectory(new java.io.File(DbLocation))
    
    // Create (or open) the database, and establish the indices.
    val graphDb = new GraphDatabaseFactory().newEmbeddedDatabase(new java.io.File(DbLocation))
    val indexManager: IndexManager = graphDb.index
    lazy val vesselIndex: Index[Node] = {
        val txIndices = graphDb.beginTx
        try {
          indexManager.forNodes("vessels")
        } catch {
          case ex: Exception => logger.error("Couldn't create index for 'vessels'")
          System.exit(1)
          null
        } finally {
          txIndices.close
        }
      }
    lazy val sourceIndex: Index[Node] = {
        val txIndices = graphDb.beginTx
        try {
          indexManager.forNodes("datasources")
        } catch {
          case ex: Exception => logger.error("Couldn't create index for 'sources'")
          System.exit(1)
          null
        } finally {
          txIndices.close
        }
      }
    
    // Ingesters
    val fIngestVessel = (node:Node) => indexManager.forNodes("vessels").add(node, "name", node.getProperty("name"))
    ingesters.get("Vessel") match {
        case Some(functions) => ingesters.put("Vessel", fIngestVessel::functions)
        case None            => ingesters.put("Vessel", List(fIngestVessel))     
      }
    val fIngestDataSource = (node:Node) => indexManager.forNodes("datasources").add(node, "name", node.getProperty("name"))
    ingesters.get("DataSource") match {
        case Some(functions) => ingesters.put("DataSource", fIngestDataSource::functions)
        case None            => ingesters.put("DataSource", List(fIngestDataSource))     
      }
    val fRelateAIStoSource = (node:Node) => {              //  Adds relationship between AIS observation
        val index = indexManager.forNodes("datasources")   //  and the AIS data source.  
        node.createRelationshipTo(index.get("name", "AIS").getSingle, ObservedBy)
        ()                                                 //  Returns value of type Unit
      }
    ingesters.get("AIS") match {
        case Some(functions) => ingesters.put("AIS", fRelateAIStoSource::functions)
        case None            => ingesters.put("AIS", List(fRelateAIStoSource))     
      }
    val fRelateBFRtoSource = (node:Node) => {              //  Adds relationship between blue force observation
        val index = indexManager.forNodes("datasources")   //  and the AIS data source.  
        node.createRelationshipTo(index.get("name", "BFR").getSingle, ObservedBy)
        ()                                                 //  Returns value of type Unit
      }
    ingesters.get("BFR") match {
        case Some(functions) => ingesters.put("BFR", fRelateBFRtoSource::functions)
        case None            => ingesters.put("BFR", List(fRelateBFRtoSource))     
      }
     val fRelateSensorObsToSource = (node:Node) => {       //  Adds relationship between sensor observation
        val index = indexManager.forNodes("datasources")   //  and the sensor data source.
        node.createRelationshipTo(index.get("name", node.getProperties("source").get("source")).getSingle, ObservedBy)
        println("Linked measurement to sensor source")
        ()                                                 //  Returns value of type Unit
      }
    ingesters.get("TargetObservation") match {
        case Some(functions) => ingesters.put("TargetObservation", fRelateSensorObsToSource::functions)
        case None            => ingesters.put("TargetObservation", List(fRelateSensorObsToSource))     
      }
    
    
    // There are a couple of data sources in the scenario. So add them.
    try {
      addDataSource(graphDb, DataSource("Hawkeye"))
      addDataSource(graphDb, DataSource("AIS"))
      addDataSource(graphDb, DataSource("BFR"))
    } catch {
      case ex: Exception => logger.error("Error while adding data source: " + ex.getMessage)
    }
    //  Pull the source IDs from the index (for convenience).
    val aisSourceId: Long = {
        val tx = graphDb.beginTx; try {
          val index = indexManager.forNodes("datasources")
          val id = index.get("name", "AIS").getSingle.getId
          tx.success
          id
        } catch {
          case ex: Exception => logger.error("Couldn't pull the AIS source from the data sources index" + ex.getMessage)
          System.exit(1)
          EndedInError
        } finally {
          tx.close
        }
      }
    val hawkeyeId: Long = {
        val tx = graphDb.beginTx; try {
          val index = indexManager.forNodes("datasources")
          val id = index.get("name", "Hawkeye").getSingle.getId
          tx.success
          id
        } catch {
          case ex: Exception => logger.error("Couldn't pull the Hawkeye source from the data sources index" + ex.getMessage)
          System.exit(1)
          EndedInError
        } finally {
          tx.close
        }      
      }
    println("Hawkeye ID = " + hawkeyeId)
    
    // Add a couple of vessels to the database
    val schotyId = try {
      addVessel(graphDb, Vessel("Schoty", "Russia", 74))
    } catch {
      case ex: Exception => logger.error("Couldn't add Schoty: " + ex.getMessage)
                            System.exit(0)
    }
    val bunkerHillId = try {
      addVessel(graphDb, Vessel("Bunker Hill", "US", 27100))
    } catch { 
      case ex: Exception => logger.error("Couldn't add Bunker Hill: " + ex.getMessage)
    }
  
    ////////////////////////////////////////////////////////////////////////////
    // Now for the scenario.
    ////////////////////////////////////////////////////////////////////////////
    
    // We get a couple of AIS messages from the Schoty about a half hour apart
    addAIS(graphDb, AIS(time(2016,6,8,0,15,21,0), 14.624465, 50.164166, 10.1, 261.0, "Schoty", "Russia", "Fishing"))
    addAIS(graphDb, AIS(time(2016,6,8,0,45,21,0), 14.605866, 50.018592, 10.2, 260.0, "Schoty", "Russia", "Fishing"))
    
    // We get a blue force self-location report from the Bunker Hill
    addBlueforceSelfLocator(graphDb, BlueforceSelfLocator(time(2016,6,8,1,9,34,0), 14.610000, 50.070000, 6.4, 87.0, "Bunker Hill"))
    
    // We get a message from an E-2D Hawkeye.
    addTargetObservation(graphDb, TargetObservation(time(2016,6,8,1,1,36,478), 14.613180, 49.956102, 131.2, "Hawkeye"))   
    
    logger.info("The neo4j id of the Schoty is: " + schotyId + "  and the id for BunkerHill is: " + bunkerHillId)
    
    //  Now let's find the Schoty using the vessel index.
    val tx = graphDb.beginTx; try {
      val index = indexManager.forNodes("vessels")
      index.get("name", "Schoty").iterator.asScala.foreach(vessel => logger.info("Vessel Schoty with flag: " + vessel.getProperty("flag")))      
      tx.success
    } catch {
      case ex: Exception => logger.warn("Exception finding vessel")
    } finally {
      tx.close  
    }
    
    //addAIS(graphDb, AIS(time(2016,7,8,0,45,21,0), 14.624465, 50.164166, 10.1, 261.0, "Schoty", "Russia", "Fishing"))
    //addAIS(graphDb, AIS(time(2016,7,8,0,45,21,0), 14.605866, 50.018592, 10.2, 260.0, "Schoty", "Russia", "Fishing"))    
  }
}