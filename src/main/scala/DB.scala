import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.neo4j.graphdb._
import org.neo4j.graphdb.index._

import org.neo4j.graphdb.factory._
import scala.collection.JavaConverters._

object ObservedBy extends RelationshipType {
  def name: String = "OBSERVED_BY"
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
object DB extends LazyLogging {
  // Constructorish stuff.
  private val DbLocation = "/tmp/neoDb4j/"
  val ingesters = new scala.collection.mutable.HashMap[String, List[(Node=>Unit)]] 
  deleteFileOrDirectory(new java.io.File(DbLocation))
  logger.info("Graph database main storage file deleted at " + DbLocation)
  val graphDb = new GraphDatabaseFactory().newEmbeddedDatabase(new java.io.File(DbLocation))
  logger.info("Graph database main storage file opened at " + DbLocation)
  // Methodish stuff.
  
  private def deleteFileOrDirectory(file: File): Unit = {
    if (file.exists) {
      if (file.isDirectory) file.listFiles.map(child => deleteFileOrDirectory(child))
      file.delete
    }
  }
  
  /**
   * Issues: Must be run from within a transaction.
   */
  def ingest(node: Node) = {
    val nodeType = node.getProperty("type").toString
    ingesters.get(nodeType) match {
      case Some(functionList) => functionList.map(_(node))
      case None =>
    }   
  }
  
  def addIngester(typeAssociation: String, function: (Node=>Unit)) = {
    ingesters.get(typeAssociation) match {
        case Some(functions) => DB.ingesters.put(typeAssociation, function::functions)
        case None            => DB.ingesters.put(typeAssociation, List(function))     
      }    
  }
  
  /**
   * Can throw exceptions.
   */
  def addDataSource(dataSource: DataSource): Long = {
    val tx = graphDb.beginTx
    try {
      val newSource = graphDb.createNode
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
  
  def addVessel(vessel: Vessel): Long = {
    val tx = graphDb.beginTx
    try {
      val newVessel = graphDb.createNode
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
  
  def addAIS(report: AIS): Long = {
    val tx = graphDb.beginTx
    try {
      val observation = graphDb.createNode
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

  def addBlueforceSelfLocator(report: BlueforceSelfLocator):Long = {
    val tx = graphDb.beginTx
    try {
      val observation = graphDb.createNode
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
  
  def addTargetObservation(report: TargetObservation): Long = {
    val tx = graphDb.beginTx
    try {
      val observation = graphDb.createNode
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
}
  
