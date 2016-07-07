import scala.collection.JavaConverters._
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.neo4j.graphdb._
import org.neo4j.graphdb.index._

/***
 * This is the main simulation. 
 */
object Demo extends LazyLogging {
  private val ZuluTime = java.util.TimeZone.getTimeZone("Zulu")
  private val calendar = new java.util.GregorianCalendar(ZuluTime)
  private def time(year: Int, month: Int, day: Int, hour: Int, min: Int, sec: Int, millis: Int): Long = {
    calendar.set(year, month, day, hour, min, sec)
    new java.util.Date(calendar.getTime.getTime + millis).getTime
  }
  def main(args: Array[String]): Unit = {
    val EndedInError = -1
    
    // For safety, let's make sure to purge any old copies of the database.
    //deleteFileOrDirectory(new java.io.File(DbLocation))
    
    // Create (or open) the database, and establish the indices.
    val indexManager: IndexManager = DB.graphDb.index
    lazy val vesselIndex: Index[Node] = {
        val txIndices = DB.graphDb.beginTx
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
        val txIndices = DB.graphDb.beginTx
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
    val fIngestVessel = 
    DB.addIngester("Vessel", (node:Node) => indexManager.forNodes("vessels").add(node, "name", node.getProperty("name")))
    DB.addIngester("DataSource", (node:Node) => indexManager.forNodes("datasources").add(node, "name", node.getProperty("name")))
    DB.addIngester("AIS", (node:Node) => {                 //  Adds relationship between AIS observation
        val index = indexManager.forNodes("datasources")   //  and the AIS data source.  
        node.createRelationshipTo(index.get("name", "AIS").getSingle, ObservedBy)
        ()                                                 //  Returns value of type Unit
      })
    DB.addIngester("BFR", (node:Node) => {                 //  Adds relationship between blue force observation
        val index = indexManager.forNodes("datasources")   //  and the AIS data source.  
        node.createRelationshipTo(index.get("name", "BFR").getSingle, ObservedBy)
        ()                                                 //  Returns value of type Unit
      })
    DB.addIngester("TargetObservation", (node:Node) => {   //  Adds relationship between sensor observation
        val index = indexManager.forNodes("datasources")   //  and the sensor data source.
        node.createRelationshipTo(index.get("name", node.getProperties("source").get("source")).getSingle, ObservedBy)
        println("Linked measurement to sensor source")
        ()                                                 //  Returns value of type Unit
      })
    
    
    // There are a couple of data sources in the scenario. So add them.
    try {
      DB.addDataSource(DataSource("Hawkeye"))
      DB.addDataSource(DataSource("AIS"))
      DB.addDataSource(DataSource("BFR"))
    } catch {
      case ex: Exception => logger.error("Error while adding data source: " + ex.getMessage)
                            System.exit(0)
    }

    // Add a couple of vessels to the database
    val schotyId = try {
      DB.addVessel(Vessel("Schoty", "Russia", 74))
    } catch {
      case ex: Exception => logger.error("Couldn't add Schoty: " + ex.getMessage)
                            System.exit(0)
    }
    val bunkerHillId = try {
      DB.addVessel(Vessel("Bunker Hill", "US", 27100))
    } catch { 
      case ex: Exception => logger.error("Couldn't add Bunker Hill: " + ex.getMessage)
    }
  
    ////////////////////////////////////////////////////////////////////////////
    // Now for the scenario.
    ////////////////////////////////////////////////////////////////////////////
    
    // We get a couple of AIS messages from the Schoty about a half hour apart
    DB.addAIS(AIS(time(2016,6,8,0,15,21,0), 14.624465, 50.164166, 10.1, 261.0, "Schoty", "Russia", "Fishing"))
    DB.addAIS(AIS(time(2016,6,8,0,45,21,0), 14.605866, 50.018592, 10.2, 260.0, "Schoty", "Russia", "Fishing"))
    
    // We get a blue force self-location report from the Bunker Hill
    DB.addBlueforceSelfLocator(BlueforceSelfLocator(time(2016,6,8,1,9,34,0), 14.610000, 50.070000, 6.4, 87.0, "Bunker Hill"))
    
    // We get a message from an E-2D Hawkeye.
    DB.addTargetObservation(TargetObservation(time(2016,6,8,1,1,36,478), 14.613180, 49.956102, 131.2, "Hawkeye"))   
    
    logger.info("The KDB id of the Schoty is: " + schotyId + "  and the id for BunkerHill is: " + bunkerHillId)
    
    //  Now let's find the Schoty using the vessel index.
    val tx = DB.graphDb.beginTx; try {
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