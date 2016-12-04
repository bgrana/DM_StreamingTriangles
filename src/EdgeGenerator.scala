import java.util.Properties

import scala.io.Source
import scala.util.Random

import kafka.javaapi.producer.Producer
import kafka.producer.KeyedMessage
import kafka.producer.ProducerConfig

object EdgeGenerator {
  def main(args: Array[String]): Unit = {
    val props = new Properties()
    props.put("metadata.broker.list", "127.0.0.1:9092")
    props.put("serializer.class", "kafka.serializer.StringEncoder")
    props.put("request.required.acks", "1")

    val config = new ProducerConfig(props)
    val producer = new Producer[String, String](config)
    val topic = "edges"
    
    // Read file and shuffle
    val content = readFile("/home/adrian/Desktop/MASTER/DataMining/Homework3/moreno_zebra/out.moreno_zebra_zebra")
    val edges = content.split("\n")
          .filter(s => s.split(" ").length == 2)
          .map(s => (s.split(" ").apply(0).toInt, s.split(" ").apply(1).toInt))
          .map(t => (t._1.max(t._2), t._1.min(t._2)))
    val shuffled = Random.shuffle(edges.toBuffer)

    // Send next edge
    for (edge <- shuffled) {
      Thread.sleep(100)
      producer.send(new KeyedMessage[String, String](topic, null,
                            edge._1.toString() + " " + edge._2.toString()))
      println("next edge: " + edge.toString())
    }

    producer.close   
  }

  def readFile(dir: String): String = {
    Source.fromFile(dir).mkString
  }
}

