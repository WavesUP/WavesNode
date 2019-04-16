package com.wavesplatform.network

import java.util

import com.wavesplatform.settings._
import com.wavesplatform.state.{BlockAdded, BlockchainUpdated, MicroBlockAdded, RollbackCompleted}
import monix.execution.{Ack, Scheduler}
import monix.execution.Ack.Continue
import monix.reactive.{Observable, Observer}
import com.wavesplatform.protobuf.events.PBEvents
import com.wavesplatform.utils.ScorexLogging
import org.apache.kafka.common.serialization.{IntegerSerializer, Serializer}
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerConfig, ProducerRecord}

private object BlockchainUpdatedSerializer extends Serializer[BlockchainUpdated] {
  override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
  override def close(): Unit                                                 = {}

  override def serialize(topic: String, data: BlockchainUpdated): Array[Byte] =
    PBEvents.protobuf(data).toByteArray
}

private object IntSerializer extends Serializer[Int] {
  val integerSerializer = new IntegerSerializer

  override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = integerSerializer.configure(configs, isKey)
  override def close(): Unit                                                 = integerSerializer.close()

  override def serialize(topic: String, data: Int): Array[Byte] =
    integerSerializer.serialize(topic, data)
}

class BlockchainUpdatesSender(settings: WavesSettings, blockchainUpdated: Observable[BlockchainUpdated])(implicit scheduler: Scheduler)
    extends ScorexLogging {

  // @todo check on startup if Kafka is available?
  private[this] val producer = new KafkaProducer[Int, BlockchainUpdated](createProperties(), IntSerializer, BlockchainUpdatedSerializer)

  blockchainUpdated.subscribe(new Observer.Sync[BlockchainUpdated] {
    override def onNext(evt: BlockchainUpdated): Ack = {
      // @todo guarantees/backpressure
      producer.send(createProducerRecord(evt))
      Continue
    }

    override def onError(ex: Throwable): Unit = {
      log.error("Error sending blockchain updates", ex)
//      shutdown()
    }

    override def onComplete(): Unit = {
      // @todo proper complete/shutdown actions
      log.info("Blockchain updates channel closed")
//      shutdown()
    }
  })(scheduler)

  private[this] def createProperties(): util.Properties = {
    val props = new util.Properties()
    props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, settings.blockchainUpdatesSettings.bootstrapServers)
    props.put(ProducerConfig.CLIENT_ID_CONFIG, settings.blockchainUpdatesSettings.clientId)
    //  props.put(ProducerConfig.RETRIES_CONFIG, "0")
    props.put(ProducerConfig.ACKS_CONFIG, "all")
    props
  }

  private[this] def createProducerRecord(event: BlockchainUpdated): ProducerRecord[Int, BlockchainUpdated] = {
    val h = event match {
      case BlockAdded(_, height, _, _)      => height
      case MicroBlockAdded(_, height, _, _) => height
      case RollbackCompleted(_, height)     => height
    }
    new ProducerRecord[Int, BlockchainUpdated](settings.blockchainUpdatesSettings.topic, h, event)
  }

  def shutdown(): Unit =
    producer.close()
}
