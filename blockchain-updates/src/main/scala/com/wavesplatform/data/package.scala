package com.wavesplatform

import java.util
import java.util.Properties

import com.wavesplatform.events.protobuf.BlockchainUpdated
import org.apache.kafka.clients.consumer.{ConsumerConfig, KafkaConsumer}
import org.apache.kafka.common.serialization.{Deserializer, IntegerDeserializer}

package object data {
  private object IntDeserializer extends Deserializer[Int] {
    val integerDeserializer = new IntegerDeserializer

    override def configure(configs: util.Map[String, _], isKey: Boolean): Unit =
      integerDeserializer.configure(configs, isKey)
    override def close(): Unit = integerDeserializer.close()

    override def deserialize(topic: String, data: Array[Byte]): Int =
      integerDeserializer.deserialize(topic, data)
  }
  private object BlockchainUpdatedDeserializer
      extends Deserializer[BlockchainUpdated] {
    override def configure(configs: util.Map[String, _],
                           isKey: Boolean): Unit = {}
    override def close(): Unit = {}
    override def deserialize(topic: String,
                             data: Array[Byte]): BlockchainUpdated =
      BlockchainUpdated.parseFrom(data)
  }
  object Consumer {
    def create(): KafkaConsumer[Int, BlockchainUpdated] = {
      val props = new Properties()

      props.put(
        ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG,
        util.Arrays.asList(sys.env.getOrElse("VOLK_KAFKA", "kafka-dev.wvservices.com:9092"))
      )
      props.put(ConsumerConfig.GROUP_ID_CONFIG, "Volk s vol strit_transfers")
      props.put(ConsumerConfig.CLIENT_ID_CONFIG, "Volk s vol strit")
      props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false")
      props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "30000")
      props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "1000")
      //    if (settings.ssl.enabled) {
      //      props.put(CommonClientConfigs.SECURITY_PROTOCOL_CONFIG, "SASL_SSL")
      //      props.put(SaslConfigs.SASL_MECHANISM, "PLAIN")
      //      props.put(
      //        SaslConfigs.SASL_JAAS_CONFIG,
      //        s"org.apache.kafka.common.security.plain.PlainLoginModule required username = '${settings.ssl.username}' password = '${settings.ssl.password}';"
      //      )
      //    }

      val consumer = new KafkaConsumer[Int, BlockchainUpdated](
        props,
        IntDeserializer,
        BlockchainUpdatedDeserializer
      )

      consumer.subscribe(util.Arrays.asList(sys.env.getOrElse("VOLK_TOPIC", "blockchain-updates-mainnet")))
      consumer
    }
  }

  import java.time.{Duration => JavaDuration}

  import com.wavesplatform.events.protobuf.BlockchainUpdated

  import scala.concurrent.duration.Duration
  class PollingAgent {
    private[this] val consumer =
      Consumer.create()

    private[this] var isCommitPending: Boolean = false

    def start(cb: Seq[BlockchainUpdated] => Unit): Unit =
      while (!Thread.currentThread().isInterrupted) {
        isCommitPending = false
        val records = consumer.poll(JavaDuration.ofMillis(1000))
        val values = Seq.newBuilder[BlockchainUpdated]
        records.forEach(record => values += record.value)
        cb(values.result())
        isCommitPending = true
        consumer.commitSync()
      }

    def shutdown(): Unit = {
      if (isCommitPending) consumer.commitSync()
      consumer.close()
    }

    def shutdown(timeout: Duration): Unit = {
      if (isCommitPending) consumer.commitSync()
      consumer.close(JavaDuration.ofMillis(timeout.toMillis))
    }
  }

}
