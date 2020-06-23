package com.wavesplatform
import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

import scala.compat.java8.StreamConverters._

object ScriptFilter extends App {
  val reader1 = new BufferedReader(new FileReader("all-updated-seq.csv"))
  val reader2 = new BufferedReader(new FileReader("all-updated-seq.csv"))
  val exceedingWriter = new BufferedWriter(new FileWriter("exceeding-updated-seq.csv"))
  val verifiersWriter = new BufferedWriter(new FileWriter("verifiers-updated-seq.csv"))
  val regex  = "(\\w+);(\\w+);(\\d+);(\\d+)".r

  exceedingWriter.write("address;type;current;new\n")
  verifiersWriter.write("address;type;current;new\n")

  reader1
    .lines()
    .toScala[Stream]
    .collect {
      case regex(address, scriptType, currentCost, newCost) if newCost.toInt > currentCost.toInt =>
        (address, scriptType, currentCost.toInt, newCost.toInt)
    }
    .sortBy(_._4)(Ordering[Int].reverse)
    .foreach {
      case (address, scriptType, currentCost, newCost) =>
        exceedingWriter.write(s"$address;$scriptType;$currentCost;$newCost\n")
    }

  reader2
    .lines()
    .toScala[Stream]
    .collect {
      case regex(address, scriptType, currentCost, newCost) if scriptType == "verifier" =>
        (address, scriptType, currentCost.toInt, newCost.toInt)
    }
    .sortBy(_._4)(Ordering[Int].reverse)
    .foreach {
      case (address, scriptType, currentCost, newCost) =>
        verifiersWriter.write(s"$address;$scriptType;$currentCost;$newCost\n")
    }

  reader1.close()
  reader2.close()
  exceedingWriter.close()
  verifiersWriter.close()
}
