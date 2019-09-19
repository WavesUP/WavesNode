//package com.wavesplatform
//
//import cats.effect.Resource
//import com.wavesplatform.state.BlockchainUpdated
//import monix.reactive.Observable
//import org.iq80.leveldb.DB
//import scorex.crypto.signatures.Signature
//
//trait Stream[M[_], A]
//
//trait DbRead[M[_]] {
//  def getHeight: M[Int]
//  def getSignature(height: Int): M[Signature]
//  def readFrom(from: Int): Stream[M, BlockchainUpdated]
//}
//
//trait DbWrite[M[_]] {
//  def writeUpdate(update: BlockchainUpdated): M[Unit]
//}
//
//trait Topic[M[_], A]
//
//trait Writer[M[_]] {
//  def run: Stream[M, BlockchainUpdated]
//}
//
//trait Config
//
//trait Module[M[_]] {
//  def dbWrite: DbWrite[M]
//  def dbRead: DbRead[M]
//}
//
//trait Db[M[_], K, V] {
//  def read(key: K): M[Option[V]]
//  def write(key: K, value: V): M[Unit]
//  def stream: Stream[M, (K, V)]
//}
//
//
//trait LevelDB[M[_]] {
//  def resource: Resource[M, Db[M]]
//}
//
//trait Init[I[_], M[_]] {
//  def run: I[(Topic[M, BlockchainUpdated], Module[M])]
//}
//
//class ConsumerImpl(...) {
//
//}
//
//object Program {
//
//  def run[M[_]]: M[Unit] = ???
//
//}
