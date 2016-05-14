package com.rklaehn.ipfs.client

import java.io.{Serializable, IOException}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.client.RequestBuilding._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.rklaehn.ipfs._
import scala.concurrent.duration._
import akka.http.scaladsl.marshalling._
import scala.concurrent.Future
import io.circe.Json
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import akka._

class IPFS(host: String, port: Int, api: String)(implicit system: ActorSystem) {

  import system.dispatcher

  private implicit val mat = ActorMaterializer()

  private val http = Http()

  private val base = s"http://$host:$port$api"

  private def get(path: String): Future[HttpResponse] =
    Http().singleRequest(Get(s"$base$path"))
      .flatMap { res =>
        if(res.status.isSuccess) Future.successful(res)
        else Future.failed(new Exception(s"Unexpected status code ${res.status.intValue}"))
      }

  private def getJson(path: String): Future[Json] =
    get(path).flatMap(Unmarshal(_).to[Json])

  def cat(hash: Multihash): Future[Source[ByteString, Any]] =
    get(s"cat/$hash").map(_.entity.dataBytes)

  def get(hash: Multihash): Future[Source[ByteString, Any]] =
    get(s"get/$hash").map(_.entity.dataBytes)

  object refs {

  }

  object pin {
  }

  object repo {
    def gc: Future[Json] = getJson("repo/gc")
  }

  trait Block
  object block extends Block {

    def get(hash: Multihash): Future[ByteString] = {
      ByteString.empty.utf8String
    }

    @throws(classOf[IOException])
    def put(data: Seq[ByteString]): Future[Seq[MerkleNode]] = {
      ???
    }
  }

  object ipfsObject extends Block {

  }

  object name {

  }

  object dht {

  }

  object file {
    def ls(path: Multihash): Future[Json] =
      getJson("file/ls?arg=" + path)
  }

  object bootstrap {

    def apply(): Future[Seq[MultiAddress]] = getJson("bootstrap/").map(
      _.asObject.flatMap(
        _("Peers").flatMap(
          _.asArray.map(
            _.flatMap(
              _.asString.map(MultiAddress.apply).toSeq
            )
          )
        )
      ).getOrElse(throw new IOException("Unexpected json"))
    )

    def list: Future[Seq[MultiAddress]] = apply()

    def add(addr: MultiAddress): Future[Seq[MultiAddress]] = getJson(s"bootstrap/add?arg=$addr").map(
      _.asObject.flatMap(
        _("Peers").flatMap(
          _.asArray.map(
            _.flatMap(_.asString.map(MultiAddress.apply))
          )
        )
      ).getOrElse(throw new IOException("Unexpected json"))
    )

    def rm(addr: MultiAddress, all: Boolean = false): Future[Seq[MultiAddress]] =
      getJson(s"bootstrap/rm?arg=$addr"+(if(all) "&all=true" else "")).map(
        _.asObject.flatMap(
          _("Peers").flatMap(
            _.asArray.map(
              _.flatMap(_.asString.map(MultiAddress.apply))
            )
          )
        ).getOrElse(throw new IOException("Unexpected json"))
      )
  }

  object diag {
    def net: Future[Json] = getJson("diag/net?stream-channels=true")
  }

  object swarm {

    def peers: Future[Seq[MultiAddress]] = {
      getJson("swarm/peers?stream-channels=true").map(
        _.asObject.flatMap(
          _("Strings").flatMap(
            _.asArray.map(
              _.toArray.flatMap(
                _.asString.map(MultiAddress.apply)
              )
            )
          )
        ).getOrElse(throw new IOException("Unexpected json"))
      )
    }

    def addrs = getJson("swarm/addrs?stream-channels=true")

    def connect(multiAddress: String) = getJson(s"swarm/connect?arg=$multiAddress")

    def disconnect(multiAddress: String) = getJson(s"swarm/disconnect?arg=$multiAddress")
  }

  object config {

  }

  object stats {
    def bw: Future[Json] = getJson("stats/bw")
  }

  def version: Future[String] = {
    getJson("version").map(_.asObject.flatMap(_.apply("Version")).flatMap(_.asString).getOrElse("unknown"))
  }

  def commands: Future[Json] = getJson("commands")

  def log: Future[Json] = getJson("log/tail")
}

object IPFS {

  sealed trait PinType
  object PinType {
    object All extends PinType
    object Direct extends PinType
    object Indirect extends PinType
    object Recursive extends PinType
  }

  val objectTemplates = List("unixfs-dir")
  val objectPatchTypes = List("add-link", "rm-link", "set-data", "append-data")

  def apply(multiaddr: String)(implicit system: ActorSystem): IPFS =
    apply(MultiAddress(multiaddr))

  def apply(addr: MultiAddress)(implicit system: ActorSystem): IPFS =
    apply(addr.getHost.get, addr.getTCPPort.get, "/api/v0/")

  def apply(host: String, port: Int)(implicit system: ActorSystem): IPFS = apply(host, port, "/api/v0/")

  def apply(host: String, port: Int, version: String)(implicit system: ActorSystem): IPFS = new IPFS(host, port, version)
}
