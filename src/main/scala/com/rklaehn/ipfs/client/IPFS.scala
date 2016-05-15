package com.rklaehn.ipfs.client

import java.io.{File, IOException}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model._
import akka.http.scaladsl.client.RequestBuilding._
import akka.http.scaladsl.unmarshalling._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.rklaehn.ipfs._
import com.rklaehn.ipfs.client.IPFS.PinType
import akka.http.scaladsl.marshalling._
import scala.concurrent.Future
import io.circe.{DecodingFailure, jawn, Decoder, Json}
import de.heikoseeberger.akkahttpcirce.CirceSupport._

class IPFS(host: String, port: Int, api: String)(implicit system: ActorSystem) {

  import IPFS.Format._

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

  private def post(path: String, entity: RequestEntity): Future[HttpResponse] =
    Http().singleRequest(Post(s"$base$path", entity))
      .flatMap { res =>
        if(res.status.isSuccess) Future.successful(res)
        else Future.failed(new Exception(s"Unexpected status code ${res.status.intValue}"))
      }

  private def getJson(path: String): Future[Json] = {
    println(s"getJson($path)")
    get(path).flatMap(Unmarshal(_).to[Json])
  }

  private def postJson(path: String, entity: RequestEntity): Future[Json] =
    post(path, entity).flatMap(Unmarshal(_).to[Json])

  private def responseToText(response: HttpResponse): Future[String] =
    response.entity.dataBytes.map(_.utf8String).runReduce(_ + _)

  private def getText(path: String): Future[String] =
    get(path).flatMap(responseToText)

  private def fail: Nothing = throw new IOException("Unexpected JSON")

  private def decodeFail(failure: DecodingFailure): Nothing = throw new IOException(s"Unexpected JSON $failure")

  private def createEntity(data: Seq[(String, Source[ByteString, Any])]): Future[RequestEntity] = {
    def toBodyPart(name: String, data: Source[ByteString, Any]): BodyPart =
      BodyPart(name, HttpEntity.IndefiniteLength(MediaTypes.`application/octet-stream`, data))
    val formData = Multipart.FormData(data.map((toBodyPart _).tupled): _*)
    Marshal(formData).to[RequestEntity]
  }

  private def createEntityFromFiles(files: Seq[File]): Future[RequestEntity] = {
    val parts = files.map(file => BodyPart.fromFile(file.getName, MediaTypes.`application/octet-stream`, file))
    val formData: Multipart.FormData = Multipart.FormData(parts: _*)
    Marshal(formData).to[RequestEntity]
  }

  private def unmarshalChunksRaw[A: Decoder]: FromEntityUnmarshaller[Seq[A]] =
    Unmarshaller(implicit ec => entity =>
      entity.dataBytes.runFold(Vector.empty[A]) { case (seq, bytes) =>
        val text = bytes.utf8String
        val json = jawn.parse(text).toOption.get
        seq :+ json.as[A].valueOr(decodeFail)
      }
    )

  private def unmarshalChunks[A: Decoder]: FromEntityUnmarshaller[Seq[A]] =
    unmarshalChunksRaw[A].forContentTypes(`application/json`)

  def cat(hash: Multihash): Future[Source[ByteString, Any]] =
    get(s"cat/$hash").map(_.entity.dataBytes)

  def get(hash: Multihash): Future[Source[ByteString, Any]] =
    get(s"get/$hash").map(_.entity.dataBytes)

  def add(parts: Seq[(String, Source[ByteString, Any])]): Future[String] = {
    createEntity(parts).flatMap(entity =>
      post("add?stream-channels=true", entity).flatMap(responseToText)
    )
  }

  def addFiles(files: File*): Future[Seq[MerkleNode]] = {
    val decodeMerkleNode: Decoder[MerkleNode] = Decoder.instance { c =>
      val res = for {
        key <- c.downField("Hash").as[Multihash]
        name <- c.downField("Name").as[Option[String]]
      } yield MerkleNode(key, name = name)
      res
    }
    val parts = files.map(file => BodyPart.fromFile(file.getName, MediaTypes.`application/octet-stream`, file))
    val formData: Multipart.FormData = Multipart.FormData(parts: _*)
    val um: FromEntityUnmarshaller[Seq[MerkleNode]] = unmarshalChunks[MerkleNode](decodeMerkleNode)
    Marshal(formData).to[RequestEntity].flatMap { entity =>
      post("add?stream-channels=true", entity).flatMap(response =>
        um(response.entity)
      )
    }
  }

  object refs {

    def apply(hash: Multihash, recursive: Boolean): Future[Json] = {
      getJson(s"refs?arg=$hash&r=$recursive")
    }

    def local: Future[Seq[Multihash]] =
      getText("refs/local").map(_.split("\n").map(Multihash.fromBase58))
  }

  object pin {

    def add(hash: Multihash): Future[Json] = {
      getJson(s"pin/add?stream-channels=true&arg=$hash")
    }

    def ls(pinType: PinType = PinType.Direct) = {
      getJson(s"pin/ls?stream-channels=true&t=${pinType.toString.toLowerCase}")
    }

    def rm(hash: Multihash, recursive: Boolean = true) = {
      getJson(s"pin/rm?stream-channels=true&r=$recursive&arg=$hash")
    }
  }

  /**
    * 'ipfs repo' is a plumbing command used to manipulate the repo.
    */
  object repo {

    def gc: Future[Json] = getJson("repo/gc")
  }

  trait Block {

    def get(hash: Multihash): Future[Source[ByteString, Any]] = {
      IPFS.this.get(s"block/get?stream-channels=true&arg=$hash").map(_.entity.dataBytes)
    }

    def put(data: Seq[(String, Source[ByteString, Any])]): Future[Seq[MerkleNode]] = {
      createEntity(data).flatMap(entity =>
        postJson("block/put?stream-channels=true", entity).map(
          _.as[MerkleNode].map(x => Seq(x)).getOrElse(fail)
        )
      )
    }

    def putFiles(files: File*): Future[Seq[MerkleNode]] = {
      val parts = files.map(file => BodyPart.fromFile(file.getName, MediaTypes.`application/octet-stream`, file))
      val formData: Multipart.FormData = Multipart.FormData(parts: _*)
      val um: FromEntityUnmarshaller[Seq[MerkleNode]] = unmarshalChunks[MerkleNode]
      Marshal(formData).to[RequestEntity].flatMap { entity =>
        println(entity.contentType)
        post("block/put?stream-channels=true", entity).flatMap(response =>
          um(response.entity)
        )
      }
    }

    def stat(hash: Multihash): Future[Json] =
      getJson(s"block/stat?stream-channels=true&arg=$hash")
  }

  /**
    *  'ipfs block' is a plumbing command used to manipulate raw ipfs blocks.
    */
  object block extends Block {

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

    def apply(): Future[Seq[MultiAddress]] = list()

    def list(): Future[Seq[MultiAddress]] =
      getJson("bootstrap/").map(
        _.hcursor.downField("Peers").as[Seq[MultiAddress]].getOrElse(fail)
      )

    def add(addr: MultiAddress): Future[Seq[MultiAddress]] =
      getJson(s"bootstrap/add?arg=$addr").map(
        _.hcursor.downField("Peers").as[Seq[MultiAddress]].getOrElse(fail)
      )

    def rm(addr: MultiAddress, all: Boolean = false): Future[Seq[MultiAddress]] =
      getJson(s"bootstrap/rm?arg=$addr"+(if(all) "&all=true" else "")).map(
        _.hcursor.downField("Peers").as[Seq[MultiAddress]].getOrElse(fail)
      )
  }

  object diag {
    def net: Future[Json] = getJson("diag/net?stream-channels=true")
  }

  object swarm {

    def peers: Future[Seq[MultiAddress]] = {
      getJson("swarm/peers?stream-channels=true").map(
        _.hcursor.downField("Strings").as[Seq[MultiAddress]].getOrElse(fail)
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

  def version: Future[String] =
    getJson("version").map(_.hcursor.downField("Version").as[String].getOrElse(fail))

  def commands: Future[Json] = getJson("commands")

  def log: Future[Json] = getJson("log/tail")
}

object IPFS {

  sealed trait PinType
  object PinType {
    case object All extends PinType
    case object Direct extends PinType
    case object Indirect extends PinType
    case object Recursive extends PinType
  }

  val objectTemplates = List("unixfs-dir")
  val objectPatchTypes = List("add-link", "rm-link", "set-data", "append-data")

  def apply(multiaddr: String)(implicit system: ActorSystem): IPFS =
    apply(MultiAddress(multiaddr))

  def apply(addr: MultiAddress)(implicit system: ActorSystem): IPFS =
    apply(addr.getHost.get, addr.getTCPPort.get, "/api/v0/")

  def apply(host: String, port: Int)(implicit system: ActorSystem): IPFS = apply(host, port, "/api/v0/")

  def apply(host: String, port: Int, version: String)(implicit system: ActorSystem): IPFS = new IPFS(host, port, version)

  object Format {

    implicit val decodeMultihash: Decoder[Multihash] = Decoder.decodeString.map(Multihash.fromBase58)

    implicit val decodeMultiAddr: Decoder[MultiAddress] = Decoder.decodeString.map(MultiAddress.apply)

    implicit val decodeMerkleNode: Decoder[MerkleNode] = Decoder.instance { c =>
      for {
        key <- c.downField("Key").as[Multihash]
        size <- c.downField("Size").as[Option[Int]]
      } yield MerkleNode(key, size = size)
    }
  }
}
