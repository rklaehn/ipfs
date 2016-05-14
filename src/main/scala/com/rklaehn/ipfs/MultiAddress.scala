package com.rklaehn.ipfs

import java.io._
import java.util.Arrays

final class MultiAddress private (private val raw: Array[Byte]) {

  def toBytes: Array[Byte] = Arrays.copyOf(raw, raw.length)

  override def equals(that: Any): Boolean = that match {
    case that: MultiAddress => Arrays.equals(this.raw, that.raw)
    case _ => false
  }

  override def hashCode: Int = Arrays.hashCode(raw)

  override def toString: String = MultiAddress.encodeToString(raw)

  def isTCPIP: Boolean = {
    val parts: Array[String] = toString.substring(1).split('/')
    if (parts.length != 4) false
    else if (!parts(0).startsWith("ip")) false
    else if (!(parts(2) == "tcp")) false
    else true
  }

  def getHost: Option[String] = {
    val parts: Array[String] = toString.substring(1).split('/')
    if (parts(0).startsWith("ip")) Some(parts(1)) else None
  }

  def getTCPPort: Option[Int] = {
    val parts: Array[String] = toString.substring(1).split('/')
    if (parts(2).startsWith("tcp")) Some(parts(3).toInt) else None
  }
}

object MultiAddress {

  def apply(text: String): MultiAddress =
    new MultiAddress(decodeFromString(text))

  def fromBytes(bytes: Array[Byte]): MultiAddress =
    apply(encodeToString(bytes)) // check validity

  private def decodeFromString(addr0: String): Array[Byte] = {
    var addr = addr0
    while (addr.endsWith("/")) addr = addr.substring(0, addr.length - 1)
    val parts: Array[String] = addr.split('/')
    if (parts(0).length != 0) throw new IllegalArgumentException("MultiAddress must start with a /")
    val bout: ByteArrayOutputStream = new ByteArrayOutputStream
    try {
      var i: Int = 1
      while (i < parts.length) {
        val part: String = parts(i)
        i += 1
        val p: Protocol = Protocol.byName(part)
        p.appendCode(bout)
        if (p.size != 0) {
          val component: String = parts(i)
          i += 1
          if (component.length == 0) throw new IllegalArgumentException("Protocol requires address, but non provided!")
          bout.write(p.addressToBytes(component))
        }
      }
      bout.toByteArray
    } catch {
      case e: IOException => {
        throw new IllegalArgumentException("Error decoding multiaddress: " + addr)
      }
    }
  }

  private def encodeToString(raw: Array[Byte]): String = {
    val b: StringBuilder = new StringBuilder
    val in: InputStream = new ByteArrayInputStream(raw)
    try {
      while (true) {
        val code: Int = Protocol.readVarint(in).toInt
        val p: Protocol = Protocol.byCode(code)
        b.append("/" + p.name)
        if(p.size != 0) {
          val addr: String = p.readAddress(in)
          if (addr.length > 0) b.append("/" + addr)
        }
      }
      ???
    } catch {
      case eof: EOFException =>
        b.toString
      case e: IOException =>
        throw e
    }
  }
}
