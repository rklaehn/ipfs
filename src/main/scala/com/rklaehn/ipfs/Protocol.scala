package com.rklaehn.ipfs

import java.io._
import java.net.InetAddress
import Protocol._
import Protocol.Type._

final class Protocol(`type`: Protocol.Type) {

  def size = `type`.size

  def code = `type`.code

  def name = `type`.name

  def appendCode(out: OutputStream): Unit =
    out.write(`type`.encoded)

  def sizeForAddress(in: InputStream): Int =
    if (`type`.size > 0)
      `type`.size / 8
    else if (`type`.size == 0)
      0
    else
      Protocol.readVarint(in).toInt

  def addressToBytes(addr: String): Array[Byte] = {
    `type` match {
      case IP4 | IP6  =>
        InetAddress.getByName(addr).getAddress
      case TCP | UDP | SCTP | DCCP =>
        val x: Int = addr.toInt
        if (x > 65535) throw new IllegalStateException("Failed to parse " + `type`.name + " address " + addr + " (> 65535")
        Array[Byte]((x >> 8).toByte, x.toByte)
      case IPFS =>
        val hash: Multihash = Multihash.fromBase58(addr)
        val bout: ByteArrayOutputStream = new ByteArrayOutputStream
        val hashBytes: Array[Byte] = hash.toBytes
        val varint: Array[Byte] = new Array[Byte]((32 - Integer.numberOfLeadingZeros(hashBytes.length) + 6) / 7)
        putUvarint(varint, hashBytes.length)
        bout.write(varint)
        bout.write(hashBytes)
        bout.toByteArray
      case ONION =>
        val split = addr.split(':')
        if (split.length != 2) throw new IllegalStateException("Onion address needs a port: " + addr)
        if (split(0).length != 16) throw new IllegalStateException("failed to parse " + name + " addr: " + addr + " not a Tor onion address.")
        val onionHostBytes = Base32.decode(split(0).toUpperCase)
        val port = split(1).toInt
        if (port > 65535) throw new IllegalStateException("Port is > 65535: " + port)
        if (port < 1) throw new IllegalStateException("Port is < 1: " + port)
        val b = new ByteArrayOutputStream
        val dout = new DataOutputStream(b)
        dout.write(onionHostBytes)
        dout.writeShort(port)
        dout.flush()
        b.toByteArray
      case _ =>
        throw new UnsupportedOperationException("Failed to parse address: " + addr)
    }
  }

  def readAddress(in: InputStream): String = {
    import Type._
    val sizeForAddress: Int = this.sizeForAddress(in)
    `type` match {
      case IP4 =>
        val buf = new Array[Byte](sizeForAddress)
        in.read(buf)
        InetAddress.getByAddress(buf).toString.substring(1)
      case IP6 =>
        val buf = new Array[Byte](sizeForAddress)
        in.read(buf)
        InetAddress.getByAddress(buf).toString.substring(1)
      case TCP | UDP | DCCP | SCTP =>
        Integer.toString((in.read << 8) | in.read)
      case IPFS =>
        val buf = new Array[Byte](sizeForAddress)
        in.read(buf)
        Multihash(buf).toBase58
      case ONION =>
        val host: Array[Byte] = new Array[Byte](10)
        in.read(host)
        val port = ((in.read << 8) | in.read).toString
        Base32.encode(host) + ":" + port
      case _ =>
        throw new UnsupportedOperationException("Unimplemented protocol type: " + `type`.name)
    }
  }
}

object Protocol {
  var LENGTH_PREFIXED_VAR_SIZE: Int = -1
  
  sealed class Type(val code: Int, val size: Int, val name: String) {
    private[ipfs] val encoded = Type.encode(code)
  }
  
  object Type {
    object IP4 extends Type(4, 32, "ip4")
    object TCP extends Type(6, 16, "tcp")
    object UDP extends Type(17, 16, "udp")
    object DCCP extends Type(33, 16, "dccp")
    object IP6 extends Type(41, 128, "ip6")
    object SCTP extends Type(132, 16, "sctp")
    object UTP extends Type(301, 0, "utp")
    object UDT extends Type(302, 0, "udt")
    object IPFS extends Type(421, LENGTH_PREFIXED_VAR_SIZE, "ipfs")
    object HTTPS extends Type(443, 0, "https")
    object HTTP extends Type(480, 0, "http")
    object ONION extends Type(444, 80, "onion")

    val values: Seq[Type] = Seq(IP4, TCP, UDP, DCCP, IP6, SCTP, UTP, UDT, IPFS, HTTPS, HTTP, ONION)

    private[ipfs] val byName = values.map(t => t.name -> t).toMap
    private[ipfs] val byCode = values.map(t => t.code -> t).toMap
    private[ipfs] def encode(code: Int): Array[Byte] = {
      val varint: Array[Byte] = new Array[Byte]((32 - Integer.numberOfLeadingZeros(code) + 6) / 7)
      putUvarint(varint, code)
      varint
    }
  }

  private[ipfs] def putUvarint(buf: Array[Byte], x0: Long): Int = {
    var x: Long = x0
    var i: Int = 0
    while (x >= 0x80) {
      buf(i) = (x | 0x80).toByte
      x >>= 7
      i += 1
    }
    buf(i) = x.toByte
    i + 1
  }

  private[ipfs] def readVarint(in: InputStream): Long = {
    var x: Long = 0
    var s: Int = 0
    var i: Int = 0
    while (i < 10) {
      val b: Int = in.read
      if (b == -1) throw new EOFException
      if (b < 0x80) {
        if (i > 9 || i == 9 && b > 1)
          throw new IllegalArgumentException("Overflow reading varint" + (-(i + 1)))
        return x | (b.toLong << s)
      }
      x |= (b.toLong & 0x7f) << s
      s += 7
      i += 1
    }
    throw new IllegalArgumentException("Varint too long!")
  }

  def byName(name: String): Protocol =
    new Protocol(Type.byName.getOrElse(name, throw new IllegalArgumentException("No protocol with name: " + name)))

  def byCode(code: Int): Protocol =
    new Protocol(Type.byCode.getOrElse(code, throw new IllegalArgumentException("No protocol with code: " + code)))
}
