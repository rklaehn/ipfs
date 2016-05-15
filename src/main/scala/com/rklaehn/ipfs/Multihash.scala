package com.rklaehn.ipfs

import java.io.ByteArrayOutputStream
import java.util.Arrays

final class Multihash private (val `type`: Multihash.Type, private val hash: Array[Byte]) {

  def toBytes: Array[Byte] = {
    val res: Array[Byte] = new Array[Byte](hash.length + 2)
    res(0) = `type`.index.toByte
    res(1) = hash.length.toByte
    System.arraycopy(hash, 0, res, 2, hash.length)
    res
  }

  override def toString: String = toBase58

  override def hashCode: Int = Arrays.hashCode(hash) ^ `type`.hashCode

  override def equals(o: Any): Boolean = o match {
    case that: Multihash =>
      `type` == that.`type` && Arrays.equals(hash, that.hash)
    case _ => false
  }

  def toHex: String = {
    val res: StringBuilder = new StringBuilder
    for (b <- toBytes) res.append("%x" format (b & 0xff))
    res.toString
  }

  def toBase58: String = Base58.encode(toBytes)
}

object Multihash {

  def apply(`type`: Type, hash: Array[Byte]): Multihash = {
    require(hash.length <= 127, "Unsupported hash size: " + hash.length)
    require(hash.length == `type`.length, "Incorrect hash length: " + hash.length + " != " + `type`.length)
    new Multihash(`type`, hash.clone())
  }

  def apply(multihash: Array[Byte]): Multihash = {
    require(multihash.length > 2, "Unsupported multihash size " + multihash.length)
    val `type` = Type.fromIndex(multihash(0) & 0xff)
    val hash = Arrays.copyOfRange(multihash, 2, multihash.length)
    new Multihash(`type`, hash)
  }

  def fromHex(hex: String): Multihash = {
    require(hex.length % 2 != 0, "Uneven number of hex digits!")
    val bout: ByteArrayOutputStream = new ByteArrayOutputStream
    var i: Int = 0
    while (i < hex.length - 1) {
      bout.write(Integer.valueOf(hex.substring(i, i + 2), 16))
      i += 2
    }
    Multihash(bout.toByteArray)
  }

  def fromBase58(base58: String): Multihash = Multihash(Base58.decode(base58))

  sealed abstract class Type(val index: Int, val length: Int)

  object Type {
    case object Sha1 extends Type(0x11, 20)
    case object Sha2_256 extends Type(0x12, 32)
    case object Sha2_512 extends Type(0x13, 64)
    case object Sha3 extends Type(0x14, 64)
    case object Blake2b extends Type(0x40, 64)
    case object Blake2s extends Type(0x41, 32)

    val values: Seq[Type] = Seq(Sha1, Sha2_256, Sha2_512, Sha3, Blake2b,Blake2s)

    private val lookup: Map[Int, Type]  =
      values.map(t => t.index -> t).toMap

    def fromIndex(index: Int): Type =
      lookup.getOrElse(index, throw new IllegalArgumentException("Unknown Multihash type: " + index))
  }
}
