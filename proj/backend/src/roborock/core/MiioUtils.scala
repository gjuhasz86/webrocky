package roborock.core

import java.net._
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import javax.crypto.Cipher
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.SecretKeySpec


object MiioUtils {
  implicit class ToByteArrayOnString(val self: String) extends AnyVal {
    def hexToByteArray: Array[Byte] = self.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  }

  implicit class ToHexStringOnByteArray(val self: Array[Byte]) extends AnyVal {
    def toHexString: String = self.map("%02X" format _).mkString
  }

  def decrypt(encPayload: Array[Byte], token: String): String = {
    val rawToken = token.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    val key = md5(rawToken)
    val iv = md5(key ++ rawToken)

    val keySpec = new SecretKeySpec(key, "AES")
    val ivSpec = new IvParameterSpec(iv)
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec)
    val decPayload = new String(cipher.doFinal(encPayload).dropRight(1), StandardCharsets.UTF_8)
    decPayload
  }

  def encrypt(decPayload: String, token: String): Array[Byte] = {
    val rawToken = token.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    val key = md5(rawToken)
    val iv = md5(key ++ rawToken)

    val keySpec = new SecretKeySpec(key, "AES")
    val ivSpec = new IvParameterSpec(iv)
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec)
    val decRawPayload = decPayload.getBytes("UTF-8") :+ 0.toByte
    cipher.doFinal(decRawPayload)
  }

  def md5(bytes: Array[Byte]): Array[Byte] = {
    MessageDigest.getInstance("MD5").digest(bytes)
  }

  def sendRaw(ip: String, port: Int, bytes: Array[Byte]): Array[Byte] = {
    val packet = new DatagramPacket(bytes, bytes.length, InetAddress.getByName("192.168.0.100"), 54321)
    val socket = new DatagramSocket(new InetSocketAddress(31333))
    socket.setSoTimeout(10000)
    socket.send(packet)

    val buf = new Array[Byte](65535)
    val rcvPacket = new DatagramPacket(buf, buf.length)
    try {
      socket.receive(rcvPacket)
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        throw ex
    } finally {
      socket.close()
    }
    val res = rcvPacket.getData
    println(s"Received ${rcvPacket.getLength} bytes")
    res.slice(rcvPacket.getOffset, rcvPacket.getOffset + rcvPacket.getLength)
  }
}