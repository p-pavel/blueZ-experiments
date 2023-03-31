

import scala.jdk.CollectionConverters.*

import com.github.hypfvieh.bluetooth
import bluetooth.wrapper.BluetoothDevice
import bluetooth.DeviceManager

import org.freedesktop.dbus
import dbus.handlers.{
  AbstractPropertiesChangedHandler,
  AbstractSignalHandlerBase
}
import dbus.types.Variant
import dbus.interfaces.Properties.PropertiesChanged
import dbus.messages.DBusSignal
import com.github.hypfvieh.DbusHelper
import dbus.connections.impl.DBusConnection
import DBusConnection.DBusBusType

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.implicits.*
import org.freedesktop.dbus.connections.impl.DBusConnection.DBusBusType
import org.freedesktop.dbus.interfaces.Introspectable





    

@main
def notifications =
  val manager = DeviceManager.createInstance(false)
  manager.registerSignalHandler(new AbstractSignalHandlerBase[DBusSignal] {

    override def handle(s: DBusSignal): Unit = println(s)

    override def getImplementationClass(): Class[DBusSignal] =
      classOf[DBusSignal]

  })
  manager.registerPropertyHandler(new AbstractPropertiesChangedHandler {

    override def handle(s: PropertiesChanged): Unit =
      println(s"Properties changed: $s")
  })

  manager.scanForBluetoothDevices(5000)
  manager.getDevices().asScala.head.connect()


@main
def tst =
  val manager = DeviceManager.createInstance(false)
  val adapter = manager.getAdapter()
  println(adapter)
  adapter.setPowered(true)
  println(s"Bluetooth: ${adapter.getBluetoothType()}")
  val devices = manager.scanForBluetoothDevices(1000).asScala
  devices.foreach(println)
  devices.lastOption.foreach { (device: BluetoothDevice) =>
    println(s"Connecting to ${device.getName}")
    val data = Map(
      "Name" -> device.getName(),
      "RSSI" -> device.getRssi()
    )
    data.foreach((k, v) => print(s"$k:$v\t"))
    println()
    println(s"Connecting to ${device.getName()}")
    val start = System.nanoTime()
    val conRes = device.connect()

    // println(s"Connected to ${device.getName()}: $conRes")
    // val servies = device.getGattServices().asScala
    // servies.foreach(svc => println(s"Service: ${svc.getUuid.toString()}"))
    val ledTextService =
      device.getGattServiceByUuid("e95dd91d-251d-470a-a062-fa1922dfa9a8")
    println("ledTextService: " + ledTextService)
    // ledTextService.getGattCharacteristics().asScala.foreach { characteristic =>
    //   println("cha:  " + characteristic.getUuid)
    // }
    val textCharacteristic = ledTextService.getGattCharacteristicByUuid(
      "e95d93ee-251d-470a-a062-fa1922dfa9a8"
    )
    println(s"Char: $textCharacteristic")
    println("==== reading ====")
    val readRes = textCharacteristic.readValue(Map.empty.asJava)
    println(String(readRes))
    val stop = System.nanoTime()
    println(s"Took: ${(stop - start) / 1000000} ms")



    textCharacteristic.writeValue("I did it!".getBytes, Map.empty.asJava)


    device.disconnect()
    println(s"Disconnected from ${device.getName}")
  }

  manager.closeConnection()
  println("Done")
