import com.github.hypfvieh.bluetooth.DeviceManager
import scala.jdk.CollectionConverters.*
import com.github.hypfvieh.bluetooth.wrapper.BluetoothDevice

@main def tst =
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
    val conRes = device.connect() 
    println(s"Connected to ${device.getName()}: $conRes")
    val servies = device.getGattServices().asScala
    servies.foreach(svc => println(s"Service: ${svc.getUuid.toString()}"))
    val ledTextService =
      device.getGattServiceByUuid("e95dd91d-251d-470a-a062-fa1922dfa9a8")
    println("ledTextService: " + ledTextService)
    ledTextService.getGattCharacteristics().asScala.foreach { characteristic =>
      println("cha:  " + characteristic.getUuid) }
    val textCharacteristic = ledTextService.getGattCharacteristicByUuid("e95d93ee-251d-470a-a062-fa1922dfa9a8")
    println(s"Char: $textCharacteristic")
    val devName = device.getName() 
    println("==== reading ====")
    val readRes = textCharacteristic.readValue(Map.empty.asJava)
    println(String(readRes))
 

    println("==== writing ====")


    textCharacteristic.writeValue("I did it!".getBytes, Map.empty.asJava)

    device.disconnect()
    println(s"Disconnected from ${device.getName}")
  }

  manager.closeConnection()
  println("Done")

