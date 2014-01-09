// Generated by ScalaBuff, the Scala Protocol Buffers compiler. DO NOT EDIT!
// source: tradesim.proto

package dke.tradesim.protobuf

final case class TransactionLog (
	`transactions`: scala.collection.immutable.Seq[Transaction] = Vector.empty[Transaction]
) extends com.google.protobuf.GeneratedMessageLite
	with com.google.protobuf.MessageLite.Builder
	with net.sandrogrzicic.scalabuff.Message[TransactionLog]
	with net.sandrogrzicic.scalabuff.Parser[TransactionLog] {

	def setTransactions(_i: Int, _v: Transaction) = copy(`transactions` = `transactions`.updated(_i, _v))
	def addTransactions(_f: Transaction) = copy(`transactions` = `transactions` :+ _f)
	def addAllTransactions(_f: Transaction*) = copy(`transactions` = `transactions` ++ _f)
	def addAllTransactions(_f: TraversableOnce[Transaction]) = copy(`transactions` = `transactions` ++ _f)

	def clearTransactions = copy(`transactions` = Vector.empty[Transaction])

	def writeTo(output: com.google.protobuf.CodedOutputStream) {
		for (_v <- `transactions`) output.writeMessage(1, _v)
	}

	def getSerializedSize = {
		import com.google.protobuf.CodedOutputStream._
		var __size = 0
		for (_v <- `transactions`) __size += computeMessageSize(1, _v)

		__size
	}

	def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): TransactionLog = {
		import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
		val __transactions: scala.collection.mutable.Buffer[Transaction] = `transactions`.toBuffer

		def __newMerged = TransactionLog(
			Vector(__transactions: _*)
		)
		while (true) in.readTag match {
			case 0 => return __newMerged
			case 10 => __transactions += readMessage[Transaction](in, Transaction.defaultInstance, _emptyRegistry)
			case default => if (!in.skipField(default)) return __newMerged
		}
		null
	}

	def mergeFrom(m: TransactionLog) = {
		TransactionLog(
			`transactions` ++ m.`transactions`
		)
	}

	def getDefaultInstanceForType = TransactionLog.defaultInstance
	def clear = getDefaultInstanceForType
	def isInitialized = true
	def build = this
	def buildPartial = this
	def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
	override def getParserForType = this
	def newBuilderForType = getDefaultInstanceForType
	def toBuilder = this
	def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
}

object TransactionLog {
	@reflect.BeanProperty val defaultInstance = new TransactionLog()

	def parseFrom(data: Array[Byte]): TransactionLog = defaultInstance.mergeFrom(data)
	def parseFrom(data: Array[Byte], offset: Int, length: Int): TransactionLog = defaultInstance.mergeFrom(data, offset, length)
	def parseFrom(byteString: com.google.protobuf.ByteString): TransactionLog = defaultInstance.mergeFrom(byteString)
	def parseFrom(stream: java.io.InputStream): TransactionLog = defaultInstance.mergeFrom(stream)
	def parseDelimitedFrom(stream: java.io.InputStream): Option[TransactionLog] = defaultInstance.mergeDelimitedFromStream(stream)

	val TRANSACTIONS_FIELD_NUMBER = 1

	def newBuilder = defaultInstance.newBuilderForType
	def newBuilder(prototype: TransactionLog) = defaultInstance.mergeFrom(prototype)

}
final case class Transaction (
	`type`: Transaction.Type.EnumVal = Transaction.Type._UNINITIALIZED,
	`order`: Option[Order] = None,
	`splitAdjustment`: Option[SplitAdjustment] = None,
	`cashDividendPayment`: Option[CashDividendPayment] = None
) extends com.google.protobuf.GeneratedMessageLite
	with com.google.protobuf.MessageLite.Builder
	with net.sandrogrzicic.scalabuff.Message[Transaction]
	with net.sandrogrzicic.scalabuff.Parser[Transaction] {

	def setOrder(_f: Order) = copy(`order` = Some(_f))
	def setSplitAdjustment(_f: SplitAdjustment) = copy(`splitAdjustment` = Some(_f))
	def setCashDividendPayment(_f: CashDividendPayment) = copy(`cashDividendPayment` = Some(_f))

	def clearOrder = copy(`order` = None)
	def clearSplitAdjustment = copy(`splitAdjustment` = None)
	def clearCashDividendPayment = copy(`cashDividendPayment` = None)

	def writeTo(output: com.google.protobuf.CodedOutputStream) {
		output.writeEnum(1, `type`)
		if (`order`.isDefined) output.writeMessage(2, `order`.get)
		if (`splitAdjustment`.isDefined) output.writeMessage(3, `splitAdjustment`.get)
		if (`cashDividendPayment`.isDefined) output.writeMessage(4, `cashDividendPayment`.get)
	}

	def getSerializedSize = {
		import com.google.protobuf.CodedOutputStream._
		var __size = 0
		__size += computeEnumSize(1, `type`)
		if (`order`.isDefined) __size += computeMessageSize(2, `order`.get)
		if (`splitAdjustment`.isDefined) __size += computeMessageSize(3, `splitAdjustment`.get)
		if (`cashDividendPayment`.isDefined) __size += computeMessageSize(4, `cashDividendPayment`.get)

		__size
	}

	def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Transaction = {
		import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
		var __type: Transaction.Type.EnumVal = Transaction.Type._UNINITIALIZED
		var __order: Option[Order] = `order`
		var __splitAdjustment: Option[SplitAdjustment] = `splitAdjustment`
		var __cashDividendPayment: Option[CashDividendPayment] = `cashDividendPayment`

		def __newMerged = Transaction(
			__type,
			__order,
			__splitAdjustment,
			__cashDividendPayment
		)
		while (true) in.readTag match {
			case 0 => return __newMerged
			case 8 => __type = Transaction.Type.valueOf(in.readEnum())
			case 18 => __order = Some(readMessage[Order](in, __order.orElse({
				__order = Order.defaultInstance
				__order
			}).get, _emptyRegistry))
			case 26 => __splitAdjustment = Some(readMessage[SplitAdjustment](in, __splitAdjustment.orElse({
				__splitAdjustment = SplitAdjustment.defaultInstance
				__splitAdjustment
			}).get, _emptyRegistry))
			case 34 => __cashDividendPayment = Some(readMessage[CashDividendPayment](in, __cashDividendPayment.orElse({
				__cashDividendPayment = CashDividendPayment.defaultInstance
				__cashDividendPayment
			}).get, _emptyRegistry))
			case default => if (!in.skipField(default)) return __newMerged
		}
		null
	}

	def mergeFrom(m: Transaction) = {
		Transaction(
			m.`type`,
			m.`order`.orElse(`order`),
			m.`splitAdjustment`.orElse(`splitAdjustment`),
			m.`cashDividendPayment`.orElse(`cashDividendPayment`)
		)
	}

	def getDefaultInstanceForType = Transaction.defaultInstance
	def clear = getDefaultInstanceForType
	def isInitialized = true
	def build = this
	def buildPartial = this
	def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
	override def getParserForType = this
	def newBuilderForType = getDefaultInstanceForType
	def toBuilder = this
	def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
}

object Transaction {
	@reflect.BeanProperty val defaultInstance = new Transaction()

	def parseFrom(data: Array[Byte]): Transaction = defaultInstance.mergeFrom(data)
	def parseFrom(data: Array[Byte], offset: Int, length: Int): Transaction = defaultInstance.mergeFrom(data, offset, length)
	def parseFrom(byteString: com.google.protobuf.ByteString): Transaction = defaultInstance.mergeFrom(byteString)
	def parseFrom(stream: java.io.InputStream): Transaction = defaultInstance.mergeFrom(stream)
	def parseDelimitedFrom(stream: java.io.InputStream): Option[Transaction] = defaultInstance.mergeDelimitedFromStream(stream)

	val TYPE_FIELD_NUMBER = 1
	val ORDER_FIELD_NUMBER = 2
	val SPLITADJUSTMENT_FIELD_NUMBER = 3
	val CASHDIVIDENDPAYMENT_FIELD_NUMBER = 4

	def newBuilder = defaultInstance.newBuilderForType
	def newBuilder(prototype: Transaction) = defaultInstance.mergeFrom(prototype)

	object Type extends net.sandrogrzicic.scalabuff.Enum {
		sealed trait EnumVal extends Value
		val _UNINITIALIZED = new EnumVal { val name = "UNINITIALIZED ENUM VALUE"; val id = -1 }

		val Order = new EnumVal { val name = "Order"; val id = 0 }
		val SplitAdjustment = new EnumVal { val name = "SplitAdjustment"; val id = 1 }
		val CashDividendPayment = new EnumVal { val name = "CashDividendPayment"; val id = 2 }

		val Order_VALUE = 0
		val SplitAdjustment_VALUE = 1
		val CashDividendPayment_VALUE = 2

		def valueOf(id: Int) = id match {
			case 0 => Order
			case 1 => SplitAdjustment
			case 2 => CashDividendPayment
			case _default => throw new net.sandrogrzicic.scalabuff.UnknownEnumException(_default)
		}
		val internalGetValueMap = new com.google.protobuf.Internal.EnumLiteMap[EnumVal] {
			def findValueByNumber(id: Int): EnumVal = valueOf(id)
		}
	}

}
final case class Order (
	`type`: Order.Type.EnumVal = Order.Type._UNINITIALIZED,
	`time`: Long = 0L,
	`securityId`: Long = 0L,
	`qty`: Long = 0L,
	`fillPrice`: Option[String] = None,
	`limitPrice`: Option[String] = None
) extends com.google.protobuf.GeneratedMessageLite
	with com.google.protobuf.MessageLite.Builder
	with net.sandrogrzicic.scalabuff.Message[Order]
	with net.sandrogrzicic.scalabuff.Parser[Order] {

	def setFillPrice(_f: String) = copy(`fillPrice` = Some(_f))
	def setLimitPrice(_f: String) = copy(`limitPrice` = Some(_f))

	def clearFillPrice = copy(`fillPrice` = None)
	def clearLimitPrice = copy(`limitPrice` = None)

	def writeTo(output: com.google.protobuf.CodedOutputStream) {
		output.writeEnum(1, `type`)
		output.writeInt64(2, `time`)
		output.writeInt64(3, `securityId`)
		output.writeInt64(4, `qty`)
		if (`fillPrice`.isDefined) output.writeString(5, `fillPrice`.get)
		if (`limitPrice`.isDefined) output.writeString(6, `limitPrice`.get)
	}

	def getSerializedSize = {
		import com.google.protobuf.CodedOutputStream._
		var __size = 0
		__size += computeEnumSize(1, `type`)
		__size += computeInt64Size(2, `time`)
		__size += computeInt64Size(3, `securityId`)
		__size += computeInt64Size(4, `qty`)
		if (`fillPrice`.isDefined) __size += computeStringSize(5, `fillPrice`.get)
		if (`limitPrice`.isDefined) __size += computeStringSize(6, `limitPrice`.get)

		__size
	}

	def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Order = {
		import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
		var __type: Order.Type.EnumVal = Order.Type._UNINITIALIZED
		var __time: Long = 0L
		var __securityId: Long = 0L
		var __qty: Long = 0L
		var __fillPrice: Option[String] = `fillPrice`
		var __limitPrice: Option[String] = `limitPrice`

		def __newMerged = Order(
			__type,
			__time,
			__securityId,
			__qty,
			__fillPrice,
			__limitPrice
		)
		while (true) in.readTag match {
			case 0 => return __newMerged
			case 8 => __type = Order.Type.valueOf(in.readEnum())
			case 16 => __time = in.readInt64()
			case 24 => __securityId = in.readInt64()
			case 32 => __qty = in.readInt64()
			case 42 => __fillPrice = Some(in.readString())
			case 50 => __limitPrice = Some(in.readString())
			case default => if (!in.skipField(default)) return __newMerged
		}
		null
	}

	def mergeFrom(m: Order) = {
		Order(
			m.`type`,
			m.`time`,
			m.`securityId`,
			m.`qty`,
			m.`fillPrice`.orElse(`fillPrice`),
			m.`limitPrice`.orElse(`limitPrice`)
		)
	}

	def getDefaultInstanceForType = Order.defaultInstance
	def clear = getDefaultInstanceForType
	def isInitialized = true
	def build = this
	def buildPartial = this
	def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
	override def getParserForType = this
	def newBuilderForType = getDefaultInstanceForType
	def toBuilder = this
	def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
}

object Order {
	@reflect.BeanProperty val defaultInstance = new Order()

	def parseFrom(data: Array[Byte]): Order = defaultInstance.mergeFrom(data)
	def parseFrom(data: Array[Byte], offset: Int, length: Int): Order = defaultInstance.mergeFrom(data, offset, length)
	def parseFrom(byteString: com.google.protobuf.ByteString): Order = defaultInstance.mergeFrom(byteString)
	def parseFrom(stream: java.io.InputStream): Order = defaultInstance.mergeFrom(stream)
	def parseDelimitedFrom(stream: java.io.InputStream): Option[Order] = defaultInstance.mergeDelimitedFromStream(stream)

	val TYPE_FIELD_NUMBER = 1
	val TIME_FIELD_NUMBER = 2
	val SECURITYID_FIELD_NUMBER = 3
	val QTY_FIELD_NUMBER = 4
	val FILLPRICE_FIELD_NUMBER = 5
	val LIMITPRICE_FIELD_NUMBER = 6

	def newBuilder = defaultInstance.newBuilderForType
	def newBuilder(prototype: Order) = defaultInstance.mergeFrom(prototype)

	object Type extends net.sandrogrzicic.scalabuff.Enum {
		sealed trait EnumVal extends Value
		val _UNINITIALIZED = new EnumVal { val name = "UNINITIALIZED ENUM VALUE"; val id = -1 }

		val MarketBuy = new EnumVal { val name = "MarketBuy"; val id = 0 }
		val MarketSell = new EnumVal { val name = "MarketSell"; val id = 1 }
		val LimitBuy = new EnumVal { val name = "LimitBuy"; val id = 2 }
		val LimitSell = new EnumVal { val name = "LimitSell"; val id = 3 }

		val MarketBuy_VALUE = 0
		val MarketSell_VALUE = 1
		val LimitBuy_VALUE = 2
		val LimitSell_VALUE = 3

		def valueOf(id: Int) = id match {
			case 0 => MarketBuy
			case 1 => MarketSell
			case 2 => LimitBuy
			case 3 => LimitSell
			case _default => throw new net.sandrogrzicic.scalabuff.UnknownEnumException(_default)
		}
		val internalGetValueMap = new com.google.protobuf.Internal.EnumLiteMap[EnumVal] {
			def findValueByNumber(id: Int): EnumVal = valueOf(id)
		}
	}

}
final case class SplitAdjustment (
	`securityId`: Long = 0L,
	`exDate`: Int = 0,
	`ratio`: String = "",
	`adjustmentTime`: Long = 0L,
	`shareQtyDelta`: Long = 0L,
	`cashPayout`: String = ""
) extends com.google.protobuf.GeneratedMessageLite
	with com.google.protobuf.MessageLite.Builder
	with net.sandrogrzicic.scalabuff.Message[SplitAdjustment]
	with net.sandrogrzicic.scalabuff.Parser[SplitAdjustment] {



	def writeTo(output: com.google.protobuf.CodedOutputStream) {
		output.writeInt64(1, `securityId`)
		output.writeInt32(2, `exDate`)
		output.writeString(3, `ratio`)
		output.writeInt64(4, `adjustmentTime`)
		output.writeInt64(5, `shareQtyDelta`)
		output.writeString(6, `cashPayout`)
	}

	def getSerializedSize = {
		import com.google.protobuf.CodedOutputStream._
		var __size = 0
		__size += computeInt64Size(1, `securityId`)
		__size += computeInt32Size(2, `exDate`)
		__size += computeStringSize(3, `ratio`)
		__size += computeInt64Size(4, `adjustmentTime`)
		__size += computeInt64Size(5, `shareQtyDelta`)
		__size += computeStringSize(6, `cashPayout`)

		__size
	}

	def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): SplitAdjustment = {
		import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
		var __securityId: Long = 0L
		var __exDate: Int = 0
		var __ratio: String = ""
		var __adjustmentTime: Long = 0L
		var __shareQtyDelta: Long = 0L
		var __cashPayout: String = ""

		def __newMerged = SplitAdjustment(
			__securityId,
			__exDate,
			__ratio,
			__adjustmentTime,
			__shareQtyDelta,
			__cashPayout
		)
		while (true) in.readTag match {
			case 0 => return __newMerged
			case 8 => __securityId = in.readInt64()
			case 16 => __exDate = in.readInt32()
			case 26 => __ratio = in.readString()
			case 32 => __adjustmentTime = in.readInt64()
			case 40 => __shareQtyDelta = in.readInt64()
			case 50 => __cashPayout = in.readString()
			case default => if (!in.skipField(default)) return __newMerged
		}
		null
	}

	def mergeFrom(m: SplitAdjustment) = {
		SplitAdjustment(
			m.`securityId`,
			m.`exDate`,
			m.`ratio`,
			m.`adjustmentTime`,
			m.`shareQtyDelta`,
			m.`cashPayout`
		)
	}

	def getDefaultInstanceForType = SplitAdjustment.defaultInstance
	def clear = getDefaultInstanceForType
	def isInitialized = true
	def build = this
	def buildPartial = this
	def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
	override def getParserForType = this
	def newBuilderForType = getDefaultInstanceForType
	def toBuilder = this
	def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
}

object SplitAdjustment {
	@reflect.BeanProperty val defaultInstance = new SplitAdjustment()

	def parseFrom(data: Array[Byte]): SplitAdjustment = defaultInstance.mergeFrom(data)
	def parseFrom(data: Array[Byte], offset: Int, length: Int): SplitAdjustment = defaultInstance.mergeFrom(data, offset, length)
	def parseFrom(byteString: com.google.protobuf.ByteString): SplitAdjustment = defaultInstance.mergeFrom(byteString)
	def parseFrom(stream: java.io.InputStream): SplitAdjustment = defaultInstance.mergeFrom(stream)
	def parseDelimitedFrom(stream: java.io.InputStream): Option[SplitAdjustment] = defaultInstance.mergeDelimitedFromStream(stream)

	val SECURITYID_FIELD_NUMBER = 1
	val EXDATE_FIELD_NUMBER = 2
	val RATIO_FIELD_NUMBER = 3
	val ADJUSTMENTTIME_FIELD_NUMBER = 4
	val SHAREQTYDELTA_FIELD_NUMBER = 5
	val CASHPAYOUT_FIELD_NUMBER = 6

	def newBuilder = defaultInstance.newBuilderForType
	def newBuilder(prototype: SplitAdjustment) = defaultInstance.mergeFrom(prototype)

}
final case class CashDividendPayment (
	`securityId`: Long = 0L,
	`exDate`: Long = 0L,
	`payableDate`: Option[Long] = None,
	`amountPerShare`: String = "",
	`adjustmentTime`: Long = 0L,
	`shareQty`: Long = 0L,
	`total`: String = ""
) extends com.google.protobuf.GeneratedMessageLite
	with com.google.protobuf.MessageLite.Builder
	with net.sandrogrzicic.scalabuff.Message[CashDividendPayment]
	with net.sandrogrzicic.scalabuff.Parser[CashDividendPayment] {

	def setPayableDate(_f: Long) = copy(`payableDate` = Some(_f))

	def clearPayableDate = copy(`payableDate` = None)

	def writeTo(output: com.google.protobuf.CodedOutputStream) {
		output.writeInt64(1, `securityId`)
		output.writeInt64(2, `exDate`)
		if (`payableDate`.isDefined) output.writeInt64(3, `payableDate`.get)
		output.writeString(4, `amountPerShare`)
		output.writeInt64(5, `adjustmentTime`)
		output.writeInt64(6, `shareQty`)
		output.writeString(7, `total`)
	}

	def getSerializedSize = {
		import com.google.protobuf.CodedOutputStream._
		var __size = 0
		__size += computeInt64Size(1, `securityId`)
		__size += computeInt64Size(2, `exDate`)
		if (`payableDate`.isDefined) __size += computeInt64Size(3, `payableDate`.get)
		__size += computeStringSize(4, `amountPerShare`)
		__size += computeInt64Size(5, `adjustmentTime`)
		__size += computeInt64Size(6, `shareQty`)
		__size += computeStringSize(7, `total`)

		__size
	}

	def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): CashDividendPayment = {
		import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
		var __securityId: Long = 0L
		var __exDate: Long = 0L
		var __payableDate: Option[Long] = `payableDate`
		var __amountPerShare: String = ""
		var __adjustmentTime: Long = 0L
		var __shareQty: Long = 0L
		var __total: String = ""

		def __newMerged = CashDividendPayment(
			__securityId,
			__exDate,
			__payableDate,
			__amountPerShare,
			__adjustmentTime,
			__shareQty,
			__total
		)
		while (true) in.readTag match {
			case 0 => return __newMerged
			case 8 => __securityId = in.readInt64()
			case 16 => __exDate = in.readInt64()
			case 24 => __payableDate = Some(in.readInt64())
			case 34 => __amountPerShare = in.readString()
			case 40 => __adjustmentTime = in.readInt64()
			case 48 => __shareQty = in.readInt64()
			case 58 => __total = in.readString()
			case default => if (!in.skipField(default)) return __newMerged
		}
		null
	}

	def mergeFrom(m: CashDividendPayment) = {
		CashDividendPayment(
			m.`securityId`,
			m.`exDate`,
			m.`payableDate`.orElse(`payableDate`),
			m.`amountPerShare`,
			m.`adjustmentTime`,
			m.`shareQty`,
			m.`total`
		)
	}

	def getDefaultInstanceForType = CashDividendPayment.defaultInstance
	def clear = getDefaultInstanceForType
	def isInitialized = true
	def build = this
	def buildPartial = this
	def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
	override def getParserForType = this
	def newBuilderForType = getDefaultInstanceForType
	def toBuilder = this
	def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
}

object CashDividendPayment {
	@reflect.BeanProperty val defaultInstance = new CashDividendPayment()

	def parseFrom(data: Array[Byte]): CashDividendPayment = defaultInstance.mergeFrom(data)
	def parseFrom(data: Array[Byte], offset: Int, length: Int): CashDividendPayment = defaultInstance.mergeFrom(data, offset, length)
	def parseFrom(byteString: com.google.protobuf.ByteString): CashDividendPayment = defaultInstance.mergeFrom(byteString)
	def parseFrom(stream: java.io.InputStream): CashDividendPayment = defaultInstance.mergeFrom(stream)
	def parseDelimitedFrom(stream: java.io.InputStream): Option[CashDividendPayment] = defaultInstance.mergeDelimitedFromStream(stream)

	val SECURITYID_FIELD_NUMBER = 1
	val EXDATE_FIELD_NUMBER = 2
	val PAYABLEDATE_FIELD_NUMBER = 3
	val AMOUNTPERSHARE_FIELD_NUMBER = 4
	val ADJUSTMENTTIME_FIELD_NUMBER = 5
	val SHAREQTY_FIELD_NUMBER = 6
	val TOTAL_FIELD_NUMBER = 7

	def newBuilder = defaultInstance.newBuilderForType
	def newBuilder(prototype: CashDividendPayment) = defaultInstance.mergeFrom(prototype)

}
final case class PortfolioValueLog (
	`portfolioValues`: scala.collection.immutable.Seq[PortfolioValue] = Vector.empty[PortfolioValue]
) extends com.google.protobuf.GeneratedMessageLite
	with com.google.protobuf.MessageLite.Builder
	with net.sandrogrzicic.scalabuff.Message[PortfolioValueLog]
	with net.sandrogrzicic.scalabuff.Parser[PortfolioValueLog] {

	def setPortfolioValues(_i: Int, _v: PortfolioValue) = copy(`portfolioValues` = `portfolioValues`.updated(_i, _v))
	def addPortfolioValues(_f: PortfolioValue) = copy(`portfolioValues` = `portfolioValues` :+ _f)
	def addAllPortfolioValues(_f: PortfolioValue*) = copy(`portfolioValues` = `portfolioValues` ++ _f)
	def addAllPortfolioValues(_f: TraversableOnce[PortfolioValue]) = copy(`portfolioValues` = `portfolioValues` ++ _f)

	def clearPortfolioValues = copy(`portfolioValues` = Vector.empty[PortfolioValue])

	def writeTo(output: com.google.protobuf.CodedOutputStream) {
		for (_v <- `portfolioValues`) output.writeMessage(1, _v)
	}

	def getSerializedSize = {
		import com.google.protobuf.CodedOutputStream._
		var __size = 0
		for (_v <- `portfolioValues`) __size += computeMessageSize(1, _v)

		__size
	}

	def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): PortfolioValueLog = {
		import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
		val __portfolioValues: scala.collection.mutable.Buffer[PortfolioValue] = `portfolioValues`.toBuffer

		def __newMerged = PortfolioValueLog(
			Vector(__portfolioValues: _*)
		)
		while (true) in.readTag match {
			case 0 => return __newMerged
			case 10 => __portfolioValues += readMessage[PortfolioValue](in, PortfolioValue.defaultInstance, _emptyRegistry)
			case default => if (!in.skipField(default)) return __newMerged
		}
		null
	}

	def mergeFrom(m: PortfolioValueLog) = {
		PortfolioValueLog(
			`portfolioValues` ++ m.`portfolioValues`
		)
	}

	def getDefaultInstanceForType = PortfolioValueLog.defaultInstance
	def clear = getDefaultInstanceForType
	def isInitialized = true
	def build = this
	def buildPartial = this
	def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
	override def getParserForType = this
	def newBuilderForType = getDefaultInstanceForType
	def toBuilder = this
	def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
}

object PortfolioValueLog {
	@reflect.BeanProperty val defaultInstance = new PortfolioValueLog()

	def parseFrom(data: Array[Byte]): PortfolioValueLog = defaultInstance.mergeFrom(data)
	def parseFrom(data: Array[Byte], offset: Int, length: Int): PortfolioValueLog = defaultInstance.mergeFrom(data, offset, length)
	def parseFrom(byteString: com.google.protobuf.ByteString): PortfolioValueLog = defaultInstance.mergeFrom(byteString)
	def parseFrom(stream: java.io.InputStream): PortfolioValueLog = defaultInstance.mergeFrom(stream)
	def parseDelimitedFrom(stream: java.io.InputStream): Option[PortfolioValueLog] = defaultInstance.mergeDelimitedFromStream(stream)

	val PORTFOLIOVALUES_FIELD_NUMBER = 1

	def newBuilder = defaultInstance.newBuilderForType
	def newBuilder(prototype: PortfolioValueLog) = defaultInstance.mergeFrom(prototype)

}
final case class PortfolioValue (
	`time`: Long = 0L,
	`value`: String = ""
) extends com.google.protobuf.GeneratedMessageLite
	with com.google.protobuf.MessageLite.Builder
	with net.sandrogrzicic.scalabuff.Message[PortfolioValue]
	with net.sandrogrzicic.scalabuff.Parser[PortfolioValue] {



	def writeTo(output: com.google.protobuf.CodedOutputStream) {
		output.writeInt64(1, `time`)
		output.writeString(2, `value`)
	}

	def getSerializedSize = {
		import com.google.protobuf.CodedOutputStream._
		var __size = 0
		__size += computeInt64Size(1, `time`)
		__size += computeStringSize(2, `value`)

		__size
	}

	def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): PortfolioValue = {
		import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
		var __time: Long = 0L
		var __value: String = ""

		def __newMerged = PortfolioValue(
			__time,
			__value
		)
		while (true) in.readTag match {
			case 0 => return __newMerged
			case 8 => __time = in.readInt64()
			case 18 => __value = in.readString()
			case default => if (!in.skipField(default)) return __newMerged
		}
		null
	}

	def mergeFrom(m: PortfolioValue) = {
		PortfolioValue(
			m.`time`,
			m.`value`
		)
	}

	def getDefaultInstanceForType = PortfolioValue.defaultInstance
	def clear = getDefaultInstanceForType
	def isInitialized = true
	def build = this
	def buildPartial = this
	def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
	override def getParserForType = this
	def newBuilderForType = getDefaultInstanceForType
	def toBuilder = this
	def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
}

object PortfolioValue {
	@reflect.BeanProperty val defaultInstance = new PortfolioValue()

	def parseFrom(data: Array[Byte]): PortfolioValue = defaultInstance.mergeFrom(data)
	def parseFrom(data: Array[Byte], offset: Int, length: Int): PortfolioValue = defaultInstance.mergeFrom(data, offset, length)
	def parseFrom(byteString: com.google.protobuf.ByteString): PortfolioValue = defaultInstance.mergeFrom(byteString)
	def parseFrom(stream: java.io.InputStream): PortfolioValue = defaultInstance.mergeFrom(stream)
	def parseDelimitedFrom(stream: java.io.InputStream): Option[PortfolioValue] = defaultInstance.mergeDelimitedFromStream(stream)

	val TIME_FIELD_NUMBER = 1
	val VALUE_FIELD_NUMBER = 2

	def newBuilder = defaultInstance.newBuilderForType
	def newBuilder(prototype: PortfolioValue) = defaultInstance.mergeFrom(prototype)

}

object Tradesim {
	def registerAllExtensions(registry: com.google.protobuf.ExtensionRegistryLite) {
	}

}