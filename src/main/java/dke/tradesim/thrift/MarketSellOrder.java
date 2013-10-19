/**
 * Autogenerated by Thrift Compiler (0.9.0)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
package dke.tradesim.thrift;

import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.SchemeFactory;
import org.apache.thrift.scheme.StandardScheme;

import org.apache.thrift.scheme.TupleScheme;
import org.apache.thrift.protocol.TTupleProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.EncodingUtils;
import org.apache.thrift.TException;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.Set;
import java.util.HashSet;
import java.util.EnumSet;
import java.util.Collections;
import java.util.BitSet;
import java.nio.ByteBuffer;
import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MarketSellOrder implements org.apache.thrift.TBase<MarketSellOrder, MarketSellOrder._Fields>, java.io.Serializable, Cloneable {
  private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new org.apache.thrift.protocol.TStruct("MarketSellOrder");

  private static final org.apache.thrift.protocol.TField TIME_FIELD_DESC = new org.apache.thrift.protocol.TField("time", org.apache.thrift.protocol.TType.I64, (short)1);
  private static final org.apache.thrift.protocol.TField SECURITY_ID_FIELD_DESC = new org.apache.thrift.protocol.TField("securityId", org.apache.thrift.protocol.TType.I32, (short)2);
  private static final org.apache.thrift.protocol.TField QTY_FIELD_DESC = new org.apache.thrift.protocol.TField("qty", org.apache.thrift.protocol.TType.I64, (short)3);
  private static final org.apache.thrift.protocol.TField FILL_PRICE_FIELD_DESC = new org.apache.thrift.protocol.TField("fillPrice", org.apache.thrift.protocol.TType.STRING, (short)4);

  private static final Map<Class<? extends IScheme>, SchemeFactory> schemes = new HashMap<Class<? extends IScheme>, SchemeFactory>();
  static {
    schemes.put(StandardScheme.class, new MarketSellOrderStandardSchemeFactory());
    schemes.put(TupleScheme.class, new MarketSellOrderTupleSchemeFactory());
  }

  public long time; // required
  public int securityId; // required
  public long qty; // required
  public String fillPrice; // optional

  /** The set of fields this struct contains, along with convenience methods for finding and manipulating them. */
  public enum _Fields implements org.apache.thrift.TFieldIdEnum {
    TIME((short)1, "time"),
    SECURITY_ID((short)2, "securityId"),
    QTY((short)3, "qty"),
    FILL_PRICE((short)4, "fillPrice");

    private static final Map<String, _Fields> byName = new HashMap<String, _Fields>();

    static {
      for (_Fields field : EnumSet.allOf(_Fields.class)) {
        byName.put(field.getFieldName(), field);
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, or null if its not found.
     */
    public static _Fields findByThriftId(int fieldId) {
      switch(fieldId) {
        case 1: // TIME
          return TIME;
        case 2: // SECURITY_ID
          return SECURITY_ID;
        case 3: // QTY
          return QTY;
        case 4: // FILL_PRICE
          return FILL_PRICE;
        default:
          return null;
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, throwing an exception
     * if it is not found.
     */
    public static _Fields findByThriftIdOrThrow(int fieldId) {
      _Fields fields = findByThriftId(fieldId);
      if (fields == null) throw new IllegalArgumentException("Field " + fieldId + " doesn't exist!");
      return fields;
    }

    /**
     * Find the _Fields constant that matches name, or null if its not found.
     */
    public static _Fields findByName(String name) {
      return byName.get(name);
    }

    private final short _thriftId;
    private final String _fieldName;

    _Fields(short thriftId, String fieldName) {
      _thriftId = thriftId;
      _fieldName = fieldName;
    }

    public short getThriftFieldId() {
      return _thriftId;
    }

    public String getFieldName() {
      return _fieldName;
    }
  }

  // isset id assignments
  private static final int __TIME_ISSET_ID = 0;
  private static final int __SECURITYID_ISSET_ID = 1;
  private static final int __QTY_ISSET_ID = 2;
  private byte __isset_bitfield = 0;
  private _Fields optionals[] = {_Fields.FILL_PRICE};
  public static final Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> metaDataMap;
  static {
    Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> tmpMap = new EnumMap<_Fields, org.apache.thrift.meta_data.FieldMetaData>(_Fields.class);
    tmpMap.put(_Fields.TIME, new org.apache.thrift.meta_data.FieldMetaData("time", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I64)));
    tmpMap.put(_Fields.SECURITY_ID, new org.apache.thrift.meta_data.FieldMetaData("securityId", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I32)));
    tmpMap.put(_Fields.QTY, new org.apache.thrift.meta_data.FieldMetaData("qty", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I64)));
    tmpMap.put(_Fields.FILL_PRICE, new org.apache.thrift.meta_data.FieldMetaData("fillPrice", org.apache.thrift.TFieldRequirementType.OPTIONAL, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.STRING)));
    metaDataMap = Collections.unmodifiableMap(tmpMap);
    org.apache.thrift.meta_data.FieldMetaData.addStructMetaDataMap(MarketSellOrder.class, metaDataMap);
  }

  public MarketSellOrder() {
  }

  public MarketSellOrder(
    long time,
    int securityId,
    long qty)
  {
    this();
    this.time = time;
    setTimeIsSet(true);
    this.securityId = securityId;
    setSecurityIdIsSet(true);
    this.qty = qty;
    setQtyIsSet(true);
  }

  /**
   * Performs a deep copy on <i>other</i>.
   */
  public MarketSellOrder(MarketSellOrder other) {
    __isset_bitfield = other.__isset_bitfield;
    this.time = other.time;
    this.securityId = other.securityId;
    this.qty = other.qty;
    if (other.isSetFillPrice()) {
      this.fillPrice = other.fillPrice;
    }
  }

  public MarketSellOrder deepCopy() {
    return new MarketSellOrder(this);
  }

  @Override
  public void clear() {
    setTimeIsSet(false);
    this.time = 0;
    setSecurityIdIsSet(false);
    this.securityId = 0;
    setQtyIsSet(false);
    this.qty = 0;
    this.fillPrice = null;
  }

  public long getTime() {
    return this.time;
  }

  public MarketSellOrder setTime(long time) {
    this.time = time;
    setTimeIsSet(true);
    return this;
  }

  public void unsetTime() {
    __isset_bitfield = EncodingUtils.clearBit(__isset_bitfield, __TIME_ISSET_ID);
  }

  /** Returns true if field time is set (has been assigned a value) and false otherwise */
  public boolean isSetTime() {
    return EncodingUtils.testBit(__isset_bitfield, __TIME_ISSET_ID);
  }

  public void setTimeIsSet(boolean value) {
    __isset_bitfield = EncodingUtils.setBit(__isset_bitfield, __TIME_ISSET_ID, value);
  }

  public int getSecurityId() {
    return this.securityId;
  }

  public MarketSellOrder setSecurityId(int securityId) {
    this.securityId = securityId;
    setSecurityIdIsSet(true);
    return this;
  }

  public void unsetSecurityId() {
    __isset_bitfield = EncodingUtils.clearBit(__isset_bitfield, __SECURITYID_ISSET_ID);
  }

  /** Returns true if field securityId is set (has been assigned a value) and false otherwise */
  public boolean isSetSecurityId() {
    return EncodingUtils.testBit(__isset_bitfield, __SECURITYID_ISSET_ID);
  }

  public void setSecurityIdIsSet(boolean value) {
    __isset_bitfield = EncodingUtils.setBit(__isset_bitfield, __SECURITYID_ISSET_ID, value);
  }

  public long getQty() {
    return this.qty;
  }

  public MarketSellOrder setQty(long qty) {
    this.qty = qty;
    setQtyIsSet(true);
    return this;
  }

  public void unsetQty() {
    __isset_bitfield = EncodingUtils.clearBit(__isset_bitfield, __QTY_ISSET_ID);
  }

  /** Returns true if field qty is set (has been assigned a value) and false otherwise */
  public boolean isSetQty() {
    return EncodingUtils.testBit(__isset_bitfield, __QTY_ISSET_ID);
  }

  public void setQtyIsSet(boolean value) {
    __isset_bitfield = EncodingUtils.setBit(__isset_bitfield, __QTY_ISSET_ID, value);
  }

  public String getFillPrice() {
    return this.fillPrice;
  }

  public MarketSellOrder setFillPrice(String fillPrice) {
    this.fillPrice = fillPrice;
    return this;
  }

  public void unsetFillPrice() {
    this.fillPrice = null;
  }

  /** Returns true if field fillPrice is set (has been assigned a value) and false otherwise */
  public boolean isSetFillPrice() {
    return this.fillPrice != null;
  }

  public void setFillPriceIsSet(boolean value) {
    if (!value) {
      this.fillPrice = null;
    }
  }

  public void setFieldValue(_Fields field, Object value) {
    switch (field) {
    case TIME:
      if (value == null) {
        unsetTime();
      } else {
        setTime((Long)value);
      }
      break;

    case SECURITY_ID:
      if (value == null) {
        unsetSecurityId();
      } else {
        setSecurityId((Integer)value);
      }
      break;

    case QTY:
      if (value == null) {
        unsetQty();
      } else {
        setQty((Long)value);
      }
      break;

    case FILL_PRICE:
      if (value == null) {
        unsetFillPrice();
      } else {
        setFillPrice((String)value);
      }
      break;

    }
  }

  public Object getFieldValue(_Fields field) {
    switch (field) {
    case TIME:
      return Long.valueOf(getTime());

    case SECURITY_ID:
      return Integer.valueOf(getSecurityId());

    case QTY:
      return Long.valueOf(getQty());

    case FILL_PRICE:
      return getFillPrice();

    }
    throw new IllegalStateException();
  }

  /** Returns true if field corresponding to fieldID is set (has been assigned a value) and false otherwise */
  public boolean isSet(_Fields field) {
    if (field == null) {
      throw new IllegalArgumentException();
    }

    switch (field) {
    case TIME:
      return isSetTime();
    case SECURITY_ID:
      return isSetSecurityId();
    case QTY:
      return isSetQty();
    case FILL_PRICE:
      return isSetFillPrice();
    }
    throw new IllegalStateException();
  }

  @Override
  public boolean equals(Object that) {
    if (that == null)
      return false;
    if (that instanceof MarketSellOrder)
      return this.equals((MarketSellOrder)that);
    return false;
  }

  public boolean equals(MarketSellOrder that) {
    if (that == null)
      return false;

    boolean this_present_time = true;
    boolean that_present_time = true;
    if (this_present_time || that_present_time) {
      if (!(this_present_time && that_present_time))
        return false;
      if (this.time != that.time)
        return false;
    }

    boolean this_present_securityId = true;
    boolean that_present_securityId = true;
    if (this_present_securityId || that_present_securityId) {
      if (!(this_present_securityId && that_present_securityId))
        return false;
      if (this.securityId != that.securityId)
        return false;
    }

    boolean this_present_qty = true;
    boolean that_present_qty = true;
    if (this_present_qty || that_present_qty) {
      if (!(this_present_qty && that_present_qty))
        return false;
      if (this.qty != that.qty)
        return false;
    }

    boolean this_present_fillPrice = true && this.isSetFillPrice();
    boolean that_present_fillPrice = true && that.isSetFillPrice();
    if (this_present_fillPrice || that_present_fillPrice) {
      if (!(this_present_fillPrice && that_present_fillPrice))
        return false;
      if (!this.fillPrice.equals(that.fillPrice))
        return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    return 0;
  }

  public int compareTo(MarketSellOrder other) {
    if (!getClass().equals(other.getClass())) {
      return getClass().getName().compareTo(other.getClass().getName());
    }

    int lastComparison = 0;
    MarketSellOrder typedOther = (MarketSellOrder)other;

    lastComparison = Boolean.valueOf(isSetTime()).compareTo(typedOther.isSetTime());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetTime()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.time, typedOther.time);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetSecurityId()).compareTo(typedOther.isSetSecurityId());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetSecurityId()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.securityId, typedOther.securityId);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetQty()).compareTo(typedOther.isSetQty());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetQty()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.qty, typedOther.qty);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetFillPrice()).compareTo(typedOther.isSetFillPrice());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetFillPrice()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.fillPrice, typedOther.fillPrice);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }

  public _Fields fieldForId(int fieldId) {
    return _Fields.findByThriftId(fieldId);
  }

  public void read(org.apache.thrift.protocol.TProtocol iprot) throws org.apache.thrift.TException {
    schemes.get(iprot.getScheme()).getScheme().read(iprot, this);
  }

  public void write(org.apache.thrift.protocol.TProtocol oprot) throws org.apache.thrift.TException {
    schemes.get(oprot.getScheme()).getScheme().write(oprot, this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("MarketSellOrder(");
    boolean first = true;

    sb.append("time:");
    sb.append(this.time);
    first = false;
    if (!first) sb.append(", ");
    sb.append("securityId:");
    sb.append(this.securityId);
    first = false;
    if (!first) sb.append(", ");
    sb.append("qty:");
    sb.append(this.qty);
    first = false;
    if (isSetFillPrice()) {
      if (!first) sb.append(", ");
      sb.append("fillPrice:");
      if (this.fillPrice == null) {
        sb.append("null");
      } else {
        sb.append(this.fillPrice);
      }
      first = false;
    }
    sb.append(")");
    return sb.toString();
  }

  public void validate() throws org.apache.thrift.TException {
    // check for required fields
    // check for sub-struct validity
  }

  private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
    try {
      write(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(out)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private void readObject(java.io.ObjectInputStream in) throws java.io.IOException, ClassNotFoundException {
    try {
      // it doesn't seem like you should have to do this, but java serialization is wacky, and doesn't call the default constructor.
      __isset_bitfield = 0;
      read(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(in)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private static class MarketSellOrderStandardSchemeFactory implements SchemeFactory {
    public MarketSellOrderStandardScheme getScheme() {
      return new MarketSellOrderStandardScheme();
    }
  }

  private static class MarketSellOrderStandardScheme extends StandardScheme<MarketSellOrder> {

    public void read(org.apache.thrift.protocol.TProtocol iprot, MarketSellOrder struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TField schemeField;
      iprot.readStructBegin();
      while (true)
      {
        schemeField = iprot.readFieldBegin();
        if (schemeField.type == org.apache.thrift.protocol.TType.STOP) { 
          break;
        }
        switch (schemeField.id) {
          case 1: // TIME
            if (schemeField.type == org.apache.thrift.protocol.TType.I64) {
              struct.time = iprot.readI64();
              struct.setTimeIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 2: // SECURITY_ID
            if (schemeField.type == org.apache.thrift.protocol.TType.I32) {
              struct.securityId = iprot.readI32();
              struct.setSecurityIdIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 3: // QTY
            if (schemeField.type == org.apache.thrift.protocol.TType.I64) {
              struct.qty = iprot.readI64();
              struct.setQtyIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 4: // FILL_PRICE
            if (schemeField.type == org.apache.thrift.protocol.TType.STRING) {
              struct.fillPrice = iprot.readString();
              struct.setFillPriceIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          default:
            org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
        }
        iprot.readFieldEnd();
      }
      iprot.readStructEnd();

      // check for required fields of primitive type, which can't be checked in the validate method
      struct.validate();
    }

    public void write(org.apache.thrift.protocol.TProtocol oprot, MarketSellOrder struct) throws org.apache.thrift.TException {
      struct.validate();

      oprot.writeStructBegin(STRUCT_DESC);
      oprot.writeFieldBegin(TIME_FIELD_DESC);
      oprot.writeI64(struct.time);
      oprot.writeFieldEnd();
      oprot.writeFieldBegin(SECURITY_ID_FIELD_DESC);
      oprot.writeI32(struct.securityId);
      oprot.writeFieldEnd();
      oprot.writeFieldBegin(QTY_FIELD_DESC);
      oprot.writeI64(struct.qty);
      oprot.writeFieldEnd();
      if (struct.fillPrice != null) {
        if (struct.isSetFillPrice()) {
          oprot.writeFieldBegin(FILL_PRICE_FIELD_DESC);
          oprot.writeString(struct.fillPrice);
          oprot.writeFieldEnd();
        }
      }
      oprot.writeFieldStop();
      oprot.writeStructEnd();
    }

  }

  private static class MarketSellOrderTupleSchemeFactory implements SchemeFactory {
    public MarketSellOrderTupleScheme getScheme() {
      return new MarketSellOrderTupleScheme();
    }
  }

  private static class MarketSellOrderTupleScheme extends TupleScheme<MarketSellOrder> {

    @Override
    public void write(org.apache.thrift.protocol.TProtocol prot, MarketSellOrder struct) throws org.apache.thrift.TException {
      TTupleProtocol oprot = (TTupleProtocol) prot;
      BitSet optionals = new BitSet();
      if (struct.isSetTime()) {
        optionals.set(0);
      }
      if (struct.isSetSecurityId()) {
        optionals.set(1);
      }
      if (struct.isSetQty()) {
        optionals.set(2);
      }
      if (struct.isSetFillPrice()) {
        optionals.set(3);
      }
      oprot.writeBitSet(optionals, 4);
      if (struct.isSetTime()) {
        oprot.writeI64(struct.time);
      }
      if (struct.isSetSecurityId()) {
        oprot.writeI32(struct.securityId);
      }
      if (struct.isSetQty()) {
        oprot.writeI64(struct.qty);
      }
      if (struct.isSetFillPrice()) {
        oprot.writeString(struct.fillPrice);
      }
    }

    @Override
    public void read(org.apache.thrift.protocol.TProtocol prot, MarketSellOrder struct) throws org.apache.thrift.TException {
      TTupleProtocol iprot = (TTupleProtocol) prot;
      BitSet incoming = iprot.readBitSet(4);
      if (incoming.get(0)) {
        struct.time = iprot.readI64();
        struct.setTimeIsSet(true);
      }
      if (incoming.get(1)) {
        struct.securityId = iprot.readI32();
        struct.setSecurityIdIsSet(true);
      }
      if (incoming.get(2)) {
        struct.qty = iprot.readI64();
        struct.setQtyIsSet(true);
      }
      if (incoming.get(3)) {
        struct.fillPrice = iprot.readString();
        struct.setFillPriceIsSet(true);
      }
    }
  }

}

