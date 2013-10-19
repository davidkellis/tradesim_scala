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

public class SplitAdjustment implements org.apache.thrift.TBase<SplitAdjustment, SplitAdjustment._Fields>, java.io.Serializable, Cloneable {
  private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new org.apache.thrift.protocol.TStruct("SplitAdjustment");

  private static final org.apache.thrift.protocol.TField SECURITY_ID_FIELD_DESC = new org.apache.thrift.protocol.TField("securityId", org.apache.thrift.protocol.TType.I32, (short)1);
  private static final org.apache.thrift.protocol.TField EX_DATE_FIELD_DESC = new org.apache.thrift.protocol.TField("exDate", org.apache.thrift.protocol.TType.I64, (short)2);
  private static final org.apache.thrift.protocol.TField RATIO_FIELD_DESC = new org.apache.thrift.protocol.TField("ratio", org.apache.thrift.protocol.TType.STRING, (short)3);
  private static final org.apache.thrift.protocol.TField ADJUSTMENT_TIME_FIELD_DESC = new org.apache.thrift.protocol.TField("adjustmentTime", org.apache.thrift.protocol.TType.I64, (short)4);
  private static final org.apache.thrift.protocol.TField SHARE_QTY_DELTA_FIELD_DESC = new org.apache.thrift.protocol.TField("shareQtyDelta", org.apache.thrift.protocol.TType.I64, (short)5);
  private static final org.apache.thrift.protocol.TField CASH_PAYOUT_FIELD_DESC = new org.apache.thrift.protocol.TField("cashPayout", org.apache.thrift.protocol.TType.STRING, (short)6);

  private static final Map<Class<? extends IScheme>, SchemeFactory> schemes = new HashMap<Class<? extends IScheme>, SchemeFactory>();
  static {
    schemes.put(StandardScheme.class, new SplitAdjustmentStandardSchemeFactory());
    schemes.put(TupleScheme.class, new SplitAdjustmentTupleSchemeFactory());
  }

  public int securityId; // required
  public long exDate; // required
  public String ratio; // required
  public long adjustmentTime; // required
  public long shareQtyDelta; // required
  public String cashPayout; // required

  /** The set of fields this struct contains, along with convenience methods for finding and manipulating them. */
  public enum _Fields implements org.apache.thrift.TFieldIdEnum {
    SECURITY_ID((short)1, "securityId"),
    EX_DATE((short)2, "exDate"),
    RATIO((short)3, "ratio"),
    ADJUSTMENT_TIME((short)4, "adjustmentTime"),
    SHARE_QTY_DELTA((short)5, "shareQtyDelta"),
    CASH_PAYOUT((short)6, "cashPayout");

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
        case 1: // SECURITY_ID
          return SECURITY_ID;
        case 2: // EX_DATE
          return EX_DATE;
        case 3: // RATIO
          return RATIO;
        case 4: // ADJUSTMENT_TIME
          return ADJUSTMENT_TIME;
        case 5: // SHARE_QTY_DELTA
          return SHARE_QTY_DELTA;
        case 6: // CASH_PAYOUT
          return CASH_PAYOUT;
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
  private static final int __SECURITYID_ISSET_ID = 0;
  private static final int __EXDATE_ISSET_ID = 1;
  private static final int __ADJUSTMENTTIME_ISSET_ID = 2;
  private static final int __SHAREQTYDELTA_ISSET_ID = 3;
  private byte __isset_bitfield = 0;
  public static final Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> metaDataMap;
  static {
    Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> tmpMap = new EnumMap<_Fields, org.apache.thrift.meta_data.FieldMetaData>(_Fields.class);
    tmpMap.put(_Fields.SECURITY_ID, new org.apache.thrift.meta_data.FieldMetaData("securityId", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I32)));
    tmpMap.put(_Fields.EX_DATE, new org.apache.thrift.meta_data.FieldMetaData("exDate", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I64)));
    tmpMap.put(_Fields.RATIO, new org.apache.thrift.meta_data.FieldMetaData("ratio", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.STRING)));
    tmpMap.put(_Fields.ADJUSTMENT_TIME, new org.apache.thrift.meta_data.FieldMetaData("adjustmentTime", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I64)));
    tmpMap.put(_Fields.SHARE_QTY_DELTA, new org.apache.thrift.meta_data.FieldMetaData("shareQtyDelta", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I64)));
    tmpMap.put(_Fields.CASH_PAYOUT, new org.apache.thrift.meta_data.FieldMetaData("cashPayout", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.STRING)));
    metaDataMap = Collections.unmodifiableMap(tmpMap);
    org.apache.thrift.meta_data.FieldMetaData.addStructMetaDataMap(SplitAdjustment.class, metaDataMap);
  }

  public SplitAdjustment() {
  }

  public SplitAdjustment(
    int securityId,
    long exDate,
    String ratio,
    long adjustmentTime,
    long shareQtyDelta,
    String cashPayout)
  {
    this();
    this.securityId = securityId;
    setSecurityIdIsSet(true);
    this.exDate = exDate;
    setExDateIsSet(true);
    this.ratio = ratio;
    this.adjustmentTime = adjustmentTime;
    setAdjustmentTimeIsSet(true);
    this.shareQtyDelta = shareQtyDelta;
    setShareQtyDeltaIsSet(true);
    this.cashPayout = cashPayout;
  }

  /**
   * Performs a deep copy on <i>other</i>.
   */
  public SplitAdjustment(SplitAdjustment other) {
    __isset_bitfield = other.__isset_bitfield;
    this.securityId = other.securityId;
    this.exDate = other.exDate;
    if (other.isSetRatio()) {
      this.ratio = other.ratio;
    }
    this.adjustmentTime = other.adjustmentTime;
    this.shareQtyDelta = other.shareQtyDelta;
    if (other.isSetCashPayout()) {
      this.cashPayout = other.cashPayout;
    }
  }

  public SplitAdjustment deepCopy() {
    return new SplitAdjustment(this);
  }

  @Override
  public void clear() {
    setSecurityIdIsSet(false);
    this.securityId = 0;
    setExDateIsSet(false);
    this.exDate = 0;
    this.ratio = null;
    setAdjustmentTimeIsSet(false);
    this.adjustmentTime = 0;
    setShareQtyDeltaIsSet(false);
    this.shareQtyDelta = 0;
    this.cashPayout = null;
  }

  public int getSecurityId() {
    return this.securityId;
  }

  public SplitAdjustment setSecurityId(int securityId) {
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

  public long getExDate() {
    return this.exDate;
  }

  public SplitAdjustment setExDate(long exDate) {
    this.exDate = exDate;
    setExDateIsSet(true);
    return this;
  }

  public void unsetExDate() {
    __isset_bitfield = EncodingUtils.clearBit(__isset_bitfield, __EXDATE_ISSET_ID);
  }

  /** Returns true if field exDate is set (has been assigned a value) and false otherwise */
  public boolean isSetExDate() {
    return EncodingUtils.testBit(__isset_bitfield, __EXDATE_ISSET_ID);
  }

  public void setExDateIsSet(boolean value) {
    __isset_bitfield = EncodingUtils.setBit(__isset_bitfield, __EXDATE_ISSET_ID, value);
  }

  public String getRatio() {
    return this.ratio;
  }

  public SplitAdjustment setRatio(String ratio) {
    this.ratio = ratio;
    return this;
  }

  public void unsetRatio() {
    this.ratio = null;
  }

  /** Returns true if field ratio is set (has been assigned a value) and false otherwise */
  public boolean isSetRatio() {
    return this.ratio != null;
  }

  public void setRatioIsSet(boolean value) {
    if (!value) {
      this.ratio = null;
    }
  }

  public long getAdjustmentTime() {
    return this.adjustmentTime;
  }

  public SplitAdjustment setAdjustmentTime(long adjustmentTime) {
    this.adjustmentTime = adjustmentTime;
    setAdjustmentTimeIsSet(true);
    return this;
  }

  public void unsetAdjustmentTime() {
    __isset_bitfield = EncodingUtils.clearBit(__isset_bitfield, __ADJUSTMENTTIME_ISSET_ID);
  }

  /** Returns true if field adjustmentTime is set (has been assigned a value) and false otherwise */
  public boolean isSetAdjustmentTime() {
    return EncodingUtils.testBit(__isset_bitfield, __ADJUSTMENTTIME_ISSET_ID);
  }

  public void setAdjustmentTimeIsSet(boolean value) {
    __isset_bitfield = EncodingUtils.setBit(__isset_bitfield, __ADJUSTMENTTIME_ISSET_ID, value);
  }

  public long getShareQtyDelta() {
    return this.shareQtyDelta;
  }

  public SplitAdjustment setShareQtyDelta(long shareQtyDelta) {
    this.shareQtyDelta = shareQtyDelta;
    setShareQtyDeltaIsSet(true);
    return this;
  }

  public void unsetShareQtyDelta() {
    __isset_bitfield = EncodingUtils.clearBit(__isset_bitfield, __SHAREQTYDELTA_ISSET_ID);
  }

  /** Returns true if field shareQtyDelta is set (has been assigned a value) and false otherwise */
  public boolean isSetShareQtyDelta() {
    return EncodingUtils.testBit(__isset_bitfield, __SHAREQTYDELTA_ISSET_ID);
  }

  public void setShareQtyDeltaIsSet(boolean value) {
    __isset_bitfield = EncodingUtils.setBit(__isset_bitfield, __SHAREQTYDELTA_ISSET_ID, value);
  }

  public String getCashPayout() {
    return this.cashPayout;
  }

  public SplitAdjustment setCashPayout(String cashPayout) {
    this.cashPayout = cashPayout;
    return this;
  }

  public void unsetCashPayout() {
    this.cashPayout = null;
  }

  /** Returns true if field cashPayout is set (has been assigned a value) and false otherwise */
  public boolean isSetCashPayout() {
    return this.cashPayout != null;
  }

  public void setCashPayoutIsSet(boolean value) {
    if (!value) {
      this.cashPayout = null;
    }
  }

  public void setFieldValue(_Fields field, Object value) {
    switch (field) {
    case SECURITY_ID:
      if (value == null) {
        unsetSecurityId();
      } else {
        setSecurityId((Integer)value);
      }
      break;

    case EX_DATE:
      if (value == null) {
        unsetExDate();
      } else {
        setExDate((Long)value);
      }
      break;

    case RATIO:
      if (value == null) {
        unsetRatio();
      } else {
        setRatio((String)value);
      }
      break;

    case ADJUSTMENT_TIME:
      if (value == null) {
        unsetAdjustmentTime();
      } else {
        setAdjustmentTime((Long)value);
      }
      break;

    case SHARE_QTY_DELTA:
      if (value == null) {
        unsetShareQtyDelta();
      } else {
        setShareQtyDelta((Long)value);
      }
      break;

    case CASH_PAYOUT:
      if (value == null) {
        unsetCashPayout();
      } else {
        setCashPayout((String)value);
      }
      break;

    }
  }

  public Object getFieldValue(_Fields field) {
    switch (field) {
    case SECURITY_ID:
      return Integer.valueOf(getSecurityId());

    case EX_DATE:
      return Long.valueOf(getExDate());

    case RATIO:
      return getRatio();

    case ADJUSTMENT_TIME:
      return Long.valueOf(getAdjustmentTime());

    case SHARE_QTY_DELTA:
      return Long.valueOf(getShareQtyDelta());

    case CASH_PAYOUT:
      return getCashPayout();

    }
    throw new IllegalStateException();
  }

  /** Returns true if field corresponding to fieldID is set (has been assigned a value) and false otherwise */
  public boolean isSet(_Fields field) {
    if (field == null) {
      throw new IllegalArgumentException();
    }

    switch (field) {
    case SECURITY_ID:
      return isSetSecurityId();
    case EX_DATE:
      return isSetExDate();
    case RATIO:
      return isSetRatio();
    case ADJUSTMENT_TIME:
      return isSetAdjustmentTime();
    case SHARE_QTY_DELTA:
      return isSetShareQtyDelta();
    case CASH_PAYOUT:
      return isSetCashPayout();
    }
    throw new IllegalStateException();
  }

  @Override
  public boolean equals(Object that) {
    if (that == null)
      return false;
    if (that instanceof SplitAdjustment)
      return this.equals((SplitAdjustment)that);
    return false;
  }

  public boolean equals(SplitAdjustment that) {
    if (that == null)
      return false;

    boolean this_present_securityId = true;
    boolean that_present_securityId = true;
    if (this_present_securityId || that_present_securityId) {
      if (!(this_present_securityId && that_present_securityId))
        return false;
      if (this.securityId != that.securityId)
        return false;
    }

    boolean this_present_exDate = true;
    boolean that_present_exDate = true;
    if (this_present_exDate || that_present_exDate) {
      if (!(this_present_exDate && that_present_exDate))
        return false;
      if (this.exDate != that.exDate)
        return false;
    }

    boolean this_present_ratio = true && this.isSetRatio();
    boolean that_present_ratio = true && that.isSetRatio();
    if (this_present_ratio || that_present_ratio) {
      if (!(this_present_ratio && that_present_ratio))
        return false;
      if (!this.ratio.equals(that.ratio))
        return false;
    }

    boolean this_present_adjustmentTime = true;
    boolean that_present_adjustmentTime = true;
    if (this_present_adjustmentTime || that_present_adjustmentTime) {
      if (!(this_present_adjustmentTime && that_present_adjustmentTime))
        return false;
      if (this.adjustmentTime != that.adjustmentTime)
        return false;
    }

    boolean this_present_shareQtyDelta = true;
    boolean that_present_shareQtyDelta = true;
    if (this_present_shareQtyDelta || that_present_shareQtyDelta) {
      if (!(this_present_shareQtyDelta && that_present_shareQtyDelta))
        return false;
      if (this.shareQtyDelta != that.shareQtyDelta)
        return false;
    }

    boolean this_present_cashPayout = true && this.isSetCashPayout();
    boolean that_present_cashPayout = true && that.isSetCashPayout();
    if (this_present_cashPayout || that_present_cashPayout) {
      if (!(this_present_cashPayout && that_present_cashPayout))
        return false;
      if (!this.cashPayout.equals(that.cashPayout))
        return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    return 0;
  }

  public int compareTo(SplitAdjustment other) {
    if (!getClass().equals(other.getClass())) {
      return getClass().getName().compareTo(other.getClass().getName());
    }

    int lastComparison = 0;
    SplitAdjustment typedOther = (SplitAdjustment)other;

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
    lastComparison = Boolean.valueOf(isSetExDate()).compareTo(typedOther.isSetExDate());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetExDate()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.exDate, typedOther.exDate);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetRatio()).compareTo(typedOther.isSetRatio());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetRatio()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.ratio, typedOther.ratio);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetAdjustmentTime()).compareTo(typedOther.isSetAdjustmentTime());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetAdjustmentTime()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.adjustmentTime, typedOther.adjustmentTime);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetShareQtyDelta()).compareTo(typedOther.isSetShareQtyDelta());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetShareQtyDelta()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.shareQtyDelta, typedOther.shareQtyDelta);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetCashPayout()).compareTo(typedOther.isSetCashPayout());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetCashPayout()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.cashPayout, typedOther.cashPayout);
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
    StringBuilder sb = new StringBuilder("SplitAdjustment(");
    boolean first = true;

    sb.append("securityId:");
    sb.append(this.securityId);
    first = false;
    if (!first) sb.append(", ");
    sb.append("exDate:");
    sb.append(this.exDate);
    first = false;
    if (!first) sb.append(", ");
    sb.append("ratio:");
    if (this.ratio == null) {
      sb.append("null");
    } else {
      sb.append(this.ratio);
    }
    first = false;
    if (!first) sb.append(", ");
    sb.append("adjustmentTime:");
    sb.append(this.adjustmentTime);
    first = false;
    if (!first) sb.append(", ");
    sb.append("shareQtyDelta:");
    sb.append(this.shareQtyDelta);
    first = false;
    if (!first) sb.append(", ");
    sb.append("cashPayout:");
    if (this.cashPayout == null) {
      sb.append("null");
    } else {
      sb.append(this.cashPayout);
    }
    first = false;
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

  private static class SplitAdjustmentStandardSchemeFactory implements SchemeFactory {
    public SplitAdjustmentStandardScheme getScheme() {
      return new SplitAdjustmentStandardScheme();
    }
  }

  private static class SplitAdjustmentStandardScheme extends StandardScheme<SplitAdjustment> {

    public void read(org.apache.thrift.protocol.TProtocol iprot, SplitAdjustment struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TField schemeField;
      iprot.readStructBegin();
      while (true)
      {
        schemeField = iprot.readFieldBegin();
        if (schemeField.type == org.apache.thrift.protocol.TType.STOP) { 
          break;
        }
        switch (schemeField.id) {
          case 1: // SECURITY_ID
            if (schemeField.type == org.apache.thrift.protocol.TType.I32) {
              struct.securityId = iprot.readI32();
              struct.setSecurityIdIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 2: // EX_DATE
            if (schemeField.type == org.apache.thrift.protocol.TType.I64) {
              struct.exDate = iprot.readI64();
              struct.setExDateIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 3: // RATIO
            if (schemeField.type == org.apache.thrift.protocol.TType.STRING) {
              struct.ratio = iprot.readString();
              struct.setRatioIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 4: // ADJUSTMENT_TIME
            if (schemeField.type == org.apache.thrift.protocol.TType.I64) {
              struct.adjustmentTime = iprot.readI64();
              struct.setAdjustmentTimeIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 5: // SHARE_QTY_DELTA
            if (schemeField.type == org.apache.thrift.protocol.TType.I64) {
              struct.shareQtyDelta = iprot.readI64();
              struct.setShareQtyDeltaIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 6: // CASH_PAYOUT
            if (schemeField.type == org.apache.thrift.protocol.TType.STRING) {
              struct.cashPayout = iprot.readString();
              struct.setCashPayoutIsSet(true);
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

    public void write(org.apache.thrift.protocol.TProtocol oprot, SplitAdjustment struct) throws org.apache.thrift.TException {
      struct.validate();

      oprot.writeStructBegin(STRUCT_DESC);
      oprot.writeFieldBegin(SECURITY_ID_FIELD_DESC);
      oprot.writeI32(struct.securityId);
      oprot.writeFieldEnd();
      oprot.writeFieldBegin(EX_DATE_FIELD_DESC);
      oprot.writeI64(struct.exDate);
      oprot.writeFieldEnd();
      if (struct.ratio != null) {
        oprot.writeFieldBegin(RATIO_FIELD_DESC);
        oprot.writeString(struct.ratio);
        oprot.writeFieldEnd();
      }
      oprot.writeFieldBegin(ADJUSTMENT_TIME_FIELD_DESC);
      oprot.writeI64(struct.adjustmentTime);
      oprot.writeFieldEnd();
      oprot.writeFieldBegin(SHARE_QTY_DELTA_FIELD_DESC);
      oprot.writeI64(struct.shareQtyDelta);
      oprot.writeFieldEnd();
      if (struct.cashPayout != null) {
        oprot.writeFieldBegin(CASH_PAYOUT_FIELD_DESC);
        oprot.writeString(struct.cashPayout);
        oprot.writeFieldEnd();
      }
      oprot.writeFieldStop();
      oprot.writeStructEnd();
    }

  }

  private static class SplitAdjustmentTupleSchemeFactory implements SchemeFactory {
    public SplitAdjustmentTupleScheme getScheme() {
      return new SplitAdjustmentTupleScheme();
    }
  }

  private static class SplitAdjustmentTupleScheme extends TupleScheme<SplitAdjustment> {

    @Override
    public void write(org.apache.thrift.protocol.TProtocol prot, SplitAdjustment struct) throws org.apache.thrift.TException {
      TTupleProtocol oprot = (TTupleProtocol) prot;
      BitSet optionals = new BitSet();
      if (struct.isSetSecurityId()) {
        optionals.set(0);
      }
      if (struct.isSetExDate()) {
        optionals.set(1);
      }
      if (struct.isSetRatio()) {
        optionals.set(2);
      }
      if (struct.isSetAdjustmentTime()) {
        optionals.set(3);
      }
      if (struct.isSetShareQtyDelta()) {
        optionals.set(4);
      }
      if (struct.isSetCashPayout()) {
        optionals.set(5);
      }
      oprot.writeBitSet(optionals, 6);
      if (struct.isSetSecurityId()) {
        oprot.writeI32(struct.securityId);
      }
      if (struct.isSetExDate()) {
        oprot.writeI64(struct.exDate);
      }
      if (struct.isSetRatio()) {
        oprot.writeString(struct.ratio);
      }
      if (struct.isSetAdjustmentTime()) {
        oprot.writeI64(struct.adjustmentTime);
      }
      if (struct.isSetShareQtyDelta()) {
        oprot.writeI64(struct.shareQtyDelta);
      }
      if (struct.isSetCashPayout()) {
        oprot.writeString(struct.cashPayout);
      }
    }

    @Override
    public void read(org.apache.thrift.protocol.TProtocol prot, SplitAdjustment struct) throws org.apache.thrift.TException {
      TTupleProtocol iprot = (TTupleProtocol) prot;
      BitSet incoming = iprot.readBitSet(6);
      if (incoming.get(0)) {
        struct.securityId = iprot.readI32();
        struct.setSecurityIdIsSet(true);
      }
      if (incoming.get(1)) {
        struct.exDate = iprot.readI64();
        struct.setExDateIsSet(true);
      }
      if (incoming.get(2)) {
        struct.ratio = iprot.readString();
        struct.setRatioIsSet(true);
      }
      if (incoming.get(3)) {
        struct.adjustmentTime = iprot.readI64();
        struct.setAdjustmentTimeIsSet(true);
      }
      if (incoming.get(4)) {
        struct.shareQtyDelta = iprot.readI64();
        struct.setShareQtyDeltaIsSet(true);
      }
      if (incoming.get(5)) {
        struct.cashPayout = iprot.readString();
        struct.setCashPayoutIsSet(true);
      }
    }
  }

}

