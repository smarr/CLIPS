package CLIPSJNI;

import java.util.List;

public abstract class PrimitiveValue
  {
   private Object theValue;
   
   /*******************/
   /* PrimitiveValue: */
   /*******************/
   protected PrimitiveValue(
     Object value)
     {
      theValue = value;
     }

   /*************/
   /* getValue: */
   /*************/
   public Object getValue()
     {
      return theValue;
     }

   /****************/
   /* numberValue: */
   /****************/
   public Number numberValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type NUMBER.");
     }

   /*************/
   /* intValue: */
   /*************/
   public int intValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type INTEGER.");
     }

   /**************/
   /* longValue: */
   /**************/
   public long longValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type INTEGER.");
     }
     
   /***************/
   /* floatValue: */
   /***************/
   public float floatValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type FLOAT.");
     }

   /****************/
   /* doubleValue: */
   /****************/
   public double doubleValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type FLOAT.");
     }

   /****************/
   /* lexemeValue: */
   /****************/
   public String lexemeValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type LEXEME.");
     }
     
   /****************/
   /* symbolValue: */
   /****************/
   public String symbolValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type SYMBOL.");
     }

   /****************/
   /* stringValue: */
   /****************/
   public String stringValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type STRING.");
     }

   /**********************/
   /* instanceNameValue: */
   /**********************/
   public String instanceNameValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not type INSTANCE NAME.");
     }

   /********************/
   /* multifieldValue: */
   /********************/
   public List multifieldValue() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not multifield type.");
     }

   /********************/
   /* get: */
   /********************/
   public PrimitiveValue get(
     int index) throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not multifield type.");
     }

   /********************/
   /* size: */
   /********************/
   public int size() throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not multifield type.");
     }

   /****************/
   /* getFactSlot: */
   /****************/     
   public PrimitiveValue getFactSlot(
     String slotName) throws Exception
     {
      throw new Exception("PrimitiveValue " + this + " is not fact address type.");
     }
     
   /*************/
   /* toString: */
   /*************/
   public String toString()
     {
      if (theValue != null)
        { return theValue.toString(); }
        
      return "";
     }
  }
