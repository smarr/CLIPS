package CLIPSJNI;

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
