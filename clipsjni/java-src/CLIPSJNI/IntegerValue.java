package CLIPSJNI;

public class IntegerValue extends PrimitiveValue
  {
   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue()
     {
      super(new Long(0));
     }

   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue(
     long value)
     {
      super(new Long(value));
     }
     
   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue(
     Long value)
     {
      super(value);
     }

   /****************/
   /* numberValue: */
   /****************/
   public Number numberValue() throws Exception
     {
      return (Number) getValue();
     }

   /*************/
   /* intValue: */
   /*************/
   public int intValue() throws Exception
     {
      return ((Long) getValue()).intValue();
     }

   /**************/
   /* longValue: */
   /**************/
   public long longValue() throws Exception
     {
      return ((Long) getValue()).longValue();
     }
  }
