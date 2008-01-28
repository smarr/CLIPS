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
     
   /*************/
   /* intValue: */
   /*************/
   public int intValue()
     {
      return ((Long) getValue()).intValue();
     }

   /**************/
   /* longValue: */
   /**************/
   public long longValue()
     {
      return ((Long) getValue()).longValue();
     }

   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue(
     Long value)
     {
      super(value);
     }
  }
