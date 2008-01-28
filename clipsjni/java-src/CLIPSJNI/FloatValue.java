package CLIPSJNI;

public class FloatValue extends PrimitiveValue
  {
   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue()
     {
      super(new Double(0.0));
     }

   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue(
     double value)
     {
      super(new Double(value));
     }

   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue(
     Double value)
     {
      super(value);
     }
     
   /*************/
   /* intValue: */
   /*************/
   public int intValue()
     {
      return ((Double) getValue()).intValue();
     }

   /**************/
   /* longValue: */
   /**************/
   public long longValue()
     {
      return ((Double) getValue()).longValue();
     }

  }
