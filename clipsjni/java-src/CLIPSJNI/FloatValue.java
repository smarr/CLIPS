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

   /****************/
   /* numberValue: */
   /****************/
   public Number numberValue() throws Exception
     {
      return (Number) getValue();
     }
     
   /***************/
   /* floatValue: */
   /***************/
   public float floatValue() throws Exception
     {
      return ((Double) getValue()).floatValue();
     }

   /****************/
   /* doubleValue: */
   /****************/
   public double doubleValue() throws Exception
     {
      return ((Double) getValue()).doubleValue();
     }
  }
