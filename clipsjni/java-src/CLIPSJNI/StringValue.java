package CLIPSJNI;

public class StringValue extends PrimitiveValue
  {
   /****************/
   /* StringValue: */
   /****************/
   public StringValue()
     {
      super(new String(""));
     }

   /****************/
   /* StringValue: */
   /****************/
   public StringValue(
     String value)
     {
      super(value);
     }
     
   /****************/
   /* stringValue: */
   /****************/
   public String stringValue()
     {
      return (String) getValue();
     }

   /*************/
   /* toString: */
   /*************/
   public String toString()
     {        
      return "\"" + super.toString() + "\"";
     }

  }
