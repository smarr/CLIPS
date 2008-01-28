package CLIPSJNI;

public class InstanceNameValue extends InstanceValue
  {
   /**********************/
   /* InstanceNameValue: */
   /**********************/
   public InstanceNameValue()
     {
      super(new String(""));
     }

   /**********************/
   /* InstanceNameValue: */
   /**********************/
   public InstanceNameValue(
     String value)
     {
      super(value);
     }
   
   /*************/
   /* toString: */
   /*************/
   public String toString()
     {        
      return "[" + super.toString() + "]";
     }
  }
