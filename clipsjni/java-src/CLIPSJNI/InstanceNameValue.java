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
     
   /****************/
   /* lexemeValue: */
   /****************/
   public String lexemeValue() throws Exception
     {
      return (String) getValue();
     }
     
   /**********************/
   /* instanceNameValue: */
   /**********************/
   public String instanceNameValue() throws Exception
     {
      return (String) getValue();
     }
   
   /*************/
   /* toString: */
   /*************/
   public String toString()
     {        
      return "[" + super.toString() + "]";
     }
  }
