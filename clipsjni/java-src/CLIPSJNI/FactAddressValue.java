package CLIPSJNI;

public class FactAddressValue extends PrimitiveValue
  {
   private Environment owner;

   /*********************/
   /* FactAddressValue: */
   /*********************/
   public FactAddressValue(
     long value,
     Environment env)
     {
      super(new Long(value));
      
      owner = env;
      env.incrementFactCount(this);
     }

   /*******************/
   /* getEnvironment: */
   /*******************/
   public Environment getEnvironment()
     { return owner; }
     
   /*******************/
   /* getFactAddress: */
   /*******************/     
   public long getFactAddress()
     { return ((Long) getValue()).longValue(); }

   /****************/
   /* getFactSlot: */
   /****************/     
   public PrimitiveValue getFactSlot(
     String slotName)
     { return Environment.getFactSlot(this,slotName); }

   /*****************/
   /* getFactIndex: */
   /*****************/     
   public long getFactIndex()
     { return Environment.factIndex(this); }
     
   /*************/
   /* toString: */
   /*************/
   public String toString()
     {        
      return "<Fact-" + getFactIndex() + ">";
     }

   /*************/
   /* finalize: */
   /*************/
   protected void finalize() throws Throwable
     {
      try
        {
         owner.decrementFactCount(this);
        }
      finally
        { super.finalize(); }
     }
  }
