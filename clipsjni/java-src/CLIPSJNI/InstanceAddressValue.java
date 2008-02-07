package CLIPSJNI;

public class InstanceAddressValue extends InstanceValue
  {
   private Environment owner;

   /*************************/
   /* InstanceAddressValue: */
   /*************************/
   public InstanceAddressValue(
     long value,
     Environment env)
     {
      super(new Long(value));
      
      owner = env;
      env.incrementInstanceCount(this);
     }

   /*******************/
   /* getEnvironment: */
   /*******************/
   public Environment getEnvironment()
     { return owner; }
     
   /***********************/
   /* getInstanceAddress: */
   /***********************/     
   public long getInstanceAddress()
     { return ((Long) getValue()).longValue(); }

   /******************/
   /* directGetSlot: */
   /******************/     
   public PrimitiveValue directGetSlot(
     String slotName)
     { return Environment.directGetSlot(this,slotName); }

   /********************/
   /* getInstanceName: */
   /********************/     
   public String getInstanceName()
     { return Environment.getInstanceName(this); }
     
   /*************/
   /* toString: */
   /*************/
   public String toString()
     {        
      return "<Instance-" + getInstanceName() + ">";
     }

   /*************/
   /* finalize: */
   /*************/
   protected void finalize() throws Throwable
     {
      try
        {
         owner.decrementInstanceCount(this);
        }
      finally
        { super.finalize(); }
     }
  }
