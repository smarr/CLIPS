
package CLIPSJNI;

import CLIPSJNI.Router;

public class Environment
  {
   private static final String CLIPSJNI_VERSION = "0.1";

   static { System.loadLibrary("CLIPSJNI"); }

   private long theEnvironment;

   /****************/
   /* Environment: */
   /****************/
   public Environment()
     {
      super();
      theEnvironment = createEnvironment();
     }

   /*********************************************************/
   /* getCLIPSJNIVersion: Gets the CLIPSJNI version number. */
   /*********************************************************/
   public static String getCLIPSJNIVersion() 
     {
      return CLIPSJNI_VERSION;
     }

   /***************************************************/
   /* getCLIPSVersion: Gets the CLIPS version number. */
   /***************************************************/
   public static native String getCLIPSVersion();

   /**************************************************************/
   /* getVersion: Gets the JClips and the CLIPS version numbers. */
   /**************************************************************/
   public static String getVersion() 
     {
      return "CLIPSJNI version " + getCLIPSJNIVersion() + 
              " (CLIPS version " + getCLIPSVersion() + ")";
     }

   /**************************/
   /* getEnvironmentAddress: */
   /**************************/
   public long getEnvironmentAddress()
     { return theEnvironment; }
     
   /***************************/
   /* createCLIPSEnvironment: */
   /***************************/
   private native long createEnvironment();

   /****************************************/
   /* clear: Clears the CLIPS environment. */
   /****************************************/
   private native void clear(long env);

   /**********/
   /* clear: */
   /**********/
   public void clear()
     {
      clear(theEnvironment);
     }

   /****************************************/
   /* reset: Resets the CLIPS environment. */
   /****************************************/
   private native void reset(long env);

   /**********/
   /* reset: */
   /**********/
   public void reset()
     {
      reset(theEnvironment);
     }
     
   /*********/
   /* load: */
   /*********/
   private native void load(long env,String filename);

   /*********/
   /* load: */
   /*********/
   public void load(String filename)
     {
      load(theEnvironment,filename);
     }
     
   /********/
   /* run: */
   /********/
   private native long run(long env,long runLimit);

   /********/
   /* run: */
   /********/
   public long run(
     long runLimit)
     {
      return run(theEnvironment,runLimit);
     }

   /********/
   /* run: */
   /********/
   public long run()
     {
      return run(theEnvironment,-1);
     }

   /*********/
   /* eval: */
   /*********/
   private native PrimitiveValue eval(long env,String evalStr);

   /*********/
   /* eval: */
   /*********/
   public PrimitiveValue eval(String evalStr)
     {
      return eval(theEnvironment,evalStr);
     }

   /**********/
   /* build: */
   /**********/
   private native boolean build(long env,String buildStr);

   /**********/
   /* build: */
   /**********/
   public boolean build(String buildStr)
     {
      return build(theEnvironment,buildStr);
     }

   /*****************/
   /* assertString: */
   /*****************/
   private native FactAddressValue assertString(long env,String factStr);

   /*****************/
   /* assertString: */
   /*****************/
   public FactAddressValue assertString(String factStr)
     {
      return assertString(theEnvironment,factStr);
     }

   /**************/
   /* factIndex: */
   /**************/
   private static native long factIndex(Environment javaEnv,long env,long fact);

   /**************/
   /* factIndex: */
   /**************/
   public static long factIndex(
     FactAddressValue theFact)
     {
      return factIndex(theFact.getEnvironment(),
                       theFact.getEnvironment().getEnvironmentAddress(),
                       theFact.getFactAddress());
     }

   /****************/
   /* getFactSlot: */
   /****************/
   private static native PrimitiveValue getFactSlot(Environment javaEnv,long env,long fact,String slotName);

   /****************/
   /* getFactSlot: */
   /****************/
   public static PrimitiveValue getFactSlot(
     FactAddressValue theFact,
     String slotName)
     {
      return getFactSlot(theFact.getEnvironment(),
                         theFact.getEnvironment().getEnvironmentAddress(),
                         theFact.getFactAddress(),slotName);
     }

   /*****************/
   /* makeInstance: */
   /*****************/
   private native InstanceAddressValue makeInstance(long env,String instanceStr);

   /*****************/
   /* makeInstance: */
   /*****************/
   public InstanceAddressValue makeInstance(String instanceStr)
     {
      return makeInstance(theEnvironment,instanceStr);
     }

   /********************/
   /* getInstanceName: */
   /********************/
   private static native String getInstanceName(Environment javaEnv,long env,long instance);

   /********************/
   /* getInstanceName: */
   /********************/
   public static String getInstanceName(
     InstanceAddressValue theInstance)
     {
      return getInstanceName(theInstance.getEnvironment(),
                             theInstance.getEnvironment().getEnvironmentAddress(),
                             theInstance.getInstanceAddress());
     }

   /******************/
   /* directGetSlot: */
   /******************/
   private static native PrimitiveValue directGetSlot(Environment javaEnv,long env,long instance,String slotName);

   /******************/
   /* directGetSlot: */
   /******************/
   public static PrimitiveValue directGetSlot(
     InstanceAddressValue theInstance,
     String slotName)
     {
      return directGetSlot(theInstance.getEnvironment(),
                           theInstance.getEnvironment().getEnvironmentAddress(),
                           theInstance.getInstanceAddress(),slotName);
     }

   /***********************/
   /* destroyEnvironment: */
   /***********************/
   private native void destroyEnvironment(long env);

   /****************/
   /* commandLoop: */
   /****************/
   private native void commandLoop(long env);
    
   /****************/
   /* commandLoop: */
   /****************/
   public void commandLoop()
     {
      commandLoop(theEnvironment);
     }

   /**************/
   /* addRouter: */
   /**************/
   private native boolean addRouter(long env,String routerName,int priority,Router theRouter);
    
   /**************/
   /* addRouter: */
   /**************/
   public boolean addRouter(
     Router theRouter)
     {
      return addRouter(theEnvironment,theRouter.getName(),theRouter.getPriority(),theRouter);
     }

   /*************/
   /* finalize: */
   /*************/
   protected void finalize() throws Throwable
     {
      try
        {
         /* TBD destroy global router references. */
         destroyEnvironment(theEnvironment); 
        }
      finally
        { super.finalize(); }
     }
     
   /*********/
   /* main: */
   /*********/
   public static void main(String args[])
     {  
      Environment clips;

      clips = new Environment();
      
      clips.commandLoop();
     }  
  }
