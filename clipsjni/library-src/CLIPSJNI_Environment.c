
#include "CLIPSJNI_Environment.h"

#include "clips.h"

#define CLIPSJNI_DATA 67

struct clipsJNIData
  { 
   int javaExternalAddressID;

   jclass classClass;
   jmethodID classGetCanonicalNameMethod;
   
   jclass longClass;
   jmethodID longInitMethod;

   jclass doubleClass;
   jmethodID doubleInitMethod;
   
   jclass arrayListClass;
   jmethodID arrayListInitMethod;
   jmethodID arrayListAddMethod;

   jclass voidValueClass;
   jmethodID voidValueInitMethod;

   jclass integerValueClass;
   jmethodID integerValueInitMethod;
   jclass floatValueClass;
   jmethodID floatValueInitMethod;

   jclass symbolValueClass;
   jmethodID symbolValueInitMethod;
   jclass stringValueClass;
   jmethodID stringValueInitMethod;
   jclass instanceNameValueClass;
   jmethodID instanceNameValueInitMethod;

   jclass multifieldValueClass;
   jmethodID multifieldValueInitMethod;

   jclass factAddressValueClass;
   jmethodID factAddressValueInitMethod;

   jclass instanceAddressValueClass;
   jmethodID instanceAddressValueInitMethod;
  };

#define CLIPSJNIData(theEnv) ((struct clipsJNIData *) GetEnvironmentData(theEnv,CLIPSJNI_DATA))

static int        QueryJNIRouter(void *,char *);
static int        ExitJNIRouter(void *,int);
static int        PrintJNIRouter(void *,char *,char *);
static int        GetcJNIRouter(void *,char *);
static int        UngetcJNIRouter(void *,int,char *);
static void       DeallocateJNIData(void *);
static jobject    ConvertSingleFieldValue(JNIEnv *,jobject,void *,int,void *);
static jobject    ConvertDataObject(JNIEnv *,jobject,void *,DATA_OBJECT *);
static void      *JLongToPointer(jlong);
static jlong      PointerToJLong(void *);
static void       PrintJavaAddress(void *,char *,void *);
static void       NewJavaAddress(void *,DATA_OBJECT *);
static intBool    CallJavaMethod(void *,DATA_OBJECT *,DATA_OBJECT *);
static intBool    DiscardJavaAddress(void *,void *);

/**********************************************/
/* DeallocateJNIData: Deallocates environment */
/*    data for the JNI functionality.         */
/**********************************************/
static void DeallocateJNIData(
  void *theEnv)
  {
   JNIEnv *env;
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);
  
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->classClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->longClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->doubleClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->arrayListClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->voidValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->integerValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->floatValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->symbolValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->stringValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->instanceNameValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->multifieldValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->factAddressValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->instanceAddressValueClass);
  }

/*************************************************/
/* FindJNIRouter: Query routine for JNI routers. */
/*************************************************/
static int QueryJNIRouter(
  void *theEnv,
  char *logicalName)
  {
   jobject context;
   jclass cls;
   JNIEnv *env;
   jmethodID mid;
   jboolean rv;
   jstring str;
  
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = GetEnvironmentRouterContext(theEnv);
  
   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"query","(Ljava/lang/String;)Z");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return FALSE; }

   str = (*env)->NewStringUTF(env,logicalName);

   rv = (*env)->CallBooleanMethod(env,context,mid,str);
      
   (*env)->DeleteLocalRef(env,str);
      
   return(rv);
  }

/*************************************************/
/* ExitJNIRouter:  Exit routine for JNI routers. */
/*************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int ExitJNIRouter(
  void *theEnv,
  int num)
  {
#if MAC_MCW || IBM_MCW || MAC_XCD
#pragma unused(num)
#endif
#if MAC_MCW || IBM_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
   /* TBD deallocate global context reference */
   return(1);
  }

/**************************************************/
/* PrintJNIRouter: Print routine for JNI routers. */
/**************************************************/
static int PrintJNIRouter(
  void *theEnv,
  char *logicalName,
  char *str)
  {
   jobject context;
   JNIEnv *env;
   jmethodID mid;
   jclass cls;
   jstring str1, str2;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = GetEnvironmentRouterContext(theEnv);

   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"print","(Ljava/lang/String;Ljava/lang/String;)V");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return FALSE; }

   str1 = (*env)->NewStringUTF(env,logicalName);
   str2 = (*env)->NewStringUTF(env,str);

   (*env)->CallVoidMethod(env,context,mid,str1,str2);

   (*env)->DeleteLocalRef(env,str1);
   (*env)->DeleteLocalRef(env,str2);
   
   return(1);
  }

/************************************************/
/* GetcJNIRouter: Getc routine for JNI routers. */
/************************************************/
static int GetcJNIRouter(
  void *theEnv,
  char *logicalName)
  {
   jint theChar;
   jobject context;
   JNIEnv *env;
   jmethodID mid;
   jclass cls;
   jstring str;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = GetEnvironmentRouterContext(theEnv);

   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"getchar","(Ljava/lang/String;)I");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return -1; }

   str = (*env)->NewStringUTF(env,logicalName);

   theChar = (*env)->CallIntMethod(env,context,mid,str);

   (*env)->DeleteLocalRef(env,str);

   return((int) theChar);
  }

/****************************************************/
/* UngetcJNIRouter: Ungetc routine for JNI routers. */
/****************************************************/
static int UngetcJNIRouter(
  void *theEnv,
  int ch,
  char *logicalName)
  {
   jint theChar;
   jobject context;
   JNIEnv *env;
   jmethodID mid;
   jclass cls;
   jstring str;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = GetEnvironmentRouterContext(theEnv);

   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"ungetchar","(Ljava/lang/String;I)I");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return -1; }

   str = (*env)->NewStringUTF(env,logicalName);

   theChar = (*env)->CallIntMethod(env,context,mid,(jint) ch,str);

   (*env)->DeleteLocalRef(env,str);

   return((int) theChar);
  }

/*******************************************************/
/* Java_CLIPSJNI_Environment_getCLIPSVersion: Native   */
/*   function for the CLIPSJNI getCLIPSVersion method. */
/* Class:     CLIPSJNI_Environment                     */
/* Method:    getCLIPSVersion                          */
/* Signature: ()Ljava/lang/String;                     */
/*******************************************************/
JNIEXPORT jstring JNICALL Java_CLIPSJNI_Environment_getCLIPSVersion(
  JNIEnv *env, 
  jclass cls) 
  {
   return (*env)->NewStringUTF(env,VERSION_STRING);
  }

/************************************************/
/* Java_CLIPSJNI_Environment_createEnvironment: */
/*                                              */
/*    Class:     CLIPSJNI_Environment           */
/*    Method:    createEnvironment              */
/*    Signature: ()J                            */
/************************************************/
JNIEXPORT jlong JNICALL Java_CLIPSJNI_Environment_createEnvironment(
  JNIEnv *env, 
  jobject obj)
  {
   void *theEnv;
   jclass theClassClass; 
   jmethodID theClassGetCanonicalNameMethod;
   jclass theLongClass; 
   jmethodID theLongInitMethod;
   jclass theDoubleClass; 
   jmethodID theDoubleInitMethod;
   jclass theArrayListClass; 
   jmethodID theArrayListInitMethod, theArrayListAddMethod;
   jclass theVoidValueClass;
   jmethodID theVoidValueInitMethod;
   jclass theIntegerValueClass, theFloatValueClass;
   jmethodID theIntegerValueInitMethod, theFloatValueInitMethod;
   jclass theSymbolValueClass, theStringValueClass, theInstanceNameValueClass;
   jmethodID theSymbolValueInitMethod, theStringValueInitMethod, theInstanceNameValueInitMethod;
   jclass theMultifieldValueClass;
   jmethodID theMultifieldValueInitMethod;
   jclass theFactAddressValueClass;
   jmethodID theFactAddressValueInitMethod;
   jclass theInstanceAddressValueClass;
   jmethodID theInstanceAddressValueInitMethod;
   struct externalAddressType javaPointer = { "java", PrintJavaAddress, PrintJavaAddress, DiscardJavaAddress, NewJavaAddress, CallJavaMethod };

   /*===========================*/
   /* Look up the Java classes. */
   /*===========================*/

   theClassClass = (*env)->FindClass(env,"java/lang/Class"); 
   theLongClass = (*env)->FindClass(env,"java/lang/Long"); 
   theDoubleClass = (*env)->FindClass(env,"java/lang/Double"); 
   theArrayListClass = (*env)->FindClass(env,"java/util/ArrayList"); 
   theVoidValueClass = (*env)->FindClass(env,"CLIPSJNI/VoidValue");
   theIntegerValueClass = (*env)->FindClass(env,"CLIPSJNI/IntegerValue");
   theFloatValueClass = (*env)->FindClass(env,"CLIPSJNI/FloatValue");
   theSymbolValueClass = (*env)->FindClass(env,"CLIPSJNI/SymbolValue");
   theStringValueClass = (*env)->FindClass(env,"CLIPSJNI/StringValue");
   theInstanceNameValueClass = (*env)->FindClass(env,"CLIPSJNI/InstanceNameValue");
   theMultifieldValueClass = (*env)->FindClass(env,"CLIPSJNI/MultifieldValue");
   theFactAddressValueClass = (*env)->FindClass(env,"CLIPSJNI/FactAddressValue");
   theInstanceAddressValueClass = (*env)->FindClass(env,"CLIPSJNI/InstanceAddressValue");
                
   /*=========================================*/
   /* If the Java classes could not be found, */
   /* abort creation of the environment.      */
   /*=========================================*/
   
   if ((theClassClass == NULL) ||
       (theLongClass == NULL) || (theDoubleClass == NULL) ||
       (theArrayListClass == NULL) ||
       (theVoidValueClass == NULL) ||
       (theIntegerValueClass == NULL) || (theFloatValueClass == NULL) ||
       (theSymbolValueClass == NULL) || (theStringValueClass == NULL) || 
       (theInstanceNameValueClass == NULL) ||
       (theMultifieldValueClass == NULL) ||
       (theFactAddressValueClass == NULL) ||
       (theInstanceAddressValueClass == NULL))
     { return((jlong) NULL); }
     
   /*================================*/
   /* Look up the Java init methods. */
   /*================================*/
   
   theClassGetCanonicalNameMethod = (*env)->GetMethodID(env,theClassClass,"getCanonicalName","()Ljava/lang/String;");
   theLongInitMethod = (*env)->GetMethodID(env,theLongClass,"<init>","(J)V");
   theDoubleInitMethod = (*env)->GetMethodID(env,theDoubleClass,"<init>","(D)V");
   theArrayListInitMethod = (*env)->GetMethodID(env,theArrayListClass,"<init>","(I)V");
   theArrayListAddMethod = (*env)->GetMethodID(env,theArrayListClass,"add","(Ljava/lang/Object;)Z");
   theVoidValueInitMethod = (*env)->GetMethodID(env,theVoidValueClass,"<init>","()V");
   theIntegerValueInitMethod = (*env)->GetMethodID(env,theIntegerValueClass,"<init>","(Ljava/lang/Long;)V");
   theFloatValueInitMethod = (*env)->GetMethodID(env,theFloatValueClass,"<init>","(Ljava/lang/Double;)V");
   theSymbolValueInitMethod = (*env)->GetMethodID(env,theSymbolValueClass,"<init>","(Ljava/lang/String;)V");
   theStringValueInitMethod = (*env)->GetMethodID(env,theStringValueClass,"<init>","(Ljava/lang/String;)V");
   theInstanceNameValueInitMethod = (*env)->GetMethodID(env,theInstanceNameValueClass,"<init>","(Ljava/lang/String;)V");
   theMultifieldValueInitMethod = (*env)->GetMethodID(env,theMultifieldValueClass,"<init>","(Ljava/util/List;)V");
   theFactAddressValueInitMethod = (*env)->GetMethodID(env,theFactAddressValueClass,"<init>","(JLCLIPSJNI/Environment;)V");
   theInstanceAddressValueInitMethod = (*env)->GetMethodID(env,theInstanceAddressValueClass,"<init>","(JLCLIPSJNI/Environment;)V");

   /*==============================================*/
   /* If the Java init methods could not be found, */
   /* abort creation of the enviroment.            */
   /*==============================================*/
   
   if ((theClassGetCanonicalNameMethod == NULL) ||
       (theLongInitMethod == NULL) || (theDoubleInitMethod == NULL) || 
       (theArrayListInitMethod == NULL) || (theArrayListAddMethod == NULL) ||
       (theVoidValueInitMethod == NULL) ||
       (theIntegerValueInitMethod == NULL) || (theFloatValueInitMethod == NULL) ||
       (theSymbolValueInitMethod == NULL) || (theStringValueInitMethod == NULL) ||
       (theInstanceNameValueInitMethod == NULL) ||
       (theMultifieldValueInitMethod == NULL) ||
       (theFactAddressValueInitMethod == NULL) ||
       (theInstanceAddressValueInitMethod == NULL))
     { return((jlong) NULL); }
     
   /*=========================*/
   /* Create the environment. */
   /*=========================*/
   
   theEnv = CreateEnvironment();
   if (theEnv == NULL) return((jlong) NULL);
   
   /*====================================*/
   /* Allocate the JNI environment data. */
   /*====================================*/
   
   AllocateEnvironmentData(theEnv,CLIPSJNI_DATA,sizeof(struct clipsJNIData),DeallocateJNIData);
   
   /*===================================================*/
   /* Cache the class and method references (converting */
   /* the local class references to global references   */
   /* so they won't be garbage collected.               */
   /*===================================================*/

   CLIPSJNIData(theEnv)->classClass = (*env)->NewGlobalRef(env,theClassClass);
   CLIPSJNIData(theEnv)->classGetCanonicalNameMethod = theClassGetCanonicalNameMethod;

   CLIPSJNIData(theEnv)->longClass = (*env)->NewGlobalRef(env,theLongClass);
   CLIPSJNIData(theEnv)->longInitMethod = theLongInitMethod;
   CLIPSJNIData(theEnv)->doubleClass = (*env)->NewGlobalRef(env,theDoubleClass);
   CLIPSJNIData(theEnv)->doubleInitMethod = theDoubleInitMethod;
   CLIPSJNIData(theEnv)->arrayListClass = (*env)->NewGlobalRef(env,theArrayListClass);
   CLIPSJNIData(theEnv)->arrayListInitMethod = theArrayListInitMethod;
   CLIPSJNIData(theEnv)->arrayListAddMethod = theArrayListAddMethod;

   CLIPSJNIData(theEnv)->voidValueClass = (*env)->NewGlobalRef(env,theVoidValueClass);
   CLIPSJNIData(theEnv)->voidValueInitMethod = theVoidValueInitMethod;
   
   CLIPSJNIData(theEnv)->integerValueClass = (*env)->NewGlobalRef(env,theIntegerValueClass);
   CLIPSJNIData(theEnv)->integerValueInitMethod = theIntegerValueInitMethod;
   CLIPSJNIData(theEnv)->floatValueClass = (*env)->NewGlobalRef(env,theFloatValueClass);
   CLIPSJNIData(theEnv)->floatValueInitMethod = theFloatValueInitMethod;
      
   CLIPSJNIData(theEnv)->symbolValueClass = (*env)->NewGlobalRef(env,theSymbolValueClass);
   CLIPSJNIData(theEnv)->symbolValueInitMethod = theSymbolValueInitMethod;
   CLIPSJNIData(theEnv)->stringValueClass = (*env)->NewGlobalRef(env,theStringValueClass);
   CLIPSJNIData(theEnv)->stringValueInitMethod = theStringValueInitMethod;
   CLIPSJNIData(theEnv)->instanceNameValueClass = (*env)->NewGlobalRef(env,theInstanceNameValueClass);
   CLIPSJNIData(theEnv)->instanceNameValueInitMethod = theInstanceNameValueInitMethod;

   CLIPSJNIData(theEnv)->multifieldValueClass = (*env)->NewGlobalRef(env,theMultifieldValueClass);
   CLIPSJNIData(theEnv)->multifieldValueInitMethod = theMultifieldValueInitMethod;

   CLIPSJNIData(theEnv)->factAddressValueClass = (*env)->NewGlobalRef(env,theFactAddressValueClass);
   CLIPSJNIData(theEnv)->factAddressValueInitMethod = theFactAddressValueInitMethod;

   CLIPSJNIData(theEnv)->instanceAddressValueClass = (*env)->NewGlobalRef(env,theInstanceAddressValueClass);
   CLIPSJNIData(theEnv)->instanceAddressValueInitMethod = theInstanceAddressValueInitMethod;
   
   /*======================================*/
   /* Store the java environment for later */
   /* access by the CLIPS environment.     */
   /*======================================*/
   
   SetEnvironmentContext(theEnv,(void *) env);
     
   /*=======================================*/
   /* Deallocate the local Java references. */
   /*=======================================*/
   
   (*env)->DeleteLocalRef(env,theClassClass);
   (*env)->DeleteLocalRef(env,theLongClass);
   (*env)->DeleteLocalRef(env,theDoubleClass);
   (*env)->DeleteLocalRef(env,theArrayListClass);
   (*env)->DeleteLocalRef(env,theVoidValueClass);
   (*env)->DeleteLocalRef(env,theIntegerValueClass);
   (*env)->DeleteLocalRef(env,theFloatValueClass);
   (*env)->DeleteLocalRef(env,theSymbolValueClass);
   (*env)->DeleteLocalRef(env,theStringValueClass);
   (*env)->DeleteLocalRef(env,theInstanceNameValueClass);
   (*env)->DeleteLocalRef(env,theMultifieldValueClass);
   (*env)->DeleteLocalRef(env,theFactAddressValueClass);
   (*env)->DeleteLocalRef(env,theInstanceAddressValueClass);

   /*=================================*/
   /* Set up Java External Addresses. */
   /*=================================*/
   
   CLIPSJNIData(theEnv)->javaExternalAddressID = InstallExternalAddressType(theEnv,&javaPointer);
   
   /*=========================*/
   /* Return the environment. */
   /*=========================*/
   
   return (PointerToJLong(theEnv));
  }

/*********************************************/
/* Java_CLIPSJNI_Environment_clear: Native   */ 
/*   function for the CLIPSJNI clear method. */
/*                                           */
/* Class:     CLIPSJNI_Environment           */
/* Method:    clear                          */
/* Signature: (J)V                           */
/*********************************************/
JNIEXPORT void JNICALL Java_CLIPSJNI_Environment_clear(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv)
  {
   EnvClear(JLongToPointer(clipsEnv));
  }

/*********************************************/
/* Java_CLIPSJNI_Environment_reset: Native   */ 
/*   function for the CLIPSJNI reset method. */
/*                                           */
/* Class:     CLIPSJNI_Environment           */
/* Method:    reset                          */
/* Signature: (J)V                           */
/*********************************************/
JNIEXPORT void JNICALL Java_CLIPSJNI_Environment_reset(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv)
  {
   EnvReset(JLongToPointer(clipsEnv));
  }

/********************************************/
/* Java_CLIPSJNI_Environment_load: Native   */ 
/*   function for the CLIPSJNI load method. */
/*                                          */
/* Class:     CLIPSJNI_Environment          */
/* Method:    load                          */
/* Signature: (JLjava/lang/String;)V        */
/********************************************/
JNIEXPORT void JNICALL Java_CLIPSJNI_Environment_load(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring fileName)
  {
   const char *cFileName = (*env)->GetStringUTFChars(env,fileName,NULL);

   EnvLoad(JLongToPointer(clipsEnv),(char *) cFileName);
   
   (*env)->ReleaseStringUTFChars(env,fileName,cFileName);
  }

/*************************************************/
/* Java_CLIPSJNI_Environment_loadFacts: Native   */ 
/*   function for the CLIPSJNI loadFacts method. */
/*                                               */
/* Class:     CLIPSJNI_Environment               */
/* Method:    loadFact                           */
/* Signature: (JLjava/lang/String;)Z             */
/*************************************************/
JNIEXPORT jboolean JNICALL Java_CLIPSJNI_Environment_loadFacts(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring fileName)
  {
   jboolean rv;
   const char *cFileName = (*env)->GetStringUTFChars(env,fileName,NULL);

   rv = EnvLoadFacts(JLongToPointer(clipsEnv),(char *) cFileName);
   
   (*env)->ReleaseStringUTFChars(env,fileName,cFileName);
   
   return rv;
  }

/*********************************************/
/* Java_CLIPSJNI_Environment_watch: Native   */ 
/*   function for the CLIPSJNI watch method. */
/*                                           */
/* Class:     CLIPSJNI_Environment           */
/* Method:    watch                          */
/* Signature: (JLjava/lang/String;)Z         */
/*********************************************/
JNIEXPORT jboolean JNICALL Java_CLIPSJNI_Environment_watch(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring watchItem)
  {
   jboolean rv;
   const char *cWatchItem = (*env)->GetStringUTFChars(env,watchItem,NULL);

   rv = EnvWatch(JLongToPointer(clipsEnv),(char *) cWatchItem);
   
   (*env)->ReleaseStringUTFChars(env,watchItem,cWatchItem);
   
   return rv;
  }

/***********************************************/
/* Java_CLIPSJNI_Environment_unwatch: Native   */ 
/*   function for the CLIPSJNI unwatch method. */
/*                                             */
/* Class:     CLIPSJNI_Environment             */
/* Method:    unwatch                          */
/* Signature: (JLjava/lang/String;)Z           */
/***********************************************/
JNIEXPORT jboolean JNICALL Java_CLIPSJNI_Environment_unwatch(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring watchItem)
  {
   jboolean rv;
   const char *cWatchItem = (*env)->GetStringUTFChars(env,watchItem,NULL);

   rv = EnvUnwatch(JLongToPointer(clipsEnv),(char *) cWatchItem);
   
   (*env)->ReleaseStringUTFChars(env,watchItem,cWatchItem);
   
   return rv;
  }

/**************************************************/
/* Java_CLIPSJNI_Environment_run: Native function */ 
/*   for the CLIPSJNI run method.                 */
/*                                                */
/* Class:     CLIPSJNI_Environment                */
/* Method:    run                                 */
/* Signature: (JJ)J                               */
/**************************************************/
JNIEXPORT jlong JNICALL Java_CLIPSJNI_Environment_run(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jlong runLimit)
  {
   return EnvRun(JLongToPointer(clipsEnv),runLimit);
  }

/*************************************************************/
/* Java_CLIPSJNI_Environment_eval: Native function for the   */
/*   CLIPSJNI eval method.                                   */
/*                                                           */
/* Class:     CLIPSJNI_Environment                           */
/* Method:    eval                                           */
/* Signature: (JLjava/lang/String;)LCLIPSJNI/PrimitiveValue; */
/*                                                           */
/*************************************************************/
JNIEXPORT jobject JNICALL Java_CLIPSJNI_Environment_eval(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring evalStr)
  {
   DATA_OBJECT theDO;
   jobject result = NULL;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   const char *cEvalStr = (*env)->GetStringUTFChars(env,evalStr,NULL);
   
   EnvEval(theCLIPSEnv,(char *) cEvalStr,&theDO);

   (*env)->ReleaseStringUTFChars(env,evalStr,cEvalStr);
   
   result = ConvertDataObject(env,obj,theCLIPSEnv,&theDO);

   return result;  
  }

/**********************/
/* ConvertDataObject: */
/**********************/
static jobject ConvertDataObject(
  JNIEnv *env,
  jobject javaEnv,
  void *clipsEnv,
  DATA_OBJECT *theDO)
  {
   jobject result = NULL, tresult;
   jint mfLength;
   struct multifield *theList;
   long i;
   
   switch(GetpType(theDO))
     {
      case MULTIFIELD:
        mfLength = GetpDOLength(theDO);

        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->arrayListClass,
                                   CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                   (jint) mfLength);
                                   
        if (result == NULL)
          { return result; }
          
        theList = (struct multifield *) DOPToPointer(theDO);
        
        for (i = GetpDOBegin(theDO); i <= GetpDOEnd(theDO); i++)
         {
          tresult = ConvertSingleFieldValue(env,javaEnv,clipsEnv,GetMFType(theList,i),GetMFValue(theList,i));
          
          if (tresult != NULL)
             { (*env)->CallBooleanMethod(env,result,CLIPSJNIData(clipsEnv)->arrayListAddMethod,tresult); }

          (*env)->DeleteLocalRef(env,tresult);
         }
       
        tresult = result;
         
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->multifieldValueClass,
                                   CLIPSJNIData(clipsEnv)->multifieldValueInitMethod,
                                   tresult);
        break;
        
      case RVOID:
      case SYMBOL:
      case STRING:
      case INSTANCE_NAME:
      case INTEGER:
      case FLOAT:
      case FACT_ADDRESS:
      case INSTANCE_ADDRESS:
        result = ConvertSingleFieldValue(env,javaEnv,clipsEnv,GetpType(theDO),GetpValue(theDO));
        break;

      default: 
        break;
     }

   return result;  
  }
  
/****************************/
/* ConvertSingleFieldValue: */
/****************************/
static jobject ConvertSingleFieldValue(
  JNIEnv *env,
  jobject javaEnv,
  void *clipsEnv,
  int type,
  void  *value)
  {
   jobject result = NULL, tresult;
   jstring sresult = NULL;
   
   switch(type)
     {
      case RVOID:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->voidValueClass,
                                   CLIPSJNIData(clipsEnv)->voidValueInitMethod,
                                   sresult);
        break;

      case SYMBOL:
        sresult = (*env)->NewStringUTF(env,ValueToString(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->symbolValueClass,
                                   CLIPSJNIData(clipsEnv)->symbolValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
        
      case STRING:
        sresult = (*env)->NewStringUTF(env,ValueToString(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->stringValueClass,
                                   CLIPSJNIData(clipsEnv)->stringValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
      case INSTANCE_NAME:
        sresult = (*env)->NewStringUTF(env,ValueToString(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->instanceNameValueClass,
                                   CLIPSJNIData(clipsEnv)->instanceNameValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
      case INTEGER:
        tresult = (*env)->NewObject(env,
                                    CLIPSJNIData(clipsEnv)->longClass,
                                    CLIPSJNIData(clipsEnv)->longInitMethod,
                                    (jlong) ValueToLong(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->integerValueClass,
                                   CLIPSJNIData(clipsEnv)->integerValueInitMethod,
                                   tresult);
        (*env)->DeleteLocalRef(env,tresult);
        break;

      case FLOAT:
        tresult = (*env)->NewObject(env,
                                    CLIPSJNIData(clipsEnv)->doubleClass,
                                    CLIPSJNIData(clipsEnv)->doubleInitMethod,
                                    (jdouble) ValueToDouble(value));

        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->floatValueClass,
                                   CLIPSJNIData(clipsEnv)->floatValueInitMethod,
                                   tresult);
        (*env)->DeleteLocalRef(env,tresult);
        break;

      case FACT_ADDRESS:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->factAddressValueClass,
                                   CLIPSJNIData(clipsEnv)->factAddressValueInitMethod,
                                   PointerToJLong(value),javaEnv);
        break;

      case INSTANCE_ADDRESS:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->instanceAddressValueClass,
                                   CLIPSJNIData(clipsEnv)->instanceAddressValueInitMethod,
                                   PointerToJLong(value),javaEnv);
        break;

      default: 
        break;
     }

   return result;  
  }

/****************************************************/
/* Java_CLIPSJNI_Environment_build: Native function */
/*   for the CLIPSJNI build method.                 */
/*                                                  */
/* Class:     CLIPSJNI_Environment                  */
/* Method:    build                                 */
/* Signature: (JLjava/lang/String;)Z                */
/****************************************************/
JNIEXPORT jboolean JNICALL Java_CLIPSJNI_Environment_build(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jstring buildStr)
  {
   jboolean rv;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cBuildStr = (*env)->GetStringUTFChars(env,buildStr,NULL);
   
   rv = (jboolean) EnvBuild(theCLIPSEnv,(char *) cBuildStr);

   (*env)->ReleaseStringUTFChars(env,buildStr,cBuildStr);
      
   return rv;
  }

/***************************************************************/
/* Java_CLIPSJNI_Environment_assertString: Native function for */
/*   the CLIPSJNI assertString method.                         */
/*                                                             */
/* Class:     CLIPSJNI_Environment                             */
/* Method:    assertString                                     */
/* Signature: (JLjava/lang/String;)LCLIPSJNI/FactAddressValue; */
/***************************************************************/
JNIEXPORT jobject JNICALL Java_CLIPSJNI_Environment_assertString(
  JNIEnv *env,
  jobject obj, 
  jlong clipsEnv, 
  jstring factStr)
  {
   void *theFact;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cFactStr = (*env)->GetStringUTFChars(env,factStr,NULL);
   
   theFact = EnvAssertString(theCLIPSEnv,(char *) cFactStr);

   (*env)->ReleaseStringUTFChars(env,factStr,cFactStr);
   
   if (theFact == NULL)
     { return(NULL); }
     
   return ConvertSingleFieldValue(env,obj,theCLIPSEnv,FACT_ADDRESS,theFact);
  }

/*************************************************/
/* Java_CLIPSJNI_Environment_factIndex: Native   */
/*   function for the CLIPSJNI factIndex method. */
/*                                               */
/* Class:     CLIPSJNI_Environment               */
/* Method:    factIndex                          */
/* Signature: (LCLIPSJNI/Environment;JJ)J        */
/*************************************************/
JNIEXPORT jlong JNICALL Java_CLIPSJNI_Environment_factIndex(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsFact)
  {
   return (jlong) EnvFactIndex(JLongToPointer(clipsEnv),JLongToPointer(clipsFact));
  }

/**************************************************************/
/* Java_CLIPSJNI_Environment_getFactSlot: Native function     */
/*   for the CLIPSJNI getFactSlot method.                     */
/*                                                            */
/* Class:     CLIPSJNI_Environment                            */
/* Method:    getFactSlot                                     */
/* Signature: (LCLIPSJNI/Environment;JJLjava/lang/String;)    */
/*            LCLIPSJNI/PrimitiveValue;                       */
/**************************************************************/
JNIEXPORT jobject JNICALL Java_CLIPSJNI_Environment_getFactSlot(
  JNIEnv *env,
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv,
  jlong clipsFact,
  jstring slotName)
  {
   DATA_OBJECT theDO;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cSlotName = (*env)->GetStringUTFChars(env,slotName,NULL);
   
   EnvGetFactSlot(JLongToPointer(clipsEnv),JLongToPointer(clipsFact),(char *) cSlotName,&theDO);

   (*env)->ReleaseStringUTFChars(env,slotName,cSlotName);
   
   return ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO);
  }

/*******************************************************************/
/* Java_CLIPSJNI_Environment_makeInstance: Native function for the */
/*   CLIPSJNI makeInstance method.                                 */
/*                                                                 */
/* Class:     CLIPSJNI_Environment                                 */
/* Method:    makeInstance                                         */
/* Signature: (JLjava/lang/String;)LCLIPSJNI/InstanceAddressValue; */
/*******************************************************************/
JNIEXPORT jobject JNICALL Java_CLIPSJNI_Environment_makeInstance(
  JNIEnv *env,
  jobject obj, 
  jlong clipsEnv, 
  jstring instanceStr)
  {
   void *theInstance;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cInstanceStr = (*env)->GetStringUTFChars(env,instanceStr,NULL);
   
   theInstance = EnvMakeInstance(theCLIPSEnv,(char *) cInstanceStr);

   (*env)->ReleaseStringUTFChars(env,instanceStr,cInstanceStr);
   
   if (theInstance == NULL)
     { return(NULL); }
     
   return ConvertSingleFieldValue(env,obj,theCLIPSEnv,INSTANCE_ADDRESS,theInstance);
  }

/***********************************************************/
/* Java_CLIPSJNI_Environment_getInstanceName: Native       */
/*   function for the CLIPSJNI getInstanceName method.     */
/*                                                         */
/* Class:     CLIPSJNI_Environment                         */
/* Method:    getInstanceName                              */
/* Signature: (LCLIPSJNI/Environment;JJ)Ljava/lang/String; */
/***********************************************************/
JNIEXPORT jstring JNICALL Java_CLIPSJNI_Environment_getInstanceName(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsInstance)
  {
   return (*env)->NewStringUTF(env,EnvGetInstanceName(JLongToPointer(clipsEnv),JLongToPointer(clipsInstance)));
  }

/**************************************************************/
/* Java_CLIPSJNI_Environment_directGetSlot: Native function   */
/*   for the CLIPSJNI directGetSlot method.                   */
/*                                                            */
/* Class:     CLIPSJNI_Environment                            */
/* Method:    directGetSlot                                   */
/* Signature: (LCLIPSJNI/Environment;JJLjava/lang/String;)    */
/*            LCLIPSJNI/PrimitiveValue;                       */
/**************************************************************/
JNIEXPORT jobject JNICALL Java_CLIPSJNI_Environment_directGetSlot(
  JNIEnv *env,
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv,
  jlong clipsInstance,
  jstring slotName)
  {
   DATA_OBJECT theDO;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cSlotName = (*env)->GetStringUTFChars(env,slotName,NULL);
   
   EnvDirectGetSlot(JLongToPointer(clipsEnv),JLongToPointer(clipsInstance),(char *) cSlotName,&theDO);

   (*env)->ReleaseStringUTFChars(env,slotName,cSlotName);
   
   return ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO);
  }

/**********************************************************/
/* Java_CLIPSJNI_Environment_destroyEnvironment: Native   */
/*   function for the CLIPSJNI destroyEnvironment method. */
/*                                                        */
/* Class:     CLIPSJNI_Environment                        */
/* Method:    destroyEnvironment                          */
/* Signature: (J)V                                        */
/**********************************************************/
JNIEXPORT void JNICALL Java_CLIPSJNI_Environment_destroyEnvironment(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   DestroyEnvironment(JLongToPointer(clipsEnv));
  }

/***************************************************/
/* Java_CLIPSJNI_Environment_commandLoop: Native   */
/*   function for the CLIPSJNI commandLoop method. */
/*                                                 */
/* Class:     CLIPSJNI_Environment                 */
/* Method:    commandLoop                          */
/* Signature: (J)V                                 */
/***************************************************/
JNIEXPORT void JNICALL Java_CLIPSJNI_Environment_commandLoop(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   CommandLoop(JLongToPointer(clipsEnv));
  }

/*******************************************************/
/* Java_CLIPSJNI_Environment_addRouter: Native         */
/*   function for the CLIPSJNI addRouter method.       */
/*                                                     */
/* Class:     CLIPSJNI_Environment                     */
/* Method:    addRouter                                */
/* Signature: (JLjava/lang/String;ILCLIPSJNI/Router;)Z */
/*******************************************************/
JNIEXPORT jboolean JNICALL Java_CLIPSJNI_Environment_addRouter(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring routerName, 
  jint priority, 
  jobject context)
  {
   int rv;
   jclass cls;
   jobject nobj;   
   const char *cRouterName = (*env)->GetStringUTFChars(env,routerName,NULL);
   
   nobj = (*env)->NewGlobalRef(env,context); /* TBD Need to deallocate when environment or router destroyed */
   
   cls = (*env)->GetObjectClass(env,context);

   rv = EnvAddRouterWithContext(JLongToPointer(clipsEnv),(char *) cRouterName,(int) priority,
                                QueryJNIRouter,PrintJNIRouter,GetcJNIRouter,
                                UngetcJNIRouter,ExitJNIRouter,(void *) nobj);
   
   (*env)->ReleaseStringUTFChars(env,routerName,cRouterName);

   return rv;
  }

/******************/
/* JLongToPointer */
/******************/
static void *JLongToPointer(
  jlong value)
  {
   return (void *) value;
  }


/******************/
/* PointerToJLong */
/******************/
static jlong PointerToJLong(
  void *value)
  {
   return (jlong) value;
  }

/*******************************************************/
/* PrintJavaAddress:  */
/*******************************************************/
static void PrintJavaAddress(
  void *theEnv,
  char *logicalName,
  void *theValue)
  {
   jobject theObject;
   jclass cls;
   jstring str;
   char *cStr;
   JNIEnv *env;
   char buffer[20];

   EnvPrintRouter(theEnv,logicalName,"<Pointer-");
        
   theObject = (jobject) ValueToExternalAddress(theValue);
   
   if (theObject != NULL)
     {
      env = (JNIEnv *) GetEnvironmentContext(theEnv);
      
      cls = (*env)->GetObjectClass(env,theObject);
      
      str = (jstring) (*env)->CallObjectMethod(env,cls,CLIPSJNIData(theEnv)->classGetCanonicalNameMethod);

      cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
      
      EnvPrintRouter(theEnv,logicalName,cStr);
      EnvPrintRouter(theEnv,logicalName,"-");
   
      (*env)->ReleaseStringUTFChars(env,str,cStr);
     }
   else
     { EnvPrintRouter(theEnv,logicalName,"java-"); }
   
   gensprintf(buffer,"%p",(void *) theObject);
   EnvPrintRouter(theEnv,logicalName,buffer);
   EnvPrintRouter(theEnv,logicalName,">");
  }

/*******************************************************/
/* NewJavaAddress:  */
/*******************************************************/
static void NewJavaAddress(
  void *theEnv,
  DATA_OBJECT *rv)
  {
   jclass theClass, tempClass;
   int numberOfArguments;
   JNIEnv *env;
   char *className;
   char *classDescriptor;
   DATA_OBJECT theValue;
   size_t i, length;
   jmethodID mid;
   jobjectArray constructorList, parameterList;
   jsize theSize, c; 
   jsize paramCount, p; 
   jobject theConstructor, theObject, oldObject; 
   int found = FALSE, matches;
   DATA_OBJECT_PTR newArgs;
   jvalue *javaArgs;
   
   /*=============================================*/
   /* Retrieve the JNI environment pointer stored */
   /* in the CLIPS environment structure.         */
   /*=============================================*/
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   /*======================================================================*/
   /* If the Java external address type is used, additional arguments must */
   /* at least include the Java class name of the object to be created.    */
   /*======================================================================*/
   
   if ((numberOfArguments = EnvArgCountCheck(theEnv,"new (with type Java)",AT_LEAST,2)) == -1) 
     { return; }
   
   /*=======================================*/
   /* The Java class name must be a symbol. */
   /*=======================================*/
   
   if (EnvArgTypeCheck(theEnv,"new (with type Java)",2,SYMBOL,&theValue) == FALSE) 
     { return; }
   
   className = DOToString(theValue);
   
   /*=============================================*/
   /* Construct the class descriptor by replacing */
   /* any periods (.) in the class name with a    */
   /* forward slash (/).                          */
   /*=============================================*/
   
   length = strlen(className);
   classDescriptor = genalloc(theEnv,length + 1);
   for (i = 0; i < length; i++)
     {
      if (className[i] != '.')
        { classDescriptor[i] = className[i]; }
      else 
        { classDescriptor[i] = '/'; }
     }
   classDescriptor[i] = 0;
   
   /*=======================*/
   /* Search for the class. */
   /*=======================*/
   
   theClass = (*env)->FindClass(env,classDescriptor); 
   
   /*========================================*/
   /* Free the constructed class descriptor. */
   /*========================================*/
   
   genfree(theEnv,classDescriptor,length + 1);

   /*============================================*/
   /* Signal an error if the class wasn't found. */
   /*============================================*/
   
   if (theClass == NULL)
     {
      if ((*env)->ExceptionOccurred(env))
        { (*env)->ExceptionClear(env); }
      SetEvaluationError(theEnv,TRUE);
      ExpectedTypeError1(theEnv,"new (with type Java)",2,"Java class name");
      return;
     }
      
   /*===========================================================================*/
   /* Evaluate the CLIPS arguments that will be passed to the java constructor. */
   /*===========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { newArgs = NULL; }
   else
     {
      newArgs = (DATA_OBJECT_PTR) genalloc(theEnv,sizeof(DATA_OBJECT) * (numberOfArguments - 2));
      for (i = 0; i < numberOfArguments - 2; i++)
        {
         EnvRtnUnknown(theEnv,i+3,&newArgs[i]);
         if (GetEvaluationError(theEnv))
           {   
            (*env)->DeleteLocalRef(env,theClass);
            return;
           }
        }
     }

   /*========================================================================*/
   /* Construct an array in which to store the corresponding java arguments. */
   /*========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { javaArgs = NULL; }
   else
     { javaArgs = (jvalue *) genalloc(theEnv,sizeof(jvalue) * (numberOfArguments - 2)); }
   
   /*=============================================*/
   /* Get the method index of the getConstructors */
   /* method from the java.lang.Class class.      */
   /*=============================================*/

   tempClass = (*env)->FindClass(env,"java/lang/Class");
   mid = (*env)->GetMethodID(env,tempClass,"getConstructors","()[Ljava/lang/reflect/Constructor;"); 
   (*env)->DeleteLocalRef(env,tempClass);
   
   /*=======================================================*/
   /* Get the list of constructors for the specified class. */
   /*=======================================================*/
     
   constructorList = (jobjectArray) (*env)->CallObjectMethod(env,theClass,mid);

   /*======================================================*/
   /* Get the method index of the getParameterTypes method */
   /* from the java.lang.reflect.Constructor class.        */
   /*======================================================*/

   tempClass = (*env)->FindClass(env,"java/lang/reflect/Constructor"); 
   mid = (*env)->GetMethodID(env,tempClass,"getParameterTypes","()[Ljava/lang/Class;"); 
   (*env)->DeleteLocalRef(env,tempClass);

   /*===============================================*/
   /* Search the constructor list for a constructor */
   /* with matching arguments.                      */
   /*===============================================*/
      
   theSize = (*env)->GetArrayLength(env,constructorList); 
   for (c = 0; c < theSize; c++) 
     { 
      theConstructor = (*env)->GetObjectArrayElement(env,constructorList,c); 
      
      parameterList = (jobjectArray) (*env)->CallObjectMethod(env,theConstructor,mid);
      
      paramCount = (*env)->GetArrayLength(env,parameterList); 
      
      if (paramCount != (numberOfArguments - 2))
        { continue; }
        
      matches = TRUE;
      
      for (p = 0; (p < paramCount) && matches; p++)
        {
         jstring str;
         char *cStr;
   
         tempClass = (jclass) (*env)->GetObjectArrayElement(env,parameterList,p);
         
         str = (jstring) (*env)->CallObjectMethod(env,tempClass,CLIPSJNIData(theEnv)->classGetCanonicalNameMethod);

         cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
                  
         if (GetType(newArgs[p]) == INTEGER)
           {
            if (strcmp(cStr,"long") == 0)
              { 
               printf("p[%d] = %s\n",(int) p,cStr);
               javaArgs[p].j = DOToLong(newArgs[p]);
              }
            else if (strcmp(cStr,"int") == 0)  
              { 
               printf("p[%d] = %s\n",(int) p,cStr);
               javaArgs[p].i = DOToLong(newArgs[p]);
              }
            else
              { matches = FALSE; }
           }
         else
           { matches = FALSE; }
         
         (*env)->ReleaseStringUTFChars(env,str,cStr);
      
         (*env)->DeleteLocalRef(env,tempClass);
        }
      
      if (matches)
        { 
         found = TRUE;
         break; 
        }
     } 

   /*==========================================*/
   /* If an appropriate constructor was found, */
   /* invoke it to create a new java object.   */
   /*==========================================*/
   
   theObject = NULL;
   if (found)
     {
      if (paramCount == 0)
        {
         mid = (*env)->FromReflectedMethod(env,theConstructor);
         theObject = (*env)->NewObject(env,theClass,mid);
        }
      else
        {
         mid = (*env)->FromReflectedMethod(env,theConstructor);
         theObject = (*env)->NewObjectA(env,theClass,mid,javaArgs);
        }
     }
 
   /*========================================================*/
   /* Delete the local reference to the class of the object. */
   /*========================================================*/
   
   (*env)->DeleteLocalRef(env,theClass);

   /*=========================================================*/
   /* If the object was created, add a global reference to it */
   /* and delete the local reference. This will prevent the   */
   /* object from being garbage collected until CLIPS deletes */
   /* the global reference.                                   */
   /*=========================================================*/
   
   if (theObject != NULL)
     {
      oldObject = theObject;
      theObject = (*env)->NewGlobalRef(env,theObject);
      (*env)->DeleteLocalRef(env,oldObject);
     }

   /*=============================================*/
   /* Return the array containing the DATA_OBJECT */
   /* arguments to the new function.              */
   /*=============================================*/
   
   if (newArgs != NULL)
     { genfree(theEnv,newArgs,sizeof(DATA_OBJECT) * (numberOfArguments - 2)); }
     
   if (javaArgs != NULL)
     { genfree(theEnv,javaArgs,sizeof(jvalue) * (numberOfArguments - 2)); }
   
   /*=============================================*/
   /* If a java exception occurred, set the CLIPS */
   /* error flag and clear the java exception.    */
   /*=============================================*/
   
   if ((*env)->ExceptionOccurred(env))
     { 
      SetEvaluationError(theEnv,TRUE);
      (*env)->ExceptionClear(env); 
     }

   /*==========================================*/
   /* Return the newly created java object if  */
   /* it was successfully created, otherwise   */
   /* leave the default return value of FALSE. */
   /*==========================================*/
   
   if (theObject != NULL)
     {
      SetpType(rv,EXTERNAL_ADDRESS);
      SetpValue(rv,EnvAddExternalAddress(theEnv,theObject,CLIPSJNIData(theEnv)->javaExternalAddressID));
     }
  }

/*******************************************************/
/* CallJavaMethod:  */
/*******************************************************/
static intBool CallJavaMethod(
  void *theEnv,
  DATA_OBJECT *target,
  DATA_OBJECT *rv)
  {
   int numberOfArguments;
   jobject theObject, theMethod;
   jclass objectClass, tempClass;
   jmethodID mid, getNameID;
   JNIEnv *env;
   jobjectArray methodList, parameterList;
   jsize theSize, c; 
   jsize paramCount, p; 
   DATA_OBJECT theValue;
   char *methodName;
   jstring str;
   char *cStr;
   int found = FALSE, matches;
   DATA_OBJECT_PTR newArgs;
   jvalue *javaArgs;
   int i;
   
   /*=============================================*/
   /* Retrieve the JNI environment pointer stored */
   /* in the CLIPS environment structure.         */
   /*=============================================*/
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   /*=================================================================*/
   /* If the Java external address type is used, additional arguments */
   /* must at least include the name of the method being called.      */
   /*=================================================================*/
   
   if ((numberOfArguments = EnvArgCountCheck(theEnv,"call (with type Java)",AT_LEAST,2)) == -1) 
     { return FALSE; }

   /*========================================*/
   /* The Java method name must be a symbol. */
   /*========================================*/
   
   if (EnvArgTypeCheck(theEnv,"call (with type Java)",2,SYMBOL,&theValue) == FALSE) 
     { return FALSE; }
   
   methodName = DOToString(theValue);

   /*===========================================================================*/
   /* Evaluate the CLIPS arguments that will be passed to the java constructor. */
   /*===========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { newArgs = NULL; }
   else
     {
      newArgs = (DATA_OBJECT_PTR) genalloc(theEnv,sizeof(DATA_OBJECT) * (numberOfArguments - 2));
      for (i = 0; i < numberOfArguments - 2; i++)
        {
         EnvRtnUnknown(theEnv,i+3,&newArgs[i]);
         if (GetEvaluationError(theEnv))
           { return FALSE; }
        }
     }

   /*========================================================================*/
   /* Construct an array in which to store the corresponding java arguments. */
   /*========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { javaArgs = NULL; }
   else
     { javaArgs = (jvalue *) genalloc(theEnv,sizeof(jvalue) * (numberOfArguments - 2)); }

   /*===============================================*/
   /* If the target is an external address, then we */
   /* should be invoking a method of an instance.   */
   /*===============================================*/

   if (GetpType(target) == EXTERNAL_ADDRESS)
     {
      theObject = DOPToExternalAddress(target);

      /*=========================================*/
      /* Determine the class of the java object. */
      /*=========================================*/
      
      objectClass = (*env)->GetObjectClass(env,theObject);

      /*=============================================*/
      /* Get the method index of the getConstructors */
      /* method from the java.lang.Class class.      */
      /*=============================================*/

      tempClass = (*env)->FindClass(env,"java/lang/Class"); /* TBD Cache this Value */
      mid = (*env)->GetMethodID(env,tempClass,"getMethods","()[Ljava/lang/reflect/Method;"); 
      (*env)->DeleteLocalRef(env,tempClass);

      /*==================================================*/
      /* Get the list of methods for the specified class. */
      /*==================================================*/
     
      methodList = (jobjectArray) (*env)->CallObjectMethod(env,objectClass,mid);
      (*env)->DeleteLocalRef(env,objectClass);

      /*======================================================*/
      /* Get the method index of the getParameterTypes method */
      /* from the java.lang.reflect.Method class.             */
      /*======================================================*/

      tempClass = (*env)->FindClass(env,"java/lang/reflect/Method"); /* TBD Cache this Value */
      mid = (*env)->GetMethodID(env,tempClass,"getParameterTypes","()[Ljava/lang/Class;"); 
      getNameID = (*env)->GetMethodID(env,tempClass,"getName","()Ljava/lang/String;"); 
      (*env)->DeleteLocalRef(env,tempClass);

      /*=====================================*/
      /* Search the method list for a method */
      /* with matching arguments.            */
      /*=====================================*/

      theSize = (*env)->GetArrayLength(env,methodList); 
      for (c = 0; c < theSize; c++) 
        { 
         theMethod = (*env)->GetObjectArrayElement(env,methodList,c); 
         str = (jstring) (*env)->CallObjectMethod(env,theMethod,getNameID);
         cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
         
         /*===================================*/
         /* If the method name doesn't match, */
         /* move on to the next method.       */
         /*===================================*/
         
         if (strcmp(methodName,cStr) != 0)
           {
            (*env)->ReleaseStringUTFChars(env,str,cStr);
            continue;
           }
         (*env)->ReleaseStringUTFChars(env,str,cStr);
         
         /*==========================================*/
         /* Get the parameter list of the method and */
         /* determine the number of parameters.      */
         /*==========================================*/
         
         parameterList = (jobjectArray) (*env)->CallObjectMethod(env,theMethod,mid);
      
         paramCount = (*env)->GetArrayLength(env,parameterList); 
      
         if (paramCount != (numberOfArguments - 2))
           { 
            (*env)->ReleaseStringUTFChars(env,str,cStr);
            continue; 
           }

         matches = TRUE;
      
         for (p = 0; (p < paramCount) && matches; p++)
           {
            tempClass = (jclass) (*env)->GetObjectArrayElement(env,parameterList,p);
         
            str = (jstring) (*env)->CallObjectMethod(env,tempClass,CLIPSJNIData(theEnv)->classGetCanonicalNameMethod);

            cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
             
            printf("p[%d] = %s\n",(int) p,cStr);
            
            if (GetType(newArgs[p]) == INTEGER)
              {
               if (strcmp(cStr,"long") == 0)
                 { 
                  /* printf("p[%d] = %s\n",(int) p,cStr); */
                  javaArgs[p].j = DOToLong(newArgs[p]);
                 }
               else if (strcmp(cStr,"int") == 0)  
                 { 
                  /* printf("p[%d] = %s\n",(int) p,cStr); */
                  javaArgs[p].i = DOToLong(newArgs[p]);
                 }
               else
                 { matches = FALSE; }
              }
            else
              { matches = FALSE; }

            (*env)->ReleaseStringUTFChars(env,str,cStr);
         
            (*env)->DeleteLocalRef(env,tempClass);
           }
      
         if (matches)
           { 
            found = TRUE;
            break; 
           }
        }
     }

   if (newArgs != NULL)
     { genfree(theEnv,newArgs,sizeof(DATA_OBJECT) * (numberOfArguments - 2)); }

   if (javaArgs != NULL)
     { genfree(theEnv,javaArgs,sizeof(jvalue) * (numberOfArguments - 2)); }
     
   return TRUE;
  }
  
/*******************************************************/
/* DiscardJavaAddress: */
/*******************************************************/
static intBool DiscardJavaAddress(
  void *theEnv,
  void *theValue)
  {
   JNIEnv *env;

   printf("Discarding Java Address %p\n",theValue);
   
   if (theValue != NULL)
     {
      env = (JNIEnv *) GetEnvironmentContext(theEnv);
      (*env)->DeleteGlobalRef(env,theValue);
     }
   
   return TRUE;
  }
