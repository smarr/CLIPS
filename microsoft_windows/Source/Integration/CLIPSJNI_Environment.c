
#include "CLIPSJNI_Environment.h"

#include "clips.h"

#define CLIPSJNI_DATA 67

#ifdef __BORLANDC__
#include <float.h>
#endif

struct clipsJNIData
  { 
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

/**********************************************/
/* DeallocateJNIData: Deallocates environment */
/*    data for the JNI functionality.         */
/**********************************************/
static void DeallocateJNIData(
  void *theEnv)
  {
   JNIEnv *env;
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);
  
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
                                                        
   /*=================================================*/
   /* Turns off floating point exceptions in Borland. */
   /* Some of the libraries in Borland cause settings */
   /* that conflict with the Java Virtual Machine.    */
   /*=================================================*/
   
#ifdef __BORLANDC__
   _control87(MCW_EM, MCW_EM);
#endif

   /*===========================*/
   /* Look up the Java classes. */
   /*===========================*/
   
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
   
   if ((theLongClass == NULL) || (theDoubleClass == NULL) ||
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
   
   if ((theLongInitMethod == NULL) || (theDoubleInitMethod == NULL) || 
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
   jobject result;
   
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
   /* jclass cls; */
   jobject nobj;
   const char *cRouterName = (*env)->GetStringUTFChars(env,routerName,NULL);

   nobj = (*env)->NewGlobalRef(env,context); /* TBD Need to deallocate when environment or router destroyed */

   /* cls = (*env)->GetObjectClass(env,context); */

   rv = EnvAddRouterWithContext(JLongToPointer(clipsEnv),(char *) cRouterName,(int) priority,
                                QueryJNIRouter,PrintJNIRouter,GetcJNIRouter,
                                UngetcJNIRouter,ExitJNIRouter,(void *) nobj);

   (*env)->ReleaseStringUTFChars(env,routerName,cRouterName);

   return (jboolean) rv;
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

