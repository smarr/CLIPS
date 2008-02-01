#include <windows.h>

#include "CLIPSWin32defs.h"

typedef int (CALL_CONV *EnvLoadPtr)(void *,char *);
typedef long long (CALL_CONV *EnvRunPtr)(void *,long long);
typedef void (CALL_CONV *EnvResetPtr)(void *);
typedef void * (CALL_CONV *CreateEnvironmentPtr)(void);
typedef void (CALL_CONV *DestroyEnvironmentPtr)(void *);

/********/
/* main */
/********/
int main()
  {
   void *theEnv;
   CreateEnvironmentPtr __CreateEnvironment;
   EnvLoadPtr __EnvLoad;
   EnvResetPtr __EnvReset;
   EnvRunPtr __EnvRun;
   DestroyEnvironmentPtr __DestroyEnvironment;
   HMODULE dll_handle;

   /*===============*/
   /* Load the DLL. */
   /*===============*/

   dll_handle = LoadLibrary("CLIPSWin32.dll");

   if (dll_handle == NULL)
     { return 0; }

   /*=====================================*/
   /* Retrieve pointers to the CLIPS API. */
   /*=====================================*/

   __CreateEnvironment = (CreateEnvironmentPtr)
      GetProcAddress(dll_handle,"__CreateEnvironment");
   
   __EnvLoad = (EnvLoadPtr)
      GetProcAddress(dll_handle,"__EnvLoad");
   
   __EnvReset = (EnvResetPtr)
      GetProcAddress(dll_handle,"__EnvReset");
   
   __EnvRun = (EnvRunPtr)
      GetProcAddress(dll_handle,"__EnvRun");
   
   __DestroyEnvironment = (DestroyEnvironmentPtr)
      GetProcAddress(dll_handle,"__DestroyEnvironment");
   
   if ((__CreateEnvironment == NULL) ||
       (__EnvLoad == NULL) ||
       (__EnvReset == NULL) ||
       (__EnvRun == NULL) ||
       (__DestroyEnvironment == NULL))
      { return 0; }

   /*===========================*/
   /* Load and run the example. */
   /*===========================*/

   theEnv = __CreateEnvironment();
   __EnvLoad(theEnv,"hello.clp");
   __EnvReset(theEnv);
   __EnvRun(theEnv,-1);
   __DestroyEnvironment(theEnv);

   /*=================*/
   /* Unload the DLL. */
   /*=================*/

   FreeLibrary(dll_handle);

   return 1;
  }

