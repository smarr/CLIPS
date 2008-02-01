#include <windows.h>

extern "C"
{
#include "CLIPSWin32.h"
}

/********/
/* main */
/********/
int main()
  {
   void *theEnv;

   /*===========================*/
   /* Load and run the example. */
   /*===========================*/

   theEnv = __CreateEnvironment();
   __EnvLoad(theEnv,"hello.clp");
   __EnvReset(theEnv);
   __EnvRun(theEnv,-1);
   __DestroyEnvironment(theEnv);
   
   return 1;
  }


