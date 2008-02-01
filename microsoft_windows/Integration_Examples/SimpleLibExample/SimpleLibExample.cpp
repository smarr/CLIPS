#include "clipscpp.h"

#include <stdio.h>

using namespace CLIPS;

int main()
  {
   CLIPSCPPEnv theEnv;

   theEnv.Load("hello.clp");
   theEnv.Reset();
   theEnv.Run(-1);

   return 0;
  }

