#include "clipscpp.h"
#include <iostream>

int main()
  {
   CLIPS::CLIPSCPPEnv theEnv;

   theEnv.Load("hello.clp");
   theEnv.Reset();
   theEnv.Run(-1);

   return 0;
  }

