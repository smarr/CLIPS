#ifndef CLIPSWin32_H
#define CLIPSWin32_H

#include "CLIPSWin32defs.h"

void __declspec(dllimport) * CALL_CONV __CreateEnvironment(void);
void __declspec(dllimport) CALL_CONV __DestroyEnvironment(void *);
void __declspec(dllimport) CALL_CONV __EnvClear(void *);
void __declspec(dllimport) CALL_CONV __EnvReset(void *);
int __declspec(dllimport) CALL_CONV __EnvLoad(void *,char *);
long long __declspec(dllimport) CALL_CONV __EnvRun(void *,long long);

#endif