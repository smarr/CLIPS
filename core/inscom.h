   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_inscom
#define _H_inscom

#ifndef _H_object
#include "object.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void SetupInstances(void);
LOCALE DllExport BOOLEAN DeleteInstance(void *);
LOCALE BOOLEAN UnmakeInstance(void *);
#if DEBUGGING_FUNCTIONS
LOCALE void InstancesCommand(void);
LOCALE void PPInstanceCommand(void);
LOCALE void Instances(char *,void *,char *,int);
#endif
LOCALE DllExport void *MakeInstance(char *);
LOCALE DllExport void *CreateRawInstance(void *,char *);
LOCALE DllExport void *FindInstance(void *,char *,BOOLEAN);
LOCALE DllExport int ValidInstanceAddress(void *);
LOCALE DllExport void DirectGetSlot(void *,char *,DATA_OBJECT *);
LOCALE DllExport int DirectPutSlot(void *,char *,DATA_OBJECT *);
LOCALE DllExport char *GetInstanceName(void *);
LOCALE DllExport void *GetInstanceClass(void *);
LOCALE DllExport unsigned long GetGlobalNumberOfInstances(void);
LOCALE DllExport void *GetNextInstance(void *);
LOCALE DllExport void *GetNextInstanceInScope(void *);
LOCALE DllExport void *GetNextInstanceInClass(void *,void *);
LOCALE void *GetNextInstanceInClassAndSubclasses(void **,void *,DATA_OBJECT *);
LOCALE DllExport void GetInstancePPForm(char *,int,void *);
LOCALE void ClassCommand(DATA_OBJECT *);
LOCALE BOOLEAN DeleteInstanceCommand(void);
LOCALE BOOLEAN UnmakeInstanceCommand(void);
LOCALE void SymbolToInstanceName(DATA_OBJECT *);
LOCALE void *InstanceNameToSymbol(void);
LOCALE void InstanceAddressCommand(DATA_OBJECT *);
LOCALE void InstanceNameCommand(DATA_OBJECT *);
LOCALE BOOLEAN InstanceAddressPCommand(void);
LOCALE BOOLEAN InstanceNamePCommand(void);
LOCALE BOOLEAN InstancePCommand(void);
LOCALE BOOLEAN InstanceExistPCommand(void);

#ifndef _INSCOM_SOURCE_
extern Thread INSTANCE_TYPE DummyInstance;
#endif

#endif





