   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*              FACTS MANAGER HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_factmngr

#define _H_factmngr

struct fact;

#ifndef _H_pattern
#include "pattern.h"
#endif
#include "multifld.h"
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

struct fact
  {
   struct patternEntity factHeader;
   struct deftemplate *whichDeftemplate;
   void *list;
   long int factIndex;
   unsigned int depth : 15;
   unsigned int garbage : 1;
   struct fact *previousFact;
   struct fact *nextFact;
   struct multifield theProposition;
  };

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _FACTMNGR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           PrintFactWithIdentifier(char *,struct fact *);
   LOCALE void                           PrintFact(char *,struct fact *);
   LOCALE void                           PrintFactIdentifierInLongForm(char *,void *);
   LOCALE DllExport BOOLEAN              Retract(void *);
   LOCALE DllExport void                *Assert(void *);
   LOCALE DllExport void                 RemoveAllFacts(void);
   LOCALE struct fact                   *CreateFactBySize(int);
   LOCALE void                           FactInstall(struct fact *);
   LOCALE void                           FactDeinstall(struct fact *);
   LOCALE DllExport void                *GetNextFact(void *);
   LOCALE void                          *GetNextFactInScope(void *);
   LOCALE DllExport void                 GetFactPPForm(char *,int,void *);
   LOCALE long int                       FactIndex(void *);
   LOCALE DllExport void                *AssertString(char *);
   LOCALE DllExport int                  GetFactListChanged(void);
   LOCALE DllExport void                 SetFactListChanged(int);
   LOCALE DllExport long int             GetNumberOfFacts(void);
   LOCALE void                           InitializeFacts(void);
   LOCALE struct fact                   *FindIndexedFact(long);
   LOCALE DllExport void                 IncrementFactCount(void *);
   LOCALE DllExport void                 DecrementFactCount(void *);
   LOCALE void                           PrintFactIdentifier(char *,void *);
   LOCALE void                           DecrementFactBasisCount(void *);
   LOCALE void                           IncrementFactBasisCount(void *);
   LOCALE void                           ReturnFact(struct fact *);
   LOCALE void                           MatchFactFunction(void *);
   LOCALE DllExport struct fact         *CreateFact(void *);
   LOCALE DllExport BOOLEAN              PutFactSlot(void *,char *,DATA_OBJECT *);
   LOCALE DllExport BOOLEAN              GetFactSlot(void *,char *,DATA_OBJECT *);
   LOCALE DllExport BOOLEAN              AssignFactSlotDefaults(void *);
   LOCALE BOOLEAN                        CopyFactSlotValues(void *,void *);

#ifndef _FACTMNGR_SOURCE_
   extern Thread int                            ChangeToFactList;
   extern Thread struct fact                    DummyFact;
#if DEBUGGING_FUNCTIONS
   extern Thread int                            WatchFacts;
#endif
#endif

#endif





