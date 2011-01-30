   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                DEFMODULE HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defmodule primitive functions such */
/*   as allocating and deallocating, traversing, and finding */
/*   defmodule data structures.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_moduldef
#define _H_moduldef

struct defmodule;
struct portItem;
struct defmoduleItemHeader;
struct moduleItem;

#ifndef _STDIO_INCLUDED_
#include <stdio.h>
#define _STDIO_INCLUDED_
#endif

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_constrct
#include "constrct.h"
#endif

/**********************************************************************/
/* defmodule                                                          */
/* ----------                                                         */
/* name: The name of the defmodule (stored as a reference in the      */
/*   table).                                                          */
/*                                                                    */
/* ppForm: The pretty print representation of the defmodule (used by  */
/*   the save and ppdefmodule commands).                              */
/*                                                                    */
/* itemsArray: An array of pointers to the module specific data used  */
/*   by each construct specified with the RegisterModuleItem          */
/*   function. The data pointer stored in the array is allocated by   */
/*   the allocateFunction in moduleItem data structure.               */
/*                                                                    */
/* importList: The list of items which are being imported by this     */
/*   module from other modules.                                       */
/*                                                                    */
/* next: A pointer to the next defmodule data structure.              */
/**********************************************************************/
struct defmodule
  {
   struct symbolHashNode *name;
   char *ppForm;
   struct defmoduleItemHeader **itemsArray;
   struct portItem *importList;
   struct portItem *exportList;
   unsigned visitedFlag;
   long bsaveID;
   struct userData *usrData;
   struct defmodule *next;
  };

struct portItem
  {
   struct symbolHashNode *moduleName;
   struct symbolHashNode *constructType;
   struct symbolHashNode *constructName;
   struct portItem *next;
  };

struct defmoduleItemHeader
  {
   struct defmodule *theModule;
   struct constructHeader *firstItem;
   struct constructHeader *lastItem;
  };

#define MIHS (struct defmoduleItemHeader *)

/**********************************************************************/
/* moduleItem                                                         */
/* ----------                                                         */
/* name: The name of the construct which can be placed in a module.   */
/*   For example, "defrule".                                          */
/*                                                                    */
/* allocateFunction: Used to allocate a data structure containing all */
/*   pertinent information related to a specific construct for a      */
/*   given module. For example, the deffacts construct stores a       */
/*   pointer to the first and last deffacts for each each module.     */
/*                                                                    */
/* freeFunction: Used to deallocate a data structure allocated by     */
/*   the allocateFunction. In addition, the freeFunction deletes      */
/*   all constructs of the specified type in the given module.        */
/*                                                                    */
/* bloadModuleReference: Used during a binary load to establish a     */
/*   link between the defmodule data structure and the data structure */
/*   containing all pertinent module information for a specific       */
/*   construct.                                                       */
/*                                                                    */
/* findFunction: Used to determine if a specified construct is in a   */
/*   specific module. The name is the specific construct is passed as */
/*   a string and the function returns a pointer to the specified     */
/*   construct if it exists.                                          */
/*                                                                    */
/* exportable: If TRUE, then the specified construct type can be      */
/*   exported (and hence imported). If FALSE, it can't be exported.   */
/*                                                                    */
/* next: A pointer to the next moduleItem data structure.             */
/**********************************************************************/

struct moduleItem
  {
   char *name;
   int moduleIndex;
   void *(*allocateFunction)(void);
   void  (*freeFunction)(void *);
   void *(*bloadModuleReference)(int);
   void  (*constructsToCModuleReference)(FILE *,int,int,int);
   void *(*findFunction)(char *);
   struct moduleItem *next;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MODULDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializeDefmodules(void);
   LOCALE void                          *FindDefmodule(char *);
   LOCALE char                          *GetDefmoduleName(void *);
   LOCALE char                          *GetDefmodulePPForm(void *);
   LOCALE void                          *GetNextDefmodule(void *);
   LOCALE void                           RemoveAllDefmodules(void);
   LOCALE int                            AllocateModuleStorage(void);
   LOCALE int                            RegisterModuleItem(char *,
                                                            void *(*)(void),
                                                            void (*)(void *),
                                                            void *(*)(int),
                                                            void (*)(FILE *,int,int,int),
                                                            void *(*)(char *));
   LOCALE void                          *GetModuleItem(struct defmodule *,int);
   LOCALE void                           SetModuleItem(struct defmodule *,int,void *);
   LOCALE DllExport void                *GetCurrentModule(void);
   LOCALE void                          *SetCurrentModule(void *);
   LOCALE SYMBOL_HN                     *GetCurrentModuleCommand(void);
   LOCALE SYMBOL_HN                     *SetCurrentModuleCommand(void);
   LOCALE int                            GetNumberOfModuleItems(void);
   LOCALE void                           CreateMainModule(void);
   LOCALE void                           SetListOfDefmodules(void *);
   LOCALE struct moduleItem             *GetListOfModuleItems(void);
   LOCALE struct moduleItem             *FindModuleItem(char *);
   LOCALE void                           SaveCurrentModule(void);
   LOCALE void                           RestoreCurrentModule(void);
   LOCALE void                           AddAfterModuleChangeFunction(char *,void (*)(void),int);
   LOCALE void                           IllegalModuleSpecifierMessage(void);

#ifndef _MODULDEF_SOURCE_
   extern Thread struct defmodule              *ListOfDefmodules;
   extern Thread struct defmodule              *CurrentModule;
   extern Thread struct defmodule              *LastDefmodule;
   extern Thread int                            NumberOfModuleItems;
   extern Thread struct moduleItem             *ListOfModuleItems;
   extern Thread long                           ModuleChangeIndex;
   extern Thread int                            MainModuleRedefinable;
#endif

#endif



