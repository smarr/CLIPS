   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*                   DEVELOPER MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines useful for browsing various    */
/*   data structures. The functions are provided for         */
/*   development use.                                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*************************************************************/

#define _DEVELOPR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "argacces.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "inscom.h"
#include "modulutl.h"
#include "router.h"
#include "utility.h"

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT
#include "tmpltdef.h"
#include "factbld.h"
#include "facthsh.h"
#endif

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
#include "classcom.h"
#include "classfun.h"
#include "objrtmch.h"
#endif
#if OBJECT_SYSTEM
#include "insfun.h"
#endif

#include "developr.h"

#if DEVELOPER

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
static void PrintOPNLevel(void *theEnv,EXEC_STATUS, OBJECT_PATTERN_NODE *,char *,int);
#endif

/**************************************************/
/* DeveloperCommands: Sets up developer commands. */
/**************************************************/
globle void DeveloperCommands(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,execStatus,"primitives-info",'v', PTIEF PrimitiveTablesInfo,"PrimitiveTablesInfo","00");
   EnvDefineFunction2(theEnv,execStatus,"primitives-usage",'v', PTIEF PrimitiveTablesUsage,"PrimitiveTablesUsage","00");
   EnvDefineFunction2(theEnv,execStatus,"enable-gc-heuristics",'v', PTIEF EnableGCHeuristics,"EnableGCHeuristics","00");
   EnvDefineFunction2(theEnv,execStatus,"disable-gc-heuristics",'v', PTIEF DisableGCHeuristics,"DisableGCHeuristics","00");

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT
   EnvDefineFunction2(theEnv,execStatus,"show-fpn",'v', PTIEF ShowFactPatternNetwork,"ShowFactPatternNetwork","11w");
   EnvDefineFunction2(theEnv,execStatus,"show-fht",'v', PTIEF ShowFactHashTable,"ShowFactHashTable","00");
#endif

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   EnvDefineFunction2(theEnv,execStatus,"show-opn",'v',PTIEF PrintObjectPatternNetwork,
                   "PrintObjectPatternNetwork","00");
#endif

#if OBJECT_SYSTEM
   EnvDefineFunction2(theEnv,execStatus,"instance-table-usage",'v', PTIEF InstanceTableUsage,"InstanceTableUsage","00");
#endif

#endif
  }

/******************************************************/
/* EnableGCHeuristics:      */
/******************************************************/
globle void EnableGCHeuristics(
  void *theEnv,
  EXEC_STATUS)
  {
   EnvArgCountCheck(theEnv,execStatus,"enable-gc-heuristics",EXACTLY,0);
   SetGarbageCollectionHeuristics(theEnv,execStatus,TRUE);
  }
  
/******************************************************/
/* DisableGCHeuristics:      */
/******************************************************/
globle void DisableGCHeuristics(
  void *theEnv,
  EXEC_STATUS)
  {
   EnvArgCountCheck(theEnv,execStatus,"disable-gc-heuristics",EXACTLY,0);
   SetGarbageCollectionHeuristics(theEnv,execStatus,FALSE);
  }

/******************************************************/
/* PrimitiveTablesInfo: Prints information about the  */
/*   symbol, float, integer, and bitmap tables.       */
/******************************************************/
globle void PrimitiveTablesInfo(
  void *theEnv,
  EXEC_STATUS)
  {
   unsigned long i;
   SYMBOL_HN **symbolArray, *symbolPtr;
   FLOAT_HN **floatArray, *floatPtr;
   INTEGER_HN **integerArray, *integerPtr;
   BITMAP_HN **bitMapArray, *bitMapPtr;
   unsigned long int symbolCount = 0, integerCount = 0;
   unsigned long int floatCount = 0, bitMapCount = 0;

   EnvArgCountCheck(theEnv,execStatus,"primitives-info",EXACTLY,0);

   /*====================================*/
   /* Count entries in the symbol table. */
   /*====================================*/

   symbolArray = GetSymbolTable(theEnv,execStatus);
   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i]; symbolPtr != NULL; symbolPtr = symbolPtr->next)
        { symbolCount++; }
     }

   /*====================================*/
   /* Count entries in the integer table. */
   /*====================================*/

   integerArray = GetIntegerTable(theEnv,execStatus);
   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i]; integerPtr != NULL; integerPtr = integerPtr->next)
        { integerCount++; }
     }

   /*====================================*/
   /* Count entries in the float table. */
   /*====================================*/

   floatArray = GetFloatTable(theEnv,execStatus);
   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i]; floatPtr != NULL; floatPtr = floatPtr->next)
        { floatCount++; }
     }

   /*====================================*/
   /* Count entries in the bitmap table. */
   /*====================================*/

   bitMapArray = GetBitMapTable(theEnv,execStatus);
   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i]; bitMapPtr != NULL; bitMapPtr = bitMapPtr->next)
        { bitMapCount++; }
     }

   /*========================*/
   /* Print the information. */
   /*========================*/

   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Symbols: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) symbolCount);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Integers: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) integerCount);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Floats: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) floatCount);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"BitMaps: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) bitMapCount);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   /*
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Ephemerals: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) EphemeralSymbolCount());
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   */
  }
  
#define COUNT_SIZE 21

/******************************************************/
/* PrimitiveTablesUsage: Prints information about the  */
/*   symbol, float, integer, and bitmap tables.       */
/******************************************************/
globle void PrimitiveTablesUsage(
  void *theEnv,
  EXEC_STATUS)
  {
   unsigned long i;
   int symbolCounts[COUNT_SIZE], floatCounts[COUNT_SIZE];
   SYMBOL_HN **symbolArray, *symbolPtr;
   FLOAT_HN **floatArray, *floatPtr;
   unsigned long int symbolCount, totalSymbolCount = 0;
   unsigned long int floatCount, totalFloatCount = 0;

   EnvArgCountCheck(theEnv,execStatus,"primitives-usage",EXACTLY,0);

   for (i = 0; i < 21; i++)
     {
      symbolCounts[i] = 0;
      floatCounts[i] = 0; 
     }
     
   /*====================================*/
   /* Count entries in the symbol table. */
   /*====================================*/

   symbolArray = GetSymbolTable(theEnv,execStatus);
   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      symbolCount = 0;
      for (symbolPtr = symbolArray[i]; symbolPtr != NULL; symbolPtr = symbolPtr->next)
        { 
         symbolCount++;
         totalSymbolCount++;
        }
           
      if (symbolCount < (COUNT_SIZE - 1))
        { symbolCounts[symbolCount]++; }
      else
        { symbolCounts[COUNT_SIZE - 1]++; }
     }

   /*===================================*/
   /* Count entries in the float table. */
   /*===================================*/
   
   floatArray = GetFloatTable(theEnv,execStatus);
   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      floatCount = 0;
      for (floatPtr = floatArray[i]; floatPtr != NULL; floatPtr = floatPtr->next)
        { 
         floatCount++;
         totalFloatCount++;
        }
           
      if (floatCount < (COUNT_SIZE - 1))
        { floatCounts[floatCount]++; }
      else
        { floatCounts[COUNT_SIZE - 1]++; }
     }


   /*========================*/
   /* Print the information. */
   /*========================*/

   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Total Symbols: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) totalSymbolCount);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   for (i = 0; i < COUNT_SIZE; i++)
     {
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) i);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) symbolCounts[i]);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
     }

   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\nTotal Floats: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) totalFloatCount);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   for (i = 0; i < COUNT_SIZE; i++)
     {
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) i);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) floatCounts[i]);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
     }

  }

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT

/*******************************************************/
/* ShowFactPatternNetwork: Command for displaying the  */
/*   fact pattern network for a specified deftemplate. */
/*******************************************************/
globle void ShowFactPatternNetwork(
  void *theEnv,
  EXEC_STATUS)
  {
   struct factPatternNode *patternPtr;
   struct deftemplate *theDeftemplate;
   char *theName;
   int depth = 0, i;

   theName = GetConstructName(theEnv,execStatus,"show-fpn","template name");
   if (theName == NULL) return;

   theDeftemplate = (struct deftemplate *) EnvFindDeftemplate(theEnv,execStatus,theName);
   if (theDeftemplate == NULL) return;

   patternPtr = theDeftemplate->patternNetwork;
   while (patternPtr != NULL)
     {
      for (i = 0; i < depth; i++) EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
      if (patternPtr->header.singlefieldNode) EnvPrintRouter(theEnv,execStatus,WDISPLAY,"SF   ");
      else if (patternPtr->header.multifieldNode)
        {
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"MF");
         if (patternPtr->header.endSlot) EnvPrintRouter(theEnv,execStatus,WDISPLAY,")");
         else EnvPrintRouter(theEnv,execStatus,WDISPLAY,"*");
         PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) patternPtr->leaveFields);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
        }

      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Slot: ");

      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) patternPtr->whichSlot);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," Field: ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) patternPtr->whichField);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," Expression: ");
      if (patternPtr->networkTest == NULL) EnvPrintRouter(theEnv,execStatus,WDISPLAY,"None");
      else PrintExpression(theEnv,execStatus,WDISPLAY,patternPtr->networkTest);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," RightHash: ");
      if (patternPtr->header.rightHash == NULL) EnvPrintRouter(theEnv,execStatus,WDISPLAY,"None");
      else PrintExpression(theEnv,execStatus,WDISPLAY,patternPtr->header.rightHash);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");

      if (patternPtr->nextLevel == NULL)
        {
         while (patternPtr->rightNode == NULL)
           {
            patternPtr = patternPtr->lastLevel;
            depth--;
            if (patternPtr == NULL) return;
           }
         patternPtr = patternPtr->rightNode;
        }
      else
        {
         patternPtr = patternPtr->nextLevel;
         depth++;
        }
     }
  }

#endif

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM

/***************************************************
  NAME         : PrintObjectPatternNetwork
  DESCRIPTION  : Displays an indented printout of
                 the object pattern network
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Object pattern network displayed
  NOTES        : None
 ***************************************************/
globle void PrintObjectPatternNetwork(
  void *theEnv,
  EXEC_STATUS)
  {
   char indentbuf[80];

   indentbuf[0] = '\0';
   PrintOPNLevel(theEnv,execStatus,ObjectNetworkPointer(theEnv,execStatus),indentbuf,0);
  }

/**********************************************************
  NAME         : PrintOPNLevel
  DESCRIPTION  : Recursivley prints object pattern network
  INPUTS       : 1) The current object pattern network node
                 2) A buffer holding preceding indentation
                    text showing the level in the tree
                 3) The length of the indentation text
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pattern nodes recursively printed
  NOTES        : None
 **********************************************************/
static void PrintOPNLevel(
  void *theEnv,
  EXEC_STATUS,
  OBJECT_PATTERN_NODE *pptr,
  char *indentbuf,
  int ilen)
  {
   CLASS_BITMAP *cbmp;
   SLOT_BITMAP *sbmp;
   register unsigned i;
   OBJECT_PATTERN_NODE *uptr;
   OBJECT_ALPHA_NODE *alphaPtr;

   while (pptr != NULL)
     {
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,indentbuf);
      if (pptr->alphaNode != NULL)
        EnvPrintRouter(theEnv,execStatus,WDISPLAY,"+");
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,ValueToString(FindIDSlotName(theEnv,execStatus,pptr->slotNameID)));
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," (");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) pptr->slotNameID);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,") ");
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,pptr->endSlot ? "EPF#" : "PF#");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) pptr->whichField);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,pptr->multifieldNode ? "$? " : "? ");
      if (pptr->networkTest != NULL)
        PrintExpression(theEnv,execStatus,WDISPLAY,pptr->networkTest);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
      alphaPtr = pptr->alphaNode;
      while (alphaPtr != NULL)
        {
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,indentbuf);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"     Classes:");
         cbmp = (CLASS_BITMAP *) ValueToBitMap(alphaPtr->classbmp);
         for (i = 0 ; i <= cbmp->maxid ; i++)
           if (TestBitMap(cbmp->map,i))
             {
              EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
              EnvPrintRouter(theEnv,execStatus,WDISPLAY,EnvGetDefclassName(theEnv,execStatus,(void *) DefclassData(theEnv,execStatus)->ClassIDMap[i]));
             }
         if (alphaPtr->slotbmp != NULL)
           {
            sbmp = (SLOT_BITMAP *) ValueToBitMap(pptr->alphaNode->slotbmp);
            EnvPrintRouter(theEnv,execStatus,WDISPLAY," *** Slots:");
            for (i = NAME_ID ; i <= sbmp->maxid ; i++)
              if (TestBitMap(sbmp->map,i))
                {
                 for (uptr = pptr ; uptr != NULL ; uptr  = uptr->lastLevel)
                   if (uptr->slotNameID == i)
                     break;
                 if (uptr == NULL)
                   {
                    EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
                    EnvPrintRouter(theEnv,execStatus,WDISPLAY,ValueToString(FindIDSlotName(theEnv,execStatus,i)));
                   }
                }
           }
         if (alphaPtr->header.rightHash != NULL)
           {
            EnvPrintRouter(theEnv,execStatus,WDISPLAY," RH: ");
            PrintExpression(theEnv,execStatus,WDISPLAY,alphaPtr->header.rightHash);
           }

         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
         alphaPtr = alphaPtr->nxtInGroup;
        }
      indentbuf[ilen++] = (char) ((pptr->rightNode != NULL) ? '|' : ' ');
      indentbuf[ilen++] = ' ';
      indentbuf[ilen++] = ' ';
      indentbuf[ilen] = '\0';
      PrintOPNLevel(theEnv,execStatus,pptr->nextLevel,indentbuf,ilen);
      ilen -= 3;
      indentbuf[ilen] = '\0';
      pptr = pptr->rightNode;
     }
  }

#endif

#if OBJECT_SYSTEM

/******************************************************/
/* InstanceTableUsage: Prints information about the  */
/*   instances in the instance hash table.       */
/******************************************************/
globle void InstanceTableUsage(
  void *theEnv,
  EXEC_STATUS)
  {
   unsigned long i;
   int instanceCounts[COUNT_SIZE];
   INSTANCE_TYPE *ins;
   unsigned long int instanceCount, totalInstanceCount = 0;

   EnvArgCountCheck(theEnv,execStatus,"instance-table-usage",EXACTLY,0);

   for (i = 0; i < COUNT_SIZE; i++)
     { instanceCounts[i] = 0; }
     
   /*======================================*/
   /* Count entries in the instance table. */
   /*======================================*/

   for (i = 0; i < INSTANCE_TABLE_HASH_SIZE; i++)
     {
      instanceCount = 0;
      for (ins = InstanceData(theEnv,execStatus)->InstanceTable[i]; ins != NULL; ins = ins->nxtHash)
        { 
         instanceCount++;
         totalInstanceCount++;
        }
           
      if (instanceCount < (COUNT_SIZE - 1))
        { instanceCounts[instanceCount]++; }
      else
        { instanceCounts[COUNT_SIZE - 1]++; }
     }

   /*========================*/
   /* Print the information. */
   /*========================*/

   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Total Instances: ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) totalInstanceCount);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
   for (i = 0; i < COUNT_SIZE; i++)
     {
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) i);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY," ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long long) instanceCounts[i]);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
     }
  }
  
#endif

#endif


