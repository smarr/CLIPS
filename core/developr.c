   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
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
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _DEVELOPR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "router.h"
#include "argacces.h"
#include "extnfunc.h"
#include "modulutl.h"

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT
#include "tmpltdef.h"
#include "factbld.h"
#include "facthsh.h"
#endif

#if INSTANCE_PATTERN_MATCHING
#include "classcom.h"
#include "classfun.h"
#include "objrtmch.h"
#endif

#include "developr.h"

#if DEVELOPER

#if INSTANCE_PATTERN_MATCHING
static void PrintOPNLevel(OBJECT_PATTERN_NODE *,char *,int);
#endif

/**************************************************/
/* DeveloperCommands: Sets up developer commands. */
/**************************************************/
globle void DeveloperCommands()
  {
#if ! RUN_TIME
   DefineFunction2("primitives-info",'v', PTIF PrimitiveTablesInfo,"PrimitiveTablesInfo","00");

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT
   DefineFunction2("show-fpn",'v', PTIF ShowFactPatternNetwork,"ShowFactPatternNetwork","11w");
   DefineFunction2("show-fht",'v', PTIF ShowFactHashTable,"ShowFactHashTable","00");
#endif
#if INSTANCE_PATTERN_MATCHING
   DefineFunction2("show-opn",'v',PTIF PrintObjectPatternNetwork,
                   "PrintObjectPatternNetwork","00");
#endif

#endif
  }

/******************************************************/
/* PrimitiveTablesInfo: Prints information about the  */
/*   symbol, float, integer, and bitmap tables.       */
/******************************************************/
globle void PrimitiveTablesInfo()
  {
   int i;
   SYMBOL_HN **symbolArray, *symbolPtr;
   FLOAT_HN **floatArray, *floatPtr;
   INTEGER_HN **integerArray, *integerPtr;
   BITMAP_HN **bitMapArray, *bitMapPtr;
   unsigned long int symbolCount = 0, integerCount = 0;
   unsigned long int floatCount = 0, bitMapCount = 0;

   ArgCountCheck("primitives-info",EXACTLY,0);

   /*====================================*/
   /* Count entries in the symbol table. */
   /*====================================*/

   symbolArray = GetSymbolTable();
   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i]; symbolPtr != NULL; symbolPtr = symbolPtr->next)
        { symbolCount++; }
     }

   /*====================================*/
   /* Count entries in the integer table. */
   /*====================================*/

   integerArray = GetIntegerTable();
   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i]; integerPtr != NULL; integerPtr = integerPtr->next)
        { integerCount++; }
     }

   /*====================================*/
   /* Count entries in the float table. */
   /*====================================*/

   floatArray = GetFloatTable();
   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i]; floatPtr != NULL; floatPtr = floatPtr->next)
        { floatCount++; }
     }

   /*====================================*/
   /* Count entries in the bitmap table. */
   /*====================================*/

   bitMapArray = GetBitMapTable();
   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i]; bitMapPtr != NULL; bitMapPtr = bitMapPtr->next)
        { bitMapCount++; }
     }

   /*========================*/
   /* Print the information. */
   /*========================*/

   PrintRouter(WDISPLAY,"Symbols: ");
   PrintLongInteger(WDISPLAY,(long) symbolCount);
   PrintRouter(WDISPLAY,"\n");
   PrintRouter(WDISPLAY,"Integers: ");
   PrintLongInteger(WDISPLAY,(long) integerCount);
   PrintRouter(WDISPLAY,"\n");
   PrintRouter(WDISPLAY,"Floats: ");
   PrintLongInteger(WDISPLAY,(long) floatCount);
   PrintRouter(WDISPLAY,"\n");
   PrintRouter(WDISPLAY,"BitMaps: ");
   PrintLongInteger(WDISPLAY,(long) bitMapCount);
   PrintRouter(WDISPLAY,"\n");
  }

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT

/*******************************************************/
/* ShowFactPatternNetwork: Command for displaying the  */
/*   fact pattern network for a specified deftemplate. */
/*******************************************************/
globle void ShowFactPatternNetwork()
  {
   struct factPatternNode *patternPtr;
   struct deftemplate *theDeftemplate;
   char *theName;
   int depth = 0, i;

   theName = GetConstructName("show-fpn","template name");
   if (theName == NULL) return;

   theDeftemplate = (struct deftemplate *) FindDeftemplate(theName);
   if (theDeftemplate == NULL) return;

   patternPtr = theDeftemplate->patternNetwork;
   while (patternPtr != NULL)
     {
      for (i = 0; i < depth; i++) PrintRouter(WDISPLAY," ");
      if (patternPtr->header.singlefieldNode) PrintRouter(WDISPLAY,"SF   ");
      else if (patternPtr->header.multifieldNode)
        {
         PrintRouter(WDISPLAY,"MF");
         if (patternPtr->header.endSlot) PrintRouter(WDISPLAY,")");
         else PrintRouter(WDISPLAY,"*");
         PrintLongInteger(WDISPLAY,(long) patternPtr->leaveFields);
         PrintRouter(WDISPLAY," ");
        }

      PrintRouter(WDISPLAY,"Slot: ");

      PrintLongInteger(WDISPLAY,(long) patternPtr->whichSlot);
      PrintRouter(WDISPLAY," Field: ");
      PrintLongInteger(WDISPLAY,(long) patternPtr->whichField);
      PrintRouter(WDISPLAY," Expression: ");
      if (patternPtr->networkTest == NULL) PrintRouter(WDISPLAY,"None");
      else PrintExpression(WDISPLAY,patternPtr->networkTest);
      PrintRouter(WDISPLAY,"\n");

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

#if INSTANCE_PATTERN_MATCHING

/***************************************************
  NAME         : PrintObjectPatternNetwork
  DESCRIPTION  : Displays an indented printout of
                 the object pattern network
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Object pattern network displayed
  NOTES        : None
 ***************************************************/
globle void PrintObjectPatternNetwork()
  {
   char indentbuf[80];

   indentbuf[0] = '\0';
   PrintOPNLevel(ObjectNetworkPointer(),indentbuf,0);
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
      PrintRouter(WDISPLAY,indentbuf);
      if (pptr->alphaNode != NULL)
        PrintRouter(WDISPLAY,"+");
      PrintRouter(WDISPLAY,ValueToString(FindIDSlotName(pptr->slotNameID)));
      PrintRouter(WDISPLAY," (");
      PrintLongInteger(WDISPLAY,(long) pptr->slotNameID);
      PrintRouter(WDISPLAY,") ");
      PrintRouter(WDISPLAY,pptr->endSlot ? "EPF#" : "PF#");
      PrintLongInteger(WDISPLAY,(long) pptr->whichField);
      PrintRouter(WDISPLAY," ");
      PrintRouter(WDISPLAY,pptr->multifieldNode ? "$? " : "? ");
      if (pptr->networkTest != NULL)
        PrintExpression(WDISPLAY,pptr->networkTest);
      PrintRouter(WDISPLAY,"\n");
      alphaPtr = pptr->alphaNode;
      while (alphaPtr != NULL)
        {
         PrintRouter(WDISPLAY,indentbuf);
         PrintRouter(WDISPLAY,"     Classes:");
         cbmp = (CLASS_BITMAP *) ValueToBitMap(alphaPtr->classbmp);
         for (i = 0 ; i <= cbmp->maxid ; i++)
           if (TestBitMap(cbmp->map,i))
             {
              PrintRouter(WDISPLAY," ");
              PrintRouter(WDISPLAY,GetDefclassName((void *) ClassIDMap[i]));
             }
         if (alphaPtr->slotbmp != NULL)
           {
            sbmp = (SLOT_BITMAP *) ValueToBitMap(pptr->alphaNode->slotbmp);
            PrintRouter(WDISPLAY," *** Slots:");
            for (i = NAME_ID ; i <= sbmp->maxid ; i++)
              if (TestBitMap(sbmp->map,i))
                {
                 for (uptr = pptr ; uptr != NULL ; uptr  = uptr->lastLevel)
                   if (uptr->slotNameID == i)
                     break;
                 if (uptr == NULL)
                   {
                    PrintRouter(WDISPLAY," ");
                    PrintRouter(WDISPLAY,ValueToString(FindIDSlotName(i)));
                   }
                }
           }
         PrintRouter(WDISPLAY,"\n");
         alphaPtr = alphaPtr->nxtInGroup;
        }
      indentbuf[ilen++] = (char) (pptr->rightNode != NULL) ? '|' : ' ';
      indentbuf[ilen++] = ' ';
      indentbuf[ilen++] = ' ';
      indentbuf[ilen] = '\0';
      PrintOPNLevel(pptr->nextLevel,indentbuf,ilen);
      ilen -= 3;
      indentbuf[ilen] = '\0';
      pptr = pptr->rightNode;
     }
  }

#endif

#endif


