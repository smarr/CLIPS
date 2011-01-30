   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                    SYMBOL MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Manages the atomic data value hash tables for    */
/*   storing symbols, integers, floats, and bit maps.        */
/*   Contains routines for adding entries, examining the     */
/*   hash tables, and performing garbage collection to       */
/*   remove entries no longer in use.                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _SYMBOL_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>

#include "setup.h"
#include "constant.h"
#include "memalloc.h"
#include "router.h"
#include "utility.h"
#include "argacces.h"
#include "symbol.h"

/**********************************************************/
/* EPHEMERON STRUCTURE: Data structure used to keep track */
/*   of ephemeral symbols, floats, and integers.          */
/*                                                        */
/*   associatedValue: Contains a pointer to the storage   */
/*   structure for the symbol, float, or integer which is */
/*   ephemeral.                                           */
/*                                                        */
/*   next: Contains a pointer to the next ephemeral item  */
/*   in a list of ephemeral items.                        */
/**********************************************************/
struct ephemeron
  {
   GENERIC_HN *associatedValue;
   struct ephemeron *next;
  };

/***************/
/* DEFINITIONS */
/***************/

#define FALSE_STRING "FALSE"
#define TRUE_STRING  "TRUE"
#define POSITIVE_INFINITY_STRING "+oo"
#define NEGATIVE_INFINITY_STRING "-oo"

#define AVERAGE_STRING_SIZE 10
#define AVERAGE_BITMAP_SIZE sizeof(long)
#define NUMBER_OF_LONGS_FOR_HASH 25

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    RemoveHashNode(GENERIC_HN *,GENERIC_HN **,int,int);
   static void                    AddEphemeralHashNode(GENERIC_HN *,struct ephemeron **,
                                                       int,int);
   static void                    RemoveEphemeralHashNodes(struct ephemeron **,
                                                           GENERIC_HN **,
                                                           int,int,int);
   static char                   *StringWithinString(char *,char *);
   static int                     CommonPrefixLength(char *,char *);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle void               *TrueSymbol;
   Thread globle void               *FalseSymbol;
   Thread globle void               *PositiveInfinity;
   Thread globle void               *NegativeInfinity;
   Thread globle void               *Zero;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static SYMBOL_HN         **SymbolTable;
   Thread static FLOAT_HN          **FloatTable;
   Thread static INTEGER_HN        **IntegerTable;
   Thread static BITMAP_HN         **BitMapTable;
   Thread static struct ephemeron   *EphemeralSymbolList = NULL;
   Thread static struct ephemeron   *EphemeralFloatList = NULL;
   Thread static struct ephemeron   *EphemeralIntegerList = NULL;
   Thread static struct ephemeron   *EphemeralBitMapList = NULL;

/********************************************************************/
/* AddSymbol: Searches for the string in the symbol table. If the   */
/*   string is already in the symbol table, then the address of the */
/*   string's location in the symbol table is returned. Otherwise,  */
/*   the string is added to the symbol table and then the address   */
/*   of the string's location in the symbol table is returned.      */
/********************************************************************/
globle void *AddSymbol(
   char *str)
   {
    int tally, length;
    SYMBOL_HN *past = NULL, *peek;

    /*====================================*/
    /* Get the hash value for the string. */
    /*====================================*/

    if (str == NULL)
      {
       SystemError("SYMBOL",1);
       ExitRouter(EXIT_FAILURE);
      }

    tally = HashSymbol(str,SYMBOL_HASH_SIZE);
    peek = SymbolTable[tally];

    /*==================================================*/
    /* Search for the string in the list of entries for */
    /* this symbol table location.  If the string is    */
    /* found, then return the address of the string.    */
    /*==================================================*/

    while (peek != NULL)
      {
       if (strcmp(str,peek->contents) == 0)
         { return((void *) peek); }
       past = peek;
       peek = peek->next;
      }

    /*==================================================*/
    /* Add the string at the end of the list of entries */
    /* for this symbol table location.                  */
    /*==================================================*/

    peek = get_struct(symbolHashNode);

    if (past == NULL) SymbolTable[tally] = peek;
    else past->next = peek;

    length = strlen(str) + 1;
    peek->contents = (char *) gm2(length);
    peek->next = NULL;
    peek->bucket = tally;
    peek->count = 0;
    strcpy(peek->contents,str);

    /*================================================*/
    /* Add the string to the list of ephemeral items. */
    /*================================================*/

    AddEphemeralHashNode((GENERIC_HN *) peek,&EphemeralSymbolList,
                         sizeof(SYMBOL_HN),AVERAGE_STRING_SIZE);
    peek->depth = CurrentEvaluationDepth;

    /*===================================*/
    /* Return the address of the symbol. */
    /*===================================*/

    return((void *) peek);
   }

/***************************************************************/
/* FindSymbol: Searches for the string in the symbol table and */
/*   returns a pointer to it if found, otherwise returns NULL. */
/***************************************************************/
globle SYMBOL_HN *FindSymbol(
   char *str)
   {
    int tally;
    SYMBOL_HN *peek;

    tally = HashSymbol(str,SYMBOL_HASH_SIZE);

    for (peek = SymbolTable[tally];
         peek != NULL;
         peek = peek->next)
      { if (strcmp(str,peek->contents) == 0) return(peek); }

    return(NULL);
   }

/******************************************************************/
/* AddDouble: Searches for the double in the hash table. If the   */
/*   double is already in the hash table, then the address of the */
/*   double is returned. Otherwise, the double is hashed into the */
/*   table and the address of the double is also returned.        */
/******************************************************************/
globle void *AddDouble(
   double number)
   {
    int tally;
    FLOAT_HN *past = NULL, *peek;

    /*====================================*/
    /* Get the hash value for the double. */
    /*====================================*/

    tally = HashFloat(number,FLOAT_HASH_SIZE);
    peek = FloatTable[tally];

    /*==================================================*/
    /* Search for the double in the list of entries for */
    /* this hash location.  If the double is found,     */
    /* then return the address of the double.           */
    /*==================================================*/

    while (peek != NULL)
      {
       if (number == peek->contents)
         { return((void *) peek); }
       past = peek;
       peek = peek->next;
      }

    /*=================================================*/
    /* Add the float at the end of the list of entries */
    /* for this hash location.                         */
    /*=================================================*/

    peek = get_struct(floatHashNode);

    if (past == NULL) FloatTable[tally] = peek;
    else past->next = peek;

    peek->contents = number;
    peek->next = NULL;
    peek->bucket = tally;
    peek->count = 0;

    /*===============================================*/
    /* Add the float to the list of ephemeral items. */
    /*===============================================*/

    AddEphemeralHashNode((GENERIC_HN *) peek,&EphemeralFloatList,
                         sizeof(FLOAT_HN),0);
    peek->depth = CurrentEvaluationDepth;

    /*==================================*/
    /* Return the address of the float. */
    /*==================================*/

    return((void *) peek);
   }

/****************************************************************/
/* AddLong: Searches for the long in the hash table. If the     */
/*   long is already in the hash table, then the address of the */
/*   long is returned. Otherwise, the long is hashed into the   */
/*   table and the address of the long is also returned.        */
/****************************************************************/
globle void *AddLong(
   long int number)
   {
    int tally;
    INTEGER_HN *past = NULL, *peek;

    /*==================================*/
    /* Get the hash value for the long. */
    /*==================================*/

    tally = HashInteger(number,INTEGER_HASH_SIZE);
    peek = IntegerTable[tally];

    /*================================================*/
    /* Search for the long in the list of entries for */
    /* this hash location. If the long is found, then */
    /* return the address of the long.                */
    /*================================================*/

    while (peek != NULL)
      {
       if (number == peek->contents)
         { return((void *) peek); }
       past = peek;
       peek = peek->next;
      }

    /*================================================*/
    /* Add the long at the end of the list of entries */
    /* for this hash location.                        */
    /*================================================*/

    peek = get_struct(integerHashNode);
    if (past == NULL) IntegerTable[tally] = peek;
    else past->next = peek;

    peek->contents = number;
    peek->next = NULL;
    peek->bucket = tally;
    peek->count = 0;

    /*=================================================*/
    /* Add the integer to the list of ephemeral items. */
    /*=================================================*/

    AddEphemeralHashNode((GENERIC_HN *) peek,&EphemeralIntegerList,
                         sizeof(INTEGER_HN),0);
    peek->depth = CurrentEvaluationDepth;

    /*====================================*/
    /* Return the address of the integer. */
    /*====================================*/

    return((void *) peek);
   }

/***************************************************************/
/* FindLong: Searches for the integer in the integer table and */
/*   returns a pointer to it if found, otherwise returns NULL. */
/***************************************************************/
globle INTEGER_HN *FindLong(
   long int theLong)
   {
    int tally;
    INTEGER_HN *peek;

    tally = HashInteger(theLong,INTEGER_HASH_SIZE);

    for (peek = IntegerTable[tally];
         peek != NULL;
         peek = peek->next)
      { if (peek->contents == theLong) return(peek); }

    return(NULL);
   }

/******************************************************************/
/* AddBitMap: Searches for the bitmap in the hash table. If the   */
/*   bitmap is already in the hash table, then the address of the */
/*   bitmap is returned. Otherwise, the bitmap is hashed into the */
/*   table and the address of the bitmap is also returned.        */
/******************************************************************/
globle void *AddBitMap(
   void *vTheBitMap,
   int size)
   {
    char *theBitMap = (char *) vTheBitMap;
    int tally, i;
    BITMAP_HN *past = NULL, *peek;

    /*====================================*/
    /* Get the hash value for the bitmap. */
    /*====================================*/

    if (theBitMap == NULL)
      {
       SystemError("SYMBOL",2);
       ExitRouter(EXIT_FAILURE);
      }

    tally = HashBitMap(theBitMap,BITMAP_HASH_SIZE,size);
    peek = BitMapTable[tally];

    /*==================================================*/
    /* Search for the bitmap in the list of entries for */
    /* this hash table location.  If the bitmap is      */
    /* found, then return the address of the bitmap.    */
    /*==================================================*/

    while (peek != NULL)
      {
       if (peek->size == size)
         {
          for (i = 0; i < size ; i++)
            { if (peek->contents[i] != theBitMap[i]) break; }

          if (i == size) return((void *) peek);
         }

       past = peek;
       peek = peek->next;
      }

    /*==================================================*/
    /* Add the bitmap at the end of the list of entries */
    /* for this hash table location.  Return the        */
    /*==================================================*/

    peek = get_struct(bitMapHashNode);
    if (past == NULL) BitMapTable[tally] = peek;
    else past->next = peek;

    peek->contents = (char *) gm2(size);
    peek->next = NULL;
    peek->bucket = tally;
    peek->count = 0;
    peek->size = (unsigned short) size;

    for (i = 0; i < size ; i++) peek->contents[i] = theBitMap[i];

    /*================================================*/
    /* Add the bitmap to the list of ephemeral items. */
    /*================================================*/

    AddEphemeralHashNode((GENERIC_HN *) peek,&EphemeralBitMapList,
                         sizeof(BITMAP_HN),sizeof(long));
    peek->depth = CurrentEvaluationDepth;

    /*===================================*/
    /* Return the address of the bitmap. */
    /*===================================*/

    return((void *) peek);
   }

/*******************************************************/
/* InitializeAtomTables: Initializes the SymbolTable,  */
/*   IntegerTable, and FloatTable. It also initializes */
/*   the TrueSymbol and FalseSymbol.                   */
/*******************************************************/
globle void InitializeAtomTables()
   {
    int i;

    /*=========================*/
    /* Create the hash tables. */
    /*=========================*/

    SymbolTable = (SYMBOL_HN **)
                   gm2((int) sizeof (SYMBOL_HN *) * SYMBOL_HASH_SIZE);

    FloatTable = (FLOAT_HN **)
                   gm2((int) sizeof (FLOAT_HN *) * FLOAT_HASH_SIZE);

    IntegerTable = (INTEGER_HN **)
                    gm2((int) sizeof (INTEGER_HN *) * INTEGER_HASH_SIZE);

    BitMapTable = (BITMAP_HN **)
                    gm2((int) sizeof (BITMAP_HN *) * BITMAP_HASH_SIZE);

    /*===================================================*/
    /* Initialize all of the hash table entries to NULL. */
    /*===================================================*/

    for (i = 0; i < SYMBOL_HASH_SIZE; i++) SymbolTable[i] = NULL;
    for (i = 0; i < FLOAT_HASH_SIZE; i++) FloatTable[i] = NULL;
    for (i = 0; i < INTEGER_HASH_SIZE; i++) IntegerTable[i] = NULL;
    for (i = 0; i < BITMAP_HASH_SIZE; i++) BitMapTable[i] = NULL;

    /*========================*/
    /* Predefine some values. */
    /*========================*/

    TrueSymbol = AddSymbol(TRUE_STRING);
    IncrementSymbolCount(TrueSymbol);
    FalseSymbol = AddSymbol(FALSE_STRING);
    IncrementSymbolCount(FalseSymbol);
    PositiveInfinity = AddSymbol(POSITIVE_INFINITY_STRING);
    IncrementSymbolCount(PositiveInfinity);
    NegativeInfinity = AddSymbol(NEGATIVE_INFINITY_STRING);
    IncrementSymbolCount(NegativeInfinity);
    Zero = AddLong(0L);
    IncrementIntegerCount(Zero);
   }

/***************************************************/
/* HashSymbol: Computes a hash value for a symbol. */
/***************************************************/
globle int HashSymbol(
  char *word,
  int range)
  {
   register int k,j,i;
   register int length;
   int tally;
   unsigned long count = 0L,tmpLong;
   char *tmpPtr;

   tmpPtr = (char *) &tmpLong;

   /*===============================================*/
   /* Count the number of characters in the symbol. */
   /*===============================================*/

   for (length = 0; word[length]; length++)
     { /* Do Nothing */ }

   /*================================================================ */
   /* Add up the first part of the word as unsigned long int values.  */
   /*================================================================ */

   length = length / sizeof(unsigned long);
   for (i = 0 , j = 0 ; i < length; i++)
     {
      for (k = 0 ; k < sizeof(unsigned long) ; k++ , j++)
        tmpPtr[k] = word[j];
      count += tmpLong;
     }

   /*============================================*/
   /* Add the remaining characters to the count. */
   /*============================================*/

   tmpLong = 0L;
   for (word = (char *) &word[j], k = 0;
        *word;
        word++, k++)
     {
      tmpPtr[k] = *word;
      /* count += (unsigned long) *word; */
     }

   count += tmpLong;

   /*========================*/
   /* Return the hash value. */
   /*========================*/

   tally = (int) (count % range);
   if (tally < 0) return(-tally);

   return(tally);
  }

/*************************************************/
/* HashFloat: Computes a hash value for a float. */
/*************************************************/
globle int HashFloat(
  double number,
  int range)
  {
   union
     {
      double fv;
      unsigned long int liv;
     } fis;
   unsigned long count;
   int tally;

   fis.liv = 0;
   fis.fv = number;
   count = fis.liv;

   tally = (int) (count % range);

   if (tally < 0) return(-tally);

   return(tally);
  }

/******************************************************/
/* HashInteger: Computes a hash value for an integer. */
/******************************************************/
globle int HashInteger(
  long int number,
  int range)
  {
   int tally;

   tally = (int) (number % range);

   if (tally < 0) return(-tally);

   return(tally);
  }

/***************************************************/
/* HashBitMap: Computes a hash value for a bitmap. */
/***************************************************/
globle int HashBitMap(
  char *word,
  int range,
  int length)
  {
   register int k,j,i;
   int tally;
   int longLength;
   unsigned long count = 0L,tmpLong;
   char *tmpPtr;

   tmpPtr = (char *) &tmpLong;

   /*================================================================ */
   /* Add up the first part of the word as unsigned long int values.  */
   /*================================================================ */

   longLength = length / sizeof(unsigned long);
   for (i = 0 , j = 0 ; i < longLength; i++)
     {
      for (k = 0 ; k < sizeof(unsigned long) ; k++ , j++)
        tmpPtr[k] = word[j];
      count += tmpLong;
     }

   /*============================================*/
   /* Add the remaining characters to the count. */
   /*============================================*/

   for (; j < length; j++) count += (unsigned long) word[j];

   /*========================*/
   /* Return the hash value. */
   /*========================*/

   tally = (int) (count % range);
   if (tally < 0) return(-tally);

   return(tally);
  }

/*****************************************************/
/* DecrementSymbolCount: Decrements the count value  */
/*   for a SymbolTable entry. Adds the symbol to the */
/*   EphemeralSymbolList if the count becomes zero.  */
/*****************************************************/
globle void DecrementSymbolCount(
  SYMBOL_HN *theValue)
  {
   if (theValue->count < 0)
     {
      SystemError("SYMBOL",3);
      ExitRouter(EXIT_FAILURE);
     }

   if (theValue->count == 0)
     {
      SystemError("SYMBOL",4);
      ExitRouter(EXIT_FAILURE);
     }

   theValue->count--;

   if (theValue->count != 0) return;

   if (theValue->markedEphemeral == FALSE)
     {
      AddEphemeralHashNode((GENERIC_HN *) theValue,&EphemeralSymbolList,
                           sizeof(SYMBOL_HN),AVERAGE_STRING_SIZE);
     }

   return;
  }

/***************************************************/
/* DecrementFloatCount: Decrements the count value */
/*   for a FloatTable entry. Adds the float to the */
/*   EphemeralFloatList if the count becomes zero. */
/***************************************************/
globle void DecrementFloatCount(
  FLOAT_HN *theValue)
  {
   if (theValue->count <= 0)
     {
      SystemError("SYMBOL",5);
      ExitRouter(EXIT_FAILURE);
     }

   theValue->count--;

   if (theValue->count != 0) return;

   if (theValue->markedEphemeral == FALSE)
     {
      AddEphemeralHashNode((GENERIC_HN *) theValue,&EphemeralFloatList,
                           sizeof(FLOAT_HN),0);
     }

   return;
  }

/*********************************************************/
/* DecrementIntegerCount: Decrements the count value for */
/*   an IntegerTable entry. Adds the integer to the      */
/*   EphemeralIntegerList if the count becomes zero.     */
/*********************************************************/
globle void DecrementIntegerCount(
  INTEGER_HN *theValue)
  {
   if (theValue->count <= 0)
     {
      SystemError("SYMBOL",6);
      ExitRouter(EXIT_FAILURE);
     }

   theValue->count--;

   if (theValue->count != 0) return;

   if (theValue->markedEphemeral == FALSE)
     {
      AddEphemeralHashNode((GENERIC_HN *) theValue,&EphemeralIntegerList,
                           sizeof(INTEGER_HN),0);
     }

   return;
  }

/*****************************************************/
/* DecrementBitMapCount: Decrements the count value  */
/*   for a BitmapTable entry. Adds the bitmap to the */
/*   EphemeralBitMapList if the count becomes zero.  */
/*****************************************************/
globle void DecrementBitMapCount(
  BITMAP_HN *theValue)
  {
   if (theValue->count < 0)
     {
      SystemError("SYMBOL",7);
      ExitRouter(EXIT_FAILURE);
     }

   if (theValue->count == 0)
     {
      SystemError("SYMBOL",8);
      ExitRouter(EXIT_FAILURE);
     }

   theValue->count--;

   if (theValue->count != 0) return;

   if (theValue->markedEphemeral == FALSE)
     {
      AddEphemeralHashNode((GENERIC_HN *) theValue,&EphemeralBitMapList,
                           sizeof(BITMAP_HN),sizeof(long));
     }

   return;
  }

/*************************************************************/
/* RemoveHashNode: Removes a hash node from the SymbolTable, */
/*   FloatTable, IntegerTable, or BitMapTable.               */
/*************************************************************/
static void RemoveHashNode(
  GENERIC_HN *theValue,
  GENERIC_HN **theTable,
  int size,
  int type)
  {
   GENERIC_HN *previousNode, *currentNode;

   /*=============================================*/
   /* Find the entry in the specified hash table. */
   /*=============================================*/

   previousNode = NULL;
   currentNode = theTable[theValue->bucket];

   while (currentNode != theValue)
     {
      previousNode = currentNode;
      currentNode = currentNode->next;

      if (currentNode == NULL)
        {
         SystemError("SYMBOL",9);
         ExitRouter(EXIT_FAILURE);
        }
     }

   /*===========================================*/
   /* Remove the entry from the list of entries */
   /* stored in the hash table bucket.          */
   /*===========================================*/

   if (previousNode == NULL)
     { theTable[theValue->bucket] = theValue->next; }
   else
     { previousNode->next = currentNode->next; }

   /*=================================================*/
   /* Symbol and bit map nodes have additional memory */
   /* use to store the character or bitmap string.    */
   /*=================================================*/

   if (type == SYMBOL)
     {
      rm(((SYMBOL_HN *) theValue)->contents,
         (int) strlen(((SYMBOL_HN *) theValue)->contents) + 1);
     }
   else if (type == BITMAPARRAY)
     {
      rm(((BITMAP_HN *) theValue)->contents,
         (int) ((BITMAP_HN *) theValue)->size);
     }

   /*===========================*/
   /* Return the table entry to */
   /* the pool of free memory.  */
   /*===========================*/

   rtn_sized_struct(size,theValue);
  }

/***********************************************************/
/* AddEphemeralHashNode: Adds a symbol, integer, float, or */
/*   bit map table entry to the list of ephemeral atomic   */
/*   values. These entries have a zero count indicating    */
/*   that no structure is using the data value.            */
/***********************************************************/
static void AddEphemeralHashNode(
  GENERIC_HN *theHashNode,
  struct ephemeron **theEphemeralList,
  int hashNodeSize,
  int averageContentsSize)
  {
   struct ephemeron *temp;

   /*===========================================*/
   /* If the count isn't zero then this routine */
   /* should never have been called.            */
   /*===========================================*/

   if (theHashNode->count != 0)
     {
      SystemError("SYMBOL",10);
      ExitRouter(EXIT_FAILURE);
     }

   /*=====================================*/
   /* Mark the atomic value as ephemeral. */
   /*=====================================*/

   theHashNode->markedEphemeral = TRUE;

   /*=============================*/
   /* Add the atomic value to the */
   /* list of ephemeral values.   */
   /*=============================*/

   temp = get_struct(ephemeron);
   temp->associatedValue = theHashNode;
   temp->next = *theEphemeralList;
   *theEphemeralList = temp;

   /*=========================================================*/
   /* Increment the ephemeral count and size variables. These */
   /* variables are used by the garbage collection routines   */
   /* to determine when garbage collection should occur.      */
   /*=========================================================*/

   EphemeralItemCount++;
   EphemeralItemSize += sizeof(struct ephemeron) + hashNodeSize +
                        averageContentsSize;
  }

/***************************************************/
/* RemoveEphemeralAtoms: Causes the removal of all */
/*   ephemeral symbols, integers, floats, and bit  */
/*   maps that still have a count value of zero,   */
/*   from their respective storage tables.         */
/***************************************************/
globle void RemoveEphemeralAtoms()
  {
   RemoveEphemeralHashNodes(&EphemeralSymbolList,(GENERIC_HN **) SymbolTable,
                            sizeof(SYMBOL_HN),SYMBOL,AVERAGE_STRING_SIZE);
   RemoveEphemeralHashNodes(&EphemeralFloatList,(GENERIC_HN **) FloatTable,
                            sizeof(FLOAT_HN),FLOAT,0);
   RemoveEphemeralHashNodes(&EphemeralIntegerList,(GENERIC_HN **) IntegerTable,
                            sizeof(INTEGER_HN),INTEGER,0);
   RemoveEphemeralHashNodes(&EphemeralBitMapList,(GENERIC_HN **) BitMapTable,
                            sizeof(BITMAP_HN),BITMAPARRAY,AVERAGE_BITMAP_SIZE);
  }

/****************************************************************/
/* RemoveEphemeralHashNodes: Removes symbols from the ephemeral */
/*   symbol list that have a count of zero and were placed on   */
/*   the list at a higher level than the current evaluation     */
/*   depth. Since symbols are ordered in the list in descending */
/*   order, the removal process can end when a depth is reached */
/*   less than the current evaluation depth. Because ephemeral  */
/*   symbols can be "pulled" up through an evaluation depth,    */
/*   this routine needs to check through both the previous and  */
/*   current evaluation depth.                                  */
/****************************************************************/
static void RemoveEphemeralHashNodes(
  struct ephemeron **theEphemeralList,
  GENERIC_HN **theTable,
  int hashNodeSize,
  int hashNodeType,
  int averageContentsSize)
  {
   struct ephemeron *edPtr, *lastPtr = NULL, *nextPtr;

   edPtr = *theEphemeralList;

   while (edPtr != NULL)
     {
      /*======================================================*/
      /* Check through previous and current evaluation depth  */
      /* because these symbols can be interspersed, otherwise */
      /* symbols are stored in descending evaluation depth.   */
      /*======================================================*/

      nextPtr = edPtr->next;

      /*==================================================*/
      /* Remove any symbols that have a count of zero and */
      /* were added to the ephemeral list at a higher     */
      /* evaluation depth.                                */
      /*==================================================*/

      if ((edPtr->associatedValue->count == 0) &&
          (edPtr->associatedValue->depth > CurrentEvaluationDepth))
        {
         RemoveHashNode(edPtr->associatedValue,theTable,hashNodeSize,hashNodeType);
         rtn_struct(ephemeron,edPtr);
         if (lastPtr == NULL) *theEphemeralList = nextPtr;
         else lastPtr->next = nextPtr;
         EphemeralItemCount--;
         EphemeralItemSize -= sizeof(struct ephemeron) + hashNodeSize +
                              averageContentsSize;
        }

      /*=======================================*/
      /* Remove ephemeral status of any symbol */
      /* with a count greater than zero.       */
      /*=======================================*/

      else if (edPtr->associatedValue->count > 0)
        {
         edPtr->associatedValue->markedEphemeral = FALSE;
         rtn_struct(ephemeron,edPtr);
         if (lastPtr == NULL) *theEphemeralList = nextPtr;
         else lastPtr->next = nextPtr;
         EphemeralItemCount--;
         EphemeralItemSize -= sizeof(struct ephemeron) + hashNodeSize +
                              averageContentsSize;
        }

      /*==================================================*/
      /* Otherwise keep the symbol in the ephemeral list. */
      /*==================================================*/

      else
        { lastPtr = edPtr; }

      edPtr = nextPtr;
     }
  }

/*********************************************************/
/* GetSymbolTable: Returns a pointer to the SymbolTable. */
/*********************************************************/
globle SYMBOL_HN **GetSymbolTable()
  {
   return(SymbolTable);
  }

/******************************************************/
/* SetSymbolTable: Sets the value of the SymbolTable. */
/******************************************************/
globle void SetSymbolTable(
  SYMBOL_HN **value)
  {
   SymbolTable = value;
  }

/*******************************************************/
/* GetFloatTable: Returns a pointer to the FloatTable. */
/*******************************************************/
globle FLOAT_HN **GetFloatTable()
  {
   return(FloatTable);
  }

/****************************************************/
/* SetFloatTable: Sets the value of the FloatTable. */
/****************************************************/
globle void SetFloatTable(
  FLOAT_HN **value)
  {
   FloatTable = value;
  }

/***********************************************************/
/* GetIntegerTable: Returns a pointer to the IntegerTable. */
/***********************************************************/
globle INTEGER_HN **GetIntegerTable()
  {
   return(IntegerTable);
  }

/********************************************************/
/* SetIntegerTable: Sets the value of the IntegerTable. */
/********************************************************/
globle void SetIntegerTable(
  INTEGER_HN **value)
  {
   IntegerTable = value;
  }

/*********************************************************/
/* GetBitMapTable: Returns a pointer to the BitMapTable. */
/*********************************************************/
globle BITMAP_HN **GetBitMapTable()
  {
   return(BitMapTable);
  }

/******************************************************/
/* SetBitMapTable: Sets the value of the BitMapTable. */
/******************************************************/
globle void SetBitMapTable(
  BITMAP_HN **value)
  {
   BitMapTable = value;
  }

/******************************************************/
/* RefreshSpecialSymbols: Resets the values of the    */
/*   TrueSymbol, FalseSymbol, Zero, PositiveInfinity, */
/*   and NegativeInfinity symbols.                    */
/******************************************************/
globle void RefreshSpecialSymbols()
  {
   TrueSymbol = (void *) FindSymbol(TRUE_STRING);
   FalseSymbol = (void *) FindSymbol(FALSE_STRING);
   PositiveInfinity = (void *) FindSymbol(POSITIVE_INFINITY_STRING);
   NegativeInfinity = (void *) FindSymbol(NEGATIVE_INFINITY_STRING);
   Zero = (void *) FindLong(0L);
  }

/***********************************************************/
/* FindSymbolMatches: Finds all symbols in the SymbolTable */
/*   which begin with a specified symbol. This function is */
/*   used to implement the command completion feature      */
/*   found in some of the machine specific interfaces.     */
/***********************************************************/
globle struct symbolMatch *FindSymbolMatches(
  char *searchString,
  int *numberOfMatches,
  int *commonPrefixLength)
  {
   struct symbolMatch *reply = NULL, *temp;
   struct symbolHashNode *hashPtr = NULL;
   int searchLength;

   searchLength = strlen(searchString);
   *numberOfMatches = 0;

   while ((hashPtr = GetNextSymbolMatch(searchString,searchLength,hashPtr,
                                        FALSE,commonPrefixLength)) != NULL)
     {
      *numberOfMatches = *numberOfMatches + 1;
      temp = get_struct(symbolMatch);
      temp->match = hashPtr;
      temp->next = reply;
      reply = temp;
     }

   return(reply);
  }

/*********************************************************/
/* ReturnSymbolMatches: Returns a set of symbol matches. */
/*********************************************************/
globle void ReturnSymbolMatches(
  struct symbolMatch *listOfMatches)
  {
   struct symbolMatch *temp;

   while (listOfMatches != NULL)
     {
      temp = listOfMatches->next;
      rtn_struct(symbolMatch,listOfMatches);
      listOfMatches = temp;
     }
  }

/***************************************************************/
/* ClearBitString: Initializes the values of a bitmap to zero. */
/***************************************************************/
globle void ClearBitString(
  void *vTheBitMap,
  int length)
  {
   char *theBitMap = (char *) vTheBitMap;
   int i;

   for (i = 0; i < length; i++) theBitMap[i] = '\0';
  }

/*****************************************************************/
/* GetNextSymbolMatch: Finds the next symbol in the SymbolTable  */
/*   which begins with a specified symbol. This function is used */
/*   to implement the command completion feature found in some   */
/*   of the machine specific interfaces.                         */
/*****************************************************************/
globle SYMBOL_HN *GetNextSymbolMatch(
  char *searchString,
  int searchLength,
  SYMBOL_HN *prevSymbol,
  int anywhere,
  int *commonPrefixLength)
  {
   register int i;
   SYMBOL_HN *hashPtr;
   int flag = TRUE;
   int prefixLength;

   /*==========================================*/
   /* If we're looking anywhere in the string, */
   /* then there's no common prefix length.    */
   /*==========================================*/

   if (anywhere && (commonPrefixLength != NULL))
     *commonPrefixLength = 0;

   /*========================================================*/
   /* If we're starting the search from the beginning of the */
   /* symbol table, the previous symbol argument is NULL.    */
   /*========================================================*/

   if (prevSymbol == NULL)
     {
      i = 0;
      hashPtr = SymbolTable[0];
     }

   /*==========================================*/
   /* Otherwise start the search at the symbol */
   /* after the last symbol found.             */
   /*==========================================*/

   else
     {
      i = prevSymbol->bucket;
      hashPtr = prevSymbol->next;
     }

   /*==============================================*/
   /* Search through all the symbol table buckets. */
   /*==============================================*/

   while (flag)
     {
      /*===================================*/
      /* Search through all of the entries */
      /* in the bucket being examined.     */
      /*===================================*/

      for (; hashPtr != NULL; hashPtr = hashPtr->next)
        {
         /*================================================*/
         /* Skip symbols that being with ( since these are */
         /* typically symbols for internal use. Also skip  */
         /* any symbols that are marked ephemeral since    */
         /* these aren't in use.                           */
         /*================================================*/

         if ((hashPtr->contents[0] == '(') ||
             (hashPtr->markedEphemeral))
           { continue; }

         /*==================================================*/
         /* Two types of matching can be performed: the type */
         /* comparing just to the beginning of the string    */
         /* and the type which looks for the substring       */
         /* anywhere within the string being examined.       */
         /*==================================================*/

         if (! anywhere)
           {
            /*=============================================*/
            /* Determine the common prefix length between  */
            /* the previously found match (if available or */
            /* the search string if not) and the symbol    */
            /* table entry.                                */
            /*=============================================*/

            if (prevSymbol != NULL)
              prefixLength = CommonPrefixLength(prevSymbol->contents,hashPtr->contents);
            else
              prefixLength = CommonPrefixLength(searchString,hashPtr->contents);

            /*===================================================*/
            /* If the prefix length is greater than or equal to  */
            /* the length of the search string, then we've found */
            /* a match. If this is the first match, the common   */
            /* prefix length is set to the length of the first   */
            /* match, otherwise the common prefix length is the  */
            /* smallest prefix length found among all matches.   */
            /*===================================================*/

            if (prefixLength >= searchLength)
              {
               if (commonPrefixLength != NULL)
                 {
                  if (prevSymbol == NULL)
                    *commonPrefixLength = strlen(hashPtr->contents);
                  else if (prefixLength < *commonPrefixLength)
                    *commonPrefixLength = prefixLength;
                 }
               return(hashPtr);
              }
           }
         else
           {
            if (StringWithinString(hashPtr->contents,searchString) != NULL)
              { return(hashPtr); }
           }
        }

      /*=================================================*/
      /* Move on to the next bucket in the symbol table. */
      /*=================================================*/

      if (++i >= SYMBOL_HASH_SIZE) flag = FALSE;
      else hashPtr = SymbolTable[i];
     }

   /*=====================================*/
   /* There are no more matching symbols. */
   /*=====================================*/

   return(NULL);
  }

/**********************************************/
/* StringWithinString: Determines if a string */
/*   is contained within another string.      */
/**********************************************/
static char *StringWithinString(
  char *cs,
  char *ct)
  {
   register unsigned i,j,k;

   for (i = 0 ; cs[i] != '\0' ; i++)
     {
      for (j = i , k = 0 ; ct[k] != '\0' && cs[j] == ct[k] ; j++, k++) ;
      if ((ct[k] == '\0') && (k != 0))
        return(cs + i);
     }
   return(NULL);
  }

/************************************************/
/* CommonPrefixLength: Determines the length of */
/*    the maximumcommon prefix of two strings   */
/************************************************/
static int CommonPrefixLength(
  char *cs,
  char *ct)
  {
   register int i;

   for (i = 0 ; (cs[i] != '\0') && (ct[i] != '\0') ; i++)
     if (cs[i] != ct[i])
       break;
   return(i);
  }

#if BLOAD_AND_BSAVE || CONSTRUCT_COMPILER || BSAVE_INSTANCES

/****************************************************************/
/* SetAtomicValueIndices: Sets the bucket values for hash table */
/*   entries with an index value that indicates the position of */
/*   the hash table in a hash table traversal (e.g. this is the */
/*   fifth entry in the  hash table.                            */
/****************************************************************/
globle void SetAtomicValueIndices(
  int setAll)
  {
   unsigned int count;
   int i;
   SYMBOL_HN *symbolPtr, **symbolArray;
   FLOAT_HN *floatPtr, **floatArray;
   INTEGER_HN *integerPtr, **integerArray;
   BITMAP_HN *bitMapPtr, **bitMapArray;

   /*===================================*/
   /* Set indices for the symbol table. */
   /*===================================*/

   count = 0;
   symbolArray = GetSymbolTable();

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i];
           symbolPtr != NULL;
           symbolPtr = symbolPtr->next)
        {
         if ((symbolPtr->neededSymbol == TRUE) || setAll)
           {
            symbolPtr->bucket = count++;
            if (symbolPtr->bucket != (count - 1))
              { SystemError("SYMBOL",667); }
           }
        }
     }

   /*==================================*/
   /* Set indices for the float table. */
   /*==================================*/

   count = 0;
   floatArray = GetFloatTable();

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i];
           floatPtr != NULL;
           floatPtr = floatPtr->next)
        {
         if ((floatPtr->neededFloat == TRUE) || setAll)
           {
            floatPtr->bucket = count++;
            if (floatPtr->bucket != (count - 1))
              { SystemError("SYMBOL",668); }
           }
        }
     }

   /*====================================*/
   /* Set indices for the integer table. */
   /*====================================*/

   count = 0;
   integerArray = GetIntegerTable();

   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i];
           integerPtr != NULL;
           integerPtr = integerPtr->next)
        {
         if ((integerPtr->neededInteger == TRUE) || setAll)
           {
            integerPtr->bucket = count++;
            if (integerPtr->bucket != (count - 1))
              { SystemError("SYMBOL",669); }
           }
        }
     }

   /*===================================*/
   /* Set indices for the bitmap table. */
   /*===================================*/

   count = 0;
   bitMapArray = GetBitMapTable();

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i];
           bitMapPtr != NULL;
           bitMapPtr = bitMapPtr->next)
        {
         if ((bitMapPtr->neededBitMap == TRUE) || setAll)
           {
            bitMapPtr->bucket = count++;
            if (bitMapPtr->bucket != (count - 1))
              { SystemError("SYMBOL",670); }
           }
        }
     }
  }

/***********************************************************************/
/* RestoreAtomicValueBuckets: Restores the bucket values of hash table */
/*   entries to the appropriate values. Normally called to undo the    */
/*   effects of a call to the SetAtomicValueIndices function.          */
/***********************************************************************/
globle void RestoreAtomicValueBuckets()
  {
   int i;
   SYMBOL_HN *symbolPtr, **symbolArray;
   FLOAT_HN *floatPtr, **floatArray;
   INTEGER_HN *integerPtr, **integerArray;
   BITMAP_HN *bitMapPtr, **bitMapArray;

   /*================================================*/
   /* Restore the bucket values in the symbol table. */
   /*================================================*/

   symbolArray = GetSymbolTable();

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i];
           symbolPtr != NULL;
           symbolPtr = symbolPtr->next)
        { symbolPtr->bucket = i; }
     }

   /*===============================================*/
   /* Restore the bucket values in the float table. */
   /*===============================================*/

   floatArray = GetFloatTable();

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i];
           floatPtr != NULL;
           floatPtr = floatPtr->next)
        { floatPtr->bucket = i; }
     }

   /*=================================================*/
   /* Restore the bucket values in the integer table. */
   /*=================================================*/

   integerArray = GetIntegerTable();

   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i];
           integerPtr != NULL;
           integerPtr = integerPtr->next)
        { integerPtr->bucket = i; }
     }

   /*================================================*/
   /* Restore the bucket values in the bitmap table. */
   /*================================================*/

   bitMapArray = GetBitMapTable();

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i];
           bitMapPtr != NULL;
           bitMapPtr = bitMapPtr->next)
        { bitMapPtr->bucket = i; }
     }
  }

#endif /* BLOAD_AND_BSAVE || CONSTRUCT_COMPILER || BSAVE_INSTANCES */
