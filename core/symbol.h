   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 SYMBOL HEADER FILE                  */
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
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_symbol
#define _H_symbol

struct symbolHashNode;
struct floatHashNode;
struct integerHashNode;
struct bitMapHashNode;
struct genericHashNode;
struct symbolMatch;

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SYMBOL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#ifndef SYMBOL_HASH_SIZE
#define SYMBOL_HASH_SIZE        1013
#endif

#ifndef FLOAT_HASH_SIZE
#define FLOAT_HASH_SIZE          503
#endif

#ifndef INTEGER_HASH_SIZE
#define INTEGER_HASH_SIZE        167
#endif

#ifndef BITMAP_HASH_SIZE
#define BITMAP_HASH_SIZE         167
#endif

#define CLIPSTrueSymbol TrueSymbol
#define CLIPSFalseSymbol FalseSymbol

/************************************************************/
/* symbolHashNode STRUCTURE:                                */
/************************************************************/
struct symbolHashNode
  {
   struct symbolHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededSymbol : 1;
   unsigned int bucket : 30;
   char *contents;
  };

/************************************************************/
/* floatHashNode STRUCTURE:                                  */
/************************************************************/
struct floatHashNode
  {
   struct floatHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededFloat : 1;
   unsigned int bucket : 30;
   double contents;
  };

/************************************************************/
/* integerHashNode STRUCTURE:                               */
/************************************************************/
struct integerHashNode
  {
   struct integerHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededInteger : 1;
   unsigned int bucket : 30;
   long int contents;
  };

/************************************************************/
/* bitMapHashNode STRUCTURE:                                */
/************************************************************/
struct bitMapHashNode
  {
   struct bitMapHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededBitMap : 1;
   unsigned int bucket : 30;
   char *contents;
   unsigned short size;
  };

/************************************************************/
/* genericHashNode STRUCTURE:                               */
/************************************************************/
struct genericHashNode
  {
   struct genericHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int needed : 1;
   unsigned int bucket : 30;
  };

/************************************************************/
/* symbolMatch STRUCTURE:                               */
/************************************************************/
struct symbolMatch
  {
   struct symbolHashNode *match;
   struct symbolMatch *next;
  };

typedef struct symbolHashNode SYMBOL_HN;
typedef struct floatHashNode FLOAT_HN;
typedef struct integerHashNode INTEGER_HN;
typedef struct bitMapHashNode BITMAP_HN;
typedef struct genericHashNode GENERIC_HN;

#define ValueToString(target) (((struct symbolHashNode *) (target))->contents)
#define ValueToDouble(target) (((struct floatHashNode *) (target))->contents)
#define ValueToLong(target) (((struct integerHashNode *) (target))->contents)
#define ValueToInteger(target) ((int) (((struct integerHashNode *) (target))->contents))
#define ValueToBitMap(target) ((void *) ((struct bitMapHashNode *) (target))->contents)

#define IncrementSymbolCount(theValue) (((SYMBOL_HN *) theValue)->count++)
#define IncrementFloatCount(theValue) (((FLOAT_HN *) theValue)->count++)
#define IncrementIntegerCount(theValue) (((INTEGER_HN *) theValue)->count++)
#define IncrementBitMapCount(theValue) (((BITMAP_HN *) theValue)->count++)

/*****************************************************/
/* The FindSymbol function is remapped under certain */
/* conditions because it conflicts with a Metroworks */
/* Code Warrior library function.                    */
/*****************************************************/
#if MAC_MCW
#define FindSymbol MCWFindSymbol
#endif

   LOCALE DllExport void                *AddSymbol(char *);
   LOCALE SYMBOL_HN                     *FindSymbol(char *);
   LOCALE DllExport void                *AddDouble(double);
   LOCALE DllExport void                *AddLong(long int);
   LOCALE void                          *AddBitMap(void *,int);
   LOCALE INTEGER_HN                    *FindLong(long int);
   LOCALE void                           InitializeAtomTables(void);
   LOCALE int                            HashSymbol(char *,int);
   LOCALE int                            HashFloat(double,int);
   LOCALE int                            HashInteger(long int,int);
   LOCALE int                            HashBitMap(char *,int,int);
   LOCALE void                           DecrementSymbolCount(struct symbolHashNode *);
   LOCALE void                           DecrementFloatCount(struct floatHashNode *);
   LOCALE void                           DecrementIntegerCount(struct integerHashNode *);
   LOCALE void                           DecrementBitMapCount(struct bitMapHashNode *);
   LOCALE void                           RemoveEphemeralAtoms(void);
   LOCALE struct symbolHashNode        **GetSymbolTable(void);
   LOCALE void                           SetSymbolTable(struct symbolHashNode **);
   LOCALE struct floatHashNode          **GetFloatTable(void);
   LOCALE void                           SetFloatTable(struct floatHashNode **);
   LOCALE struct integerHashNode       **GetIntegerTable(void);
   LOCALE void                           SetIntegerTable(struct integerHashNode **);
   LOCALE struct bitMapHashNode        **GetBitMapTable(void);
   LOCALE void                           SetBitMapTable(struct bitMapHashNode **);
   LOCALE void                           RefreshSpecialSymbols(void);
   LOCALE struct symbolMatch            *FindSymbolMatches(char *,int *,int *);
   LOCALE void                           ReturnSymbolMatches(struct symbolMatch *);
   LOCALE SYMBOL_HN                     *GetNextSymbolMatch(char *,int,SYMBOL_HN *,int,int *);
   LOCALE void                           ClearBitString(void *,int);
   LOCALE void                           SetAtomicValueIndices(int);
   LOCALE void                           RestoreAtomicValueBuckets(void);


#ifndef _SYMBOL_SOURCE
   extern Thread void                   *TrueSymbol;
   extern Thread void                   *FalseSymbol;
   extern Thread void                   *NegativeInfinity;
   extern Thread void                   *PositiveInfinity;
   extern Thread void                   *Zero;
#endif

#endif
