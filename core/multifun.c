   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  11/15/99            */
   /*                                                     */
   /*             MULTIFIELD FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several multifield         */
/*   functions including first$, rest$, subseq$, delete$,    */
/*   delete-member$, replace-member$                         */
/*   replace$, insert$, explode$, implode$, nth$, member$,   */
/*   subsetp, progn$, str-implode, str-explode, subset, nth, */
/*   mv-replace, member, mv-subseq, and mv-delete.           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian Donnell                                        */
/*      Barry Cameron                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */ 
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/*      MDT          | 15-Nov-1999 |        DR835            */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _MULTIFUN_SOURCE_

#include "setup.h"

#if MULTIFIELD_FUNCTIONS || OBJECT_SYSTEM

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "memalloc.h"
#include "argacces.h"
#include "multifld.h"
#include "router.h"
#include "multifun.h"
#include "exprnpsr.h"
#include "prcdrpsr.h"
#include "prcdrfun.h"
#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "scanner.h"
#endif
#include "utility.h"

#if OBJECT_SYSTEM
#include "object.h"
#endif

/**************/
/* STRUCTURES */
/**************/

typedef struct fieldVarStack
  {
   int type;
   void *value;
   long index;
   struct fieldVarStack *nxt;
  } FIELD_VAR_STACK;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if MULTIFIELD_FUNCTIONS
   static BOOLEAN                 FindDOsInSegment(DATA_OBJECT_PTR,int,DATA_OBJECT_PTR,
                                                   long *,long *,long *,int);
   static BOOLEAN                 MVRangeCheck(long,long,long *,int);
#if (! BLOAD_ONLY) && (! RUN_TIME)
   static struct expr            *MultifieldPrognParser(struct expr *,char *);
   static void                    ReplaceMvPrognFieldVars(SYMBOL_HN *,struct expr *,int);
#endif
#endif
   static void                    MVRangeError(long,long,long,char *);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if MULTIFIELD_FUNCTIONS

   Thread static FIELD_VAR_STACK    *FieldVarStack = NULL;

#if ! RUN_TIME
/**********************************************/
/* MultifieldFunctionDefinitions: Initializes */
/*   the multifield functions.                */
/**********************************************/
globle void MultifieldFunctionDefinitions()
  {
   DefineFunction2("first$", 'm', PTIF FirstFunction, "FirstFunction", "11m");
   DefineFunction2("rest$", 'm', PTIF RestFunction, "RestFunction", "11m");
   DefineFunction2("subseq$", 'm', PTIF SubseqFunction, "SubseqFunction", "33im");
   DefineFunction2("delete-member$", 'm', PTIF DeleteMemberFunction, "DeleteMemberFunction", "2*um");
   DefineFunction2("replace-member$", 'm', PTIF ReplaceMemberFunction, "ReplaceMemberFunction","3*um");
   DefineFunction2("delete$", 'm', PTIF DeleteFunction, "DeleteFunction", "33im");
   DefineFunction2("replace$", 'm', PTIF ReplaceFunction, "ReplaceFunction","4**mii");
   DefineFunction2("insert$", 'm', PTIF InsertFunction, "InsertFunction", "3**mi");
   DefineFunction2("explode$", 'm', PTIF ExplodeFunction, "ExplodeFunction", "11s");
   DefineFunction2("implode$", 's', PTIF ImplodeFunction, "ImplodeFunction", "11m");
   DefineFunction2("nth$", 'u', PTIF NthFunction, "NthFunction", "22*im");
   DefineFunction2("member$", 'u', PTIF MemberFunction, "MemberFunction", "22*um");
   DefineFunction2("subsetp", 'b', PTIF SubsetpFunction, "SubsetpFunction", "22*mm");
   DefineFunction2("progn$", 'u', PTIF MultifieldPrognFunction, "MultifieldPrognFunction", NULL);
   DefineFunction2("str-implode", 's', PTIF ImplodeFunction, "ImplodeFunction", "11m");
   DefineFunction2("str-explode", 'm', PTIF ExplodeFunction, "ExplodeFunction", "11s");
   DefineFunction2("subset", 'b', PTIF SubsetpFunction, "SubsetpFunction", "22*mm");
   DefineFunction2("nth", 'u', PTIF NthFunction, "NthFunction", "22*im");
   DefineFunction2("mv-replace", 'm', PTIF MVReplaceFunction, "MVReplaceFunction","33*im");
   DefineFunction2("member", 'u', PTIF MemberFunction, "MemberFunction", "22*um");
   DefineFunction2("mv-subseq", 'm', PTIF MVSubseqFunction, "MVSubseqFunction", "33*iim");
   DefineFunction2("mv-delete", 'm', PTIF MVDeleteFunction,"MVDeleteFunction", "22*im");
#if ! BLOAD_ONLY
   AddFunctionParser("progn$",MultifieldPrognParser);
#endif
   FuncSeqOvlFlags("progn$",FALSE,FALSE);
   DefineFunction2("(get-progn$-field)", 'u', PTIF GetMvPrognField, "GetMvPrognField", "00");
   DefineFunction2("(get-progn$-index)", 'l', PTIF GetMvPrognIndex, "GetMvPrognIndex", "00");
  }
#endif

/****************************************/
/* DeleteFunction: H/L access routine   */
/*   for the delete$ function.          */
/****************************************/
globle void DeleteFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT value1, value2, value3;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((ArgTypeCheck("delete$",1,MULTIFIELD,&value1) == FALSE) ||
       (ArgTypeCheck("delete$",2,INTEGER,&value2) == FALSE) ||
       (ArgTypeCheck("delete$",3,INTEGER,&value3) == FALSE))
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=================================================*/
   /* Delete the section out of the multifield value. */
   /*=================================================*/

   if (DeleteMultiValueField(returnValue,&value1,
            DOToLong(value2),DOToLong(value3),"delete$") == FALSE)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }

/******************************************/
/* MVDeleteFunction: H/L access routine   */
/*   for the mv-delete function.          */
/******************************************/
globle void MVDeleteFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT value1, value2;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((ArgTypeCheck("mv-delete",1,INTEGER,&value1) == FALSE) ||
       (ArgTypeCheck("mv-delete",2,MULTIFIELD,&value2) == FALSE))
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=================================================*/
   /* Delete the section out of the multifield value. */
   /*=================================================*/

   if (DeleteMultiValueField(returnValue,&value2,
            DOToLong(value1),DOToLong(value1),"mv-delete") == FALSE)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }

/*****************************************/
/* ReplaceFunction: H/L access routine   */
/*   for the replace$ function.          */
/*****************************************/
globle void ReplaceFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT value1, value2, value3, value4;
   EXPRESSION *fieldarg;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((ArgTypeCheck("replace$",1,MULTIFIELD,&value1) == FALSE) ||
       (ArgTypeCheck("replace$",2,INTEGER,&value2) == FALSE) ||
       (ArgTypeCheck("replace$",3,INTEGER,&value3) == FALSE))
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*===============================*/
   /* Create the replacement value. */
   /*===============================*/

   fieldarg = GetFirstArgument()->nextArg->nextArg->nextArg;
   if (fieldarg->nextArg != NULL)
     { StoreInMultifield(&value4,fieldarg,TRUE); }
   else
     { EvaluateExpression(fieldarg,&value4); }

   /*==============================================*/
   /* Replace the section in the multifield value. */
   /*==============================================*/

   if (ReplaceMultiValueField(returnValue,&value1,DOToInteger(value2),
                   DOToInteger(value3),&value4,"replace$") == FALSE)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }

/*******************************************/
/* MVReplaceFunction: H/L access routine   */
/*   for the mv-replace function.          */
/*******************************************/
globle void MVReplaceFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT value1, value2, value3;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((ArgTypeCheck("mv-replace",1,INTEGER,&value1) == FALSE) ||
       (ArgTypeCheck("mv-replace",2,MULTIFIELD,&value2) == FALSE))
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*===============================*/
   /* Create the replacement value. */
   /*===============================*/

   EvaluateExpression(GetFirstArgument()->nextArg->nextArg,&value3);

   /*==============================================*/
   /* Replace the section in the multifield value. */
   /*==============================================*/

   if (ReplaceMultiValueField(returnValue,&value2,DOToInteger(value1),
                   DOToInteger(value1),&value3,"mv-replace") == FALSE)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }

/**********************************************/
/* DeleteMemberFunction: H/L access routine   */
/*   for the delete-member$ function.         */
/**********************************************/
globle void DeleteMemberFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT resultValue,*delVals,tmpVal;
   int i,argCnt,delSize;
   long j,k;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   argCnt = ArgCountCheck("delete-member$",AT_LEAST,2);
   if (argCnt == -1)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/
   if (ArgTypeCheck("delete-member$",1,MULTIFIELD,&resultValue) == FALSE)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=================================================
     For every value specified, delete all occurrences
     of those values from the multifield
     ================================================= */
   delSize = (int) (sizeof(DATA_OBJECT) * (argCnt-1));
   delVals = (DATA_OBJECT_PTR) gm2(delSize);
   for (i = 2 ; i <= argCnt ; i++)
     {
      if (!RtnUnknown(i,&delVals[i-2]))
        {
         rm((void *) delVals,delSize);
         SetEvaluationError(TRUE);
         SetMultifieldErrorValue(returnValue);
         return;
        }
     }

   while (FindDOsInSegment(delVals,argCnt-1,&resultValue,&j,&k,NULL,0))
     {
      if (DeleteMultiValueField(&tmpVal,&resultValue,
                                j,k,"delete-member$") == FALSE)
        {
         rm((void *) delVals,delSize);
         SetEvaluationError(TRUE);
         SetMultifieldErrorValue(returnValue);
         return;
        }
      GenCopyMemory(DATA_OBJECT,1,&resultValue,&tmpVal);
     }
   rm((void *) delVals,delSize);
   GenCopyMemory(DATA_OBJECT,1,returnValue,&resultValue);
  }

/***********************************************/
/* ReplaceMemberFunction: H/L access routine   */
/*   for the replace-member$ function.         */
/***********************************************/
globle void ReplaceMemberFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT resultValue,replVal,*delVals,tmpVal;
   int i,argCnt,delSize;
   long j,k,mink[2],*minkp;
   long replLen = 1L;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   argCnt = ArgCountCheck("replace-member$",AT_LEAST,3);
   if (argCnt == -1)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/
   if (ArgTypeCheck("replace-member$",1,MULTIFIELD,&resultValue) == FALSE)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   if (!RtnUnknown(2,&replVal))
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }
   if (GetType(replVal) == MULTIFIELD)
     replLen = GetDOLength(replVal);

   /*=====================================================
     For the value (or values from multifield ) specified,
     replace all occurrences of those values with all
     values specified
     ===================================================== */
   delSize = (int) (sizeof(DATA_OBJECT) * (argCnt-2));
   delVals = (DATA_OBJECT_PTR) gm2(delSize);
   for (i = 3 ; i <= argCnt ; i++)
     {
      if (!RtnUnknown(i,&delVals[i-3]))
        {
         rm((void *) delVals,delSize);
         SetEvaluationError(TRUE);
         SetMultifieldErrorValue(returnValue);
         return;
        }
     }
   minkp = NULL;
   while (FindDOsInSegment(delVals,argCnt-2,&resultValue,&j,&k,minkp,minkp ? 1 : 0))
     {
      if (ReplaceMultiValueField(&tmpVal,&resultValue,j,k,
                                 &replVal,"replace-member$") == FALSE)
        {
         rm((void *) delVals,delSize);
         SetEvaluationError(TRUE);
         SetMultifieldErrorValue(returnValue);
         return;
        }
      GenCopyMemory(DATA_OBJECT,1,&resultValue,&tmpVal);
      mink[0] = 1L;
      mink[1] = j + replLen - 1L;
      minkp = mink;
     }
   rm((void *) delVals,delSize);
   GenCopyMemory(DATA_OBJECT,1,returnValue,&resultValue);
  }

/****************************************/
/* InsertFunction: H/L access routine   */
/*   for the insert$ function.          */
/****************************************/
globle void InsertFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT value1, value2, value3;
   EXPRESSION *fieldarg;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((ArgTypeCheck("insert$",1,MULTIFIELD,&value1) == FALSE) ||
       (ArgTypeCheck("insert$",2,INTEGER,&value2) == FALSE))
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=============================*/
   /* Create the insertion value. */
   /*=============================*/

   fieldarg = GetFirstArgument()->nextArg->nextArg;
   if (fieldarg->nextArg != NULL)
     StoreInMultifield(&value3,fieldarg,TRUE);
   else
     EvaluateExpression(fieldarg,&value3);

   /*===========================================*/
   /* Insert the value in the multifield value. */
   /*===========================================*/

   if (InsertMultiValueField(returnValue,&value1,DOToLong(value2),
                             &value3,"insert$") == FALSE)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }

/*****************************************/
/* ExplodeFunction: H/L access routine   */
/*   for the explode$ function.          */
/*****************************************/
globle void ExplodeFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT value;
   struct multifield *theMultifield;
   long end; /* 6.04 Bug Fix */

   /*=====================================*/
   /* Explode$ expects a single argument. */
   /*=====================================*/

   if (ArgCountCheck("explode$",EXACTLY,1) == -1)
     {
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*==================================*/
   /* The argument should be a string. */
   /*==================================*/

   if (ArgTypeCheck("explode$",1,STRING,&value) == FALSE)
     {
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=====================================*/
   /* Convert the string to a multifield. */
   /*=====================================*/

   theMultifield = StringToMultifield(DOToString(value));
   if (theMultifield == NULL)
     {
      theMultifield = (struct multifield *) CreateMultifield(0L);
      end = 0;
     }
   else
     { end = GetMFLength(theMultifield); }

   /*========================*/
   /* Return the multifield. */
   /*========================*/

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,end);
   SetpValue(returnValue,(void *) theMultifield);
   return;
  }

/*****************************************/
/* ImplodeFunction: H/L access routine   */
/*   for the implode$ function.          */
/*****************************************/
globle void *ImplodeFunction()
  {
   DATA_OBJECT value;
   long strsize = 0;
   long i, j;
   char *tmp_str;
   char *ret_str;
   struct multifield *theMultifield;
   void *rv;

   /*=====================================*/
   /* Implode$ expects a single argument. */
   /*=====================================*/

   if (ArgCountCheck("implode$",EXACTLY,1) == -1)
     { return(AddSymbol("")); }

   /*======================================*/
   /* The argument should be a multifield. */
   /*======================================*/

   if (ArgTypeCheck("implode$",1,MULTIFIELD,&value) == FALSE)
     { return(AddSymbol("")); }

   /*===================================================*/
   /* Determine the size of the string to be allocated. */
   /*===================================================*/

   theMultifield = (struct multifield *) GetValue(value);
   for (i = GetDOBegin(value) ; i <= GetDOEnd(value) ; i++)
     {
      if (GetMFType(theMultifield,i) == FLOAT)
        {
         tmp_str = FloatToString(ValueToDouble(GetMFValue(theMultifield,i)));
         strsize += strlen(tmp_str) + 1;
        }
      else if (GetMFType(theMultifield,i) == INTEGER)
        {
         tmp_str = LongIntegerToString(ValueToLong(GetMFValue(theMultifield,i)));
         strsize += strlen(tmp_str) + 1;
        }
      else if (GetMFType(theMultifield,i) == STRING)
        {
         strsize += strlen(ValueToString(GetMFValue(theMultifield,i))) + 3;
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         while(*tmp_str)
           {
             if (*tmp_str == '"')
               { strsize++; }
             else if (*tmp_str == '\\') 
              { strsize++; }          
            tmp_str++;
           }
        }
#if OBJECT_SYSTEM
      else if (GetMFType(theMultifield,i) == INSTANCE_NAME)
        { strsize += strlen(ValueToString(GetMFValue(theMultifield,i))) + 3; }
      else if (GetMFType(theMultifield,i) == INSTANCE_ADDRESS)
        { strsize += strlen(ValueToString(((INSTANCE_TYPE *)
                            GetMFValue(theMultifield,i))->name)) + 3; }
#endif

      else
        { strsize += strlen(ValueToString(GetMFValue(theMultifield,i))) + 1; }
     }

   /*=============================================*/
   /* Allocate the string and copy all components */
   /* of the MULTIFIELD variable to it.             */
   /*=============================================*/

   if (strsize == 0) return(AddSymbol(""));
   ret_str = (char *) gm2(strsize);
   for(j=0, i=GetDOBegin(value); i <= GetDOEnd(value) ; i++)
     {

      /*============================*/
      /* Convert numbers to strings */
      /*============================*/

      if (GetMFType(theMultifield,i) == FLOAT)
        {
         tmp_str = FloatToString(ValueToDouble(GetMFValue(theMultifield,i)));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
        }
      else if (GetMFType(theMultifield,i) == INTEGER)
        {
         tmp_str = LongIntegerToString(ValueToLong(GetMFValue(theMultifield,i)));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
        }

      /*=======================================*/
      /* Enclose strings in quotes and preceed */
      /* imbedded quotes with a backslash      */
      /*=======================================*/

      else if (GetMFType(theMultifield,i) == STRING)
        {
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         *(ret_str+j) = '"';
         j++;
         while(*tmp_str)
           {
        
            if (*tmp_str == '"')
              {
               *(ret_str+j) = '\\';
               j++;
              }
            else if (*tmp_str == '\\') 
              {                       
               *(ret_str+j) = '\\';    
               j++;                    
              }                        
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str+j) = '"';
         j++;
        }
#if OBJECT_SYSTEM
      else if (GetMFType(theMultifield,i) == INSTANCE_NAME)
        {
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         *(ret_str + j++) = '[';
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str + j++) = ']';
        }
      else if (GetMFType(theMultifield,i) == INSTANCE_ADDRESS)
        {
         tmp_str = ValueToString(((INSTANCE_TYPE *) GetMFValue(theMultifield,i))->name);
         *(ret_str + j++) = '[';
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str + j++) = ']';
        }
#endif
      else
        {
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         }
      *(ret_str+j) = ' ';
      j++;
     }
   *(ret_str+j-1) = '\0';

   /*====================*/
   /* Return the string. */
   /*====================*/

   rv = AddSymbol(ret_str);
   rm(ret_str,strsize);
   return(rv);
  }

/****************************************/
/* SubseqFunction: H/L access routine   */
/*   for the subseq$ function.          */
/****************************************/
globle void SubseqFunction(
  DATA_OBJECT_PTR sub_value)
  {
   DATA_OBJECT value;
   struct multifield *theList;
   long offset, start, end, length; /* 6.04 Bug Fix */

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/

   if (ArgTypeCheck("subseq$",1,MULTIFIELD,&value) == FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   theList = (struct multifield *) DOToPointer(value);
   offset = GetDOBegin(value);
   length = GetDOLength(value);

   /*=============================================*/
   /* Get range arguments. If they are not within */
   /* appropriate ranges, return a null segment.  */
   /*=============================================*/

   if (ArgTypeCheck("subseq$",2,INTEGER,&value) == FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   start = DOToInteger(value);

   if (ArgTypeCheck("subseq$",3,INTEGER,&value) == FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   end = DOToInteger(value);

   if ((end < 1) || (end < start))
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }

   /*===================================================*/
   /* Adjust lengths  to conform to segment boundaries. */
   /*===================================================*/

   if (start > length)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   if (end > length) end = length;
   if (start < 1) start = 1;

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(sub_value,MULTIFIELD);
   SetpValue(sub_value,theList);
   SetpDOEnd(sub_value,offset + end - 1);
   SetpDOBegin(sub_value,offset + start - 1);
  }

/******************************************/
/* MVSubseqFunction: H/L access routine   */
/*   for the mv-subseq function.          */
/******************************************/
globle void MVSubseqFunction(
  DATA_OBJECT_PTR sub_value)
  {
   DATA_OBJECT value;
   struct multifield *theList;
   long offset, start, end, length; /* 6.04 Bug Fix */

   /*=============================================*/
   /* Get range arguments. If they are not within */
   /* appropriate ranges, return a null segment.  */
   /*=============================================*/

   if (ArgTypeCheck("mv-subseq",1,INTEGER,&value) == FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   start = DOToInteger(value);

   if (ArgTypeCheck("mv-subseq",2,INTEGER,&value) == FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   end = DOToInteger(value);

   if ((end < 1) || (end < start))
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/

   if (ArgTypeCheck("mv-subseq",3,MULTIFIELD,&value) == FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   theList = (struct multifield *) DOToPointer(value);
   offset = GetDOBegin(value);

   /*===================================================*/
   /* Adjust lengths  to conform to segment boundaries. */
   /*===================================================*/

   length = GetDOLength(value);
   if (start > length)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   if (end > length) end = length;
   if (start < 1) start = 1;

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(sub_value,MULTIFIELD);
   SetpValue(sub_value,theList);
   SetpDOEnd(sub_value,offset + end - 1);
   SetpDOBegin(sub_value,offset + start - 1);
  }

/***************************************/
/* FirstFunction: H/L access routine   */
/*   for the first$ function.          */
/***************************************/
globle void FirstFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT theValue;
   struct multifield *theList;

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/

   if (ArgTypeCheck("first$",1,MULTIFIELD,&theValue) == FALSE)
     {
      SetMultifieldErrorValue(returnValue);
      return;
     }

   theList = (struct multifield *) DOToPointer(theValue);

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(returnValue,MULTIFIELD);
   SetpValue(returnValue,theList);
   if (GetDOEnd(theValue) >= GetDOBegin(theValue))
     { SetpDOEnd(returnValue,GetDOBegin(theValue)); }
   else
     { SetpDOEnd(returnValue,GetDOEnd(theValue)); }
   SetpDOBegin(returnValue,GetDOBegin(theValue));
  }

/**************************************/
/* RestFunction: H/L access routine   */
/*   for the rest$ function.          */
/**************************************/
globle void RestFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT theValue;
   struct multifield *theList;

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/

   if (ArgTypeCheck("rest$",1,MULTIFIELD,&theValue) == FALSE)
     {
      SetMultifieldErrorValue(returnValue);
      return;
     }

   theList = (struct multifield *) DOToPointer(theValue);

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(returnValue,MULTIFIELD);
   SetpValue(returnValue,theList);
   if (GetDOBegin(theValue) > GetDOEnd(theValue))
     { SetpDOBegin(returnValue,GetDOBegin(theValue)); }
   else
     { SetpDOBegin(returnValue,GetDOBegin(theValue) + 1); }
   SetpDOEnd(returnValue,GetDOEnd(theValue));
  }

/*************************************/
/* NthFunction: H/L access routine   */
/*   for the nth$ function.          */
/*************************************/
globle void NthFunction(
  DATA_OBJECT_PTR nth_value)
  {
   DATA_OBJECT value1, value2;
   struct multifield *elm_ptr;
   long n; /* 6.04 Bug Fix */

   if (ArgCountCheck("nth$",EXACTLY,2) == -1)
     {
      SetpType(nth_value,SYMBOL);
      SetpValue(nth_value,(void *) AddSymbol("nil"));
      return;
     }

   if ((ArgTypeCheck("nth$",1,INTEGER,&value1) == FALSE) ||
       (ArgTypeCheck("nth$",2,MULTIFIELD,&value2) == FALSE))
     {
      SetpType(nth_value,SYMBOL);
      SetpValue(nth_value,(void *) AddSymbol("nil"));
      return;
     }

   n = DOToLong(value1); /* 6.04 Bug Fix */
   if ((n > GetDOLength(value2)) || (n < 1))
     {
      SetpType(nth_value,SYMBOL);
      SetpValue(nth_value,(void *) AddSymbol("nil"));
      return;
     }

   elm_ptr = (struct multifield *) GetValue(value2);
   SetpType(nth_value,GetMFType(elm_ptr,n + GetDOBegin(value2) - 1));
   SetpValue(nth_value,GetMFValue(elm_ptr,n + GetDOBegin(value2) - 1));
  }

/* ------------------------------------------------------------------
 *    SubsetFunction:
 *               This function compares two multi-field variables
 *               to see if the first is a subset of the second. It
 *               does not consider order.
 *
 *    INPUTS:    Two arguments via argument stack. First is the sublist
 *               multi-field variable, the second is the list to be
 *               compared to. Both should be of type MULTIFIELD.
 *
 *    OUTPUTS:   TRUE if the first list is a subset of the
 *               second, else FALSE
 *
 *    NOTES:     This function is called from H/L with the subset
 *               command. Repeated values in the sublist must also
 *               be repeated in the main list.
 * ------------------------------------------------------------------
 */

globle BOOLEAN SubsetpFunction()
  {
   DATA_OBJECT item1, item2, tmpItem;
   long i,j,k; /* 6.04 Bug Fix */
   /*
   int fiSize;
   long * foundIndices;
   int l,usedficnt;
   */
   if (ArgCountCheck("subsetp",EXACTLY,2) == -1)
     return(FALSE);

   if (ArgTypeCheck("subsetp",1,MULTIFIELD,&item1) == FALSE)
     return(FALSE);

   if (ArgTypeCheck("subsetp",2,MULTIFIELD,&item2) == FALSE)
     return(FALSE);

   if (GetDOLength(item1) == 0) return(TRUE);
   if (GetDOLength(item2) == 0) return(FALSE);

   for (i = GetDOBegin(item1) ; i <= GetDOEnd(item1) ; i++)
     {
      SetType(tmpItem,GetMFType((struct multifield *) GetValue(item1),i));
      SetValue(tmpItem,GetMFValue((struct multifield *) GetValue(item1),i));


      if (! FindDOsInSegment(&tmpItem,1,&item2,&j,&k,NULL,0))
        { return(FALSE); }

      /*
      if (FindItemInSegment(GetMFType((struct multifield *) GetValue(item1),i),
                            GetMFValue((struct multifield *) GetValue(item1),i),&item2) == 0)
        { return(FALSE); }
      */
     }

   /* Brian's subsetp fix. */
   /*
   fiSize = (int) (sizeof(long) * GetDOLength(item2) * 2);
   foundIndices = (long *) gm2(fiSize);
   for (i = 0 ; i < GetDOLength(item2) * 2 ; i++)
     foundIndices[i] = 0L;
   usedficnt = 0;
   for (i = GetDOBegin(item1) ; i <= GetDOEnd(item1) ; i++)
     {
      SetType(tmpItem,GetMFType((struct multifield *) GetValue(item1),i));
      SetValue(tmpItem,GetMFValue((struct multifield *) GetValue(item1),i));
      if (!FindDOsInSegment(&tmpItem,1,&item2,&j,&k,usedficnt ? foundIndices : NULL,usedficnt))
        {
         rm((void *) foundIndices,fiSize);
         return(FALSE);
        }
      for (l = 0 ; l < usedficnt ; l++)
        if ((foundIndices[l*2] == j) &&
            (foundIndices[l*2+1] == k))
          break;
      if (l >= usedficnt)
        {
         foundIndices[l*2] = j;
         foundIndices[l*2+1] = k;
         usedficnt++;
        }
     }
   rm((void *) foundIndices,fiSize);
   */

   return(TRUE);
  }

/****************************************/
/* MemberFunction: H/L access routine   */
/*   for the member$ function.          */
/****************************************/
globle void MemberFunction(
  DATA_OBJECT_PTR result)
  {
   DATA_OBJECT item1, item2;
   long j,k;

   result->type = SYMBOL;
   result->value = FalseSymbol;

   if (ArgCountCheck("member$",EXACTLY,2) == -1) return;

   RtnUnknown(1,&item1);

   if (ArgTypeCheck("member$",2,MULTIFIELD,&item2) == FALSE) return;

   if (FindDOsInSegment(&item1,1,&item2,&j,&k,NULL,0))
     {
      if (j == k)
        {
         result->type = INTEGER;
         result->value = (void *) AddLong(j);
        }
      else
        {
         result->type = MULTIFIELD;
         result->value = CreateMultifield(2);
         SetMFType(result->value,1,INTEGER);
         SetMFValue(result->value,1,AddLong(j));
         SetMFType(result->value,2,INTEGER);
         SetMFValue(result->value,2,AddLong(k));
         SetpDOBegin(result,1);
         SetpDOEnd(result,2);
        }
     }
  }

/***************************************/
/* FindDOsInSegment:                  */
/***************************************/
/* 6.05 Bug Fix */
static BOOLEAN FindDOsInSegment(
  DATA_OBJECT_PTR searchDOs,
  int scnt,
  DATA_OBJECT_PTR value,
  long *si,
  long *ei,
  long *excludes,
  int epaircnt)
  {
   long mul_length,slen,i,k; /* 6.04 Bug Fix */
   int j;

   mul_length = GetpDOLength(value);
   for (i = 0 ; i < mul_length ; i++)
     {
      for (j = 0 ; j < scnt ; j++)
        {
         if (GetType(searchDOs[j]) == MULTIFIELD)
           {
            slen = GetDOLength(searchDOs[j]);
            if (MVRangeCheck(i+1L,i+slen,excludes,epaircnt))
              {
               for (k = 0L ; (k < slen) && ((k + i) < mul_length) ; k++)
                 if ((GetMFType(GetValue(searchDOs[j]),k+1L) !=
                      GetMFType(GetpValue(value),k+i+1L)) ||
                     (GetMFValue(GetValue(searchDOs[j]),k+1L) !=
                      GetMFValue(GetpValue(value),k+i+1L)))
                   break;
               if (k >= slen)
                 {
                  *si = i + 1L;
                  *ei = i + slen;
                  return(TRUE);
                 }
              }
           }
         else if ((GetValue(searchDOs[j]) == GetMFValue(GetpValue(value),i + GetpDOBegin(value))) &&
                  (GetType(searchDOs[j]) == GetMFType(GetpValue(value),i + GetpDOBegin(value))) &&
                  MVRangeCheck(i+1L,i+1L,excludes,epaircnt))
           {
            *si = *ei = i+1L;
            return(TRUE);
           }
        }
     }

   return(FALSE);
  }

/******************************************************/
/* MVRangeCheck:  */
/******************************************************/
static BOOLEAN MVRangeCheck(
  long si,
  long ei,
  long *elist,
  int epaircnt)
{
  int i;

  if (!elist || !epaircnt)
    return(TRUE);
  for (i = 0 ; i < epaircnt ; i++)
    if (((si >= elist[i*2]) && (si <= elist[i*2+1])) ||
        ((ei >= elist[i*2]) && (ei <= elist[i*2+1])))
    return(FALSE);

  return(TRUE);
}

#if (! BLOAD_ONLY) && (! RUN_TIME)

/******************************************************/
/* MultifieldPrognParser: Parses the progn$ function. */
/******************************************************/
static struct expr *MultifieldPrognParser(
  struct expr *top,
  char *infile)
  {
   struct BindInfo *oldBindList,*newBindList,*prev;
   struct token tkn;
   struct expr *tmp;
   SYMBOL_HN *fieldVar = NULL;

   SavePPBuffer(" ");
   GetToken(infile,&tkn);

   /* ================================
      Simple form: progn$ <mf-exp> ...
      ================================ */
   if (tkn.type != LPAREN)
     {
      top->argList = ParseAtomOrExpression(infile,&tkn);
      if (top->argList == NULL)
        {
         ReturnExpression(top);
         return(NULL);
        }
     }
   else
     {
      GetToken(infile,&tkn);
      if (tkn.type != SF_VARIABLE)
        {
         if (tkn.type != SYMBOL)
           goto MvPrognParseError;
         top->argList = Function2Parse(infile,ValueToString(tkn.value));
         if (top->argList == NULL)
           {
            ReturnExpression(top);
            return(NULL);
           }
        }

      /* =========================================
         Complex form: progn$ (<var> <mf-exp>) ...
         ========================================= */
      else
        {
         fieldVar = (SYMBOL_HN *) tkn.value;
         SavePPBuffer(" ");
         top->argList = ParseAtomOrExpression(infile,NULL);
         if (top->argList == NULL)
           {
            ReturnExpression(top);
            return(NULL);
           }
         GetToken(infile,&tkn);
         if (tkn.type != RPAREN)
           goto MvPrognParseError;
         PPBackup();
         /* PPBackup(); */
         SavePPBuffer(tkn.printForm);
         SavePPBuffer(" ");
        }
     }

   if (CheckArgumentAgainstRestriction(top->argList,(int) 'm'))
     goto MvPrognParseError;
   oldBindList = GetParsedBindNames();
   SetParsedBindNames(NULL);
   IncrementIndentDepth(3);
   BreakContext = TRUE;
   ReturnContext = svContexts->rtn;
   PPCRAndIndent();
   top->argList->nextArg = GroupActions(infile,&tkn,TRUE,NULL,FALSE);
   DecrementIndentDepth(3);
   PPBackup();
   PPBackup();
   SavePPBuffer(tkn.printForm);
   if (top->argList->nextArg == NULL)
     {
      SetParsedBindNames(oldBindList);
      ReturnExpression(top);
      return(NULL);
     }
   tmp = top->argList->nextArg;
   top->argList->nextArg = tmp->argList;
   tmp->argList = NULL;
   ReturnExpression(tmp);
   newBindList = GetParsedBindNames();
   prev = NULL;
   while (newBindList != NULL)
     {
      if ((fieldVar == NULL) ? FALSE :
          (strcmp(ValueToString(newBindList->name),ValueToString(fieldVar)) == 0))
        {
         ClearParsedBindNames();
         SetParsedBindNames(oldBindList);
         PrintErrorID("MULTIFUN",2,FALSE);
         PrintRouter(WERROR,"Cannot rebind field variable in function progn$.\n");
         ReturnExpression(top);
         return(NULL);
        }
      prev = newBindList;
      newBindList = newBindList->next;
     }
   if (prev == NULL)
     SetParsedBindNames(oldBindList);
   else
     prev->next = oldBindList;
   if (fieldVar != NULL)
     ReplaceMvPrognFieldVars(fieldVar,top->argList->nextArg,0);
   return(top);

MvPrognParseError:
   SyntaxErrorMessage("progn$");
   ReturnExpression(top);
   return(NULL);
  }

/**********************************************/
/* ReplaceMvPrognFieldVars: Replaces variable */
/*   references found in the progn$ function. */
/**********************************************/
static void ReplaceMvPrognFieldVars(
  SYMBOL_HN *fieldVar,
  struct expr *exp,
  int depth)
  {
   int flen;

   flen = strlen(ValueToString(fieldVar));
   while (exp != NULL)
     {
      if ((exp->type != SF_VARIABLE) ? FALSE :
          (strncmp(ValueToString(exp->value),ValueToString(fieldVar),
                   (CLIPS_STD_SIZE) flen) == 0))
        {
         if (ValueToString(exp->value)[flen] == '\0')
           {
            exp->type = FCALL;
            exp->value = (void *) FindFunction("(get-progn$-field)");
            exp->argList = GenConstant(INTEGER,AddLong((long) depth));
           }
         else if (strcmp(ValueToString(exp->value) + flen,"-index") == 0)
           {
            exp->type = FCALL;
            exp->value = (void *) FindFunction("(get-progn$-index)");
            exp->argList = GenConstant(INTEGER,AddLong((long) depth));
           }
        }
      else if (exp->argList != NULL)
        {
         if ((exp->type == FCALL) && (exp->value == (void *) FindFunction("progn$")))
           ReplaceMvPrognFieldVars(fieldVar,exp->argList,depth+1);
         else
           ReplaceMvPrognFieldVars(fieldVar,exp->argList,depth);
        }
      exp = exp->nextArg;
     }
  }

#endif

/*****************************************/
/* MultifieldPrognFunction: H/L access   */
/*   routine for the progn$ function.    */
/*****************************************/
globle void MultifieldPrognFunction(
  DATA_OBJECT_PTR result)
  {
   EXPRESSION *exp;
   DATA_OBJECT argval;
   long i, end; /* 6.04 Bug Fix */
   FIELD_VAR_STACK *tmpField;

   tmpField = get_struct(fieldVarStack);
   tmpField->type = SYMBOL;
   tmpField->value = FalseSymbol;
   tmpField->nxt = FieldVarStack;
   FieldVarStack = tmpField;
   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (ArgTypeCheck("progn$",1,MULTIFIELD,&argval) == FALSE)
     {
      FieldVarStack = tmpField->nxt;
      rtn_struct(fieldVarStack,tmpField);
      return;
     }
   ValueInstall(&argval);
   end = GetDOEnd(argval);
   for (i = GetDOBegin(argval) ; i <= end ; i++)
     {
      tmpField->type = GetMFType(argval.value,i);
      tmpField->value = GetMFValue(argval.value,i);
      tmpField->index = i;
      for (exp = GetFirstArgument()->nextArg ; exp != NULL ; exp = exp->nextArg)
        {
         CurrentEvaluationDepth++;
         EvaluateExpression(exp,result);
         CurrentEvaluationDepth--;
         if (ReturnFlag == TRUE)
           { PropagateReturnValue(result); }
         PeriodicCleanup(FALSE,TRUE);

         if (HaltExecution || BreakFlag || ReturnFlag)
           {
            ValueDeinstall(&argval);
            BreakFlag = FALSE;
            if (HaltExecution)
              {
               result->type = SYMBOL;
               result->value = FalseSymbol;
              }
            FieldVarStack = tmpField->nxt;
            rtn_struct(fieldVarStack,tmpField);
            return;
           }
        }
     }
   ValueDeinstall(&argval);
   BreakFlag = FALSE;
   FieldVarStack = tmpField->nxt;
   rtn_struct(fieldVarStack,tmpField);
  }

/***************************************************/
/* GetMvPrognField                                 */
/***************************************************/
globle void GetMvPrognField(
  DATA_OBJECT_PTR result)
  {
   int depth;
   FIELD_VAR_STACK *tmpField;

   depth = ValueToInteger(GetFirstArgument()->value);
   tmpField = FieldVarStack;
   while (depth > 0)
     {
      tmpField = tmpField->nxt;
      depth--;
     }
   result->type = tmpField->type;
   result->value = tmpField->value;
  }

/***************************************************/
/* GetMvPrognField                                 */
/***************************************************/
globle long GetMvPrognIndex()
  {
   int depth;
   FIELD_VAR_STACK *tmpField;

   depth = ValueToInteger(GetFirstArgument()->value);
   tmpField = FieldVarStack;
   while (depth > 0)
     {
      tmpField = tmpField->nxt;
      depth--;
     }
   return(tmpField->index);
  }

#endif

#if OBJECT_SYSTEM || MULTIFIELD_FUNCTIONS

/**************************************************************************
  NAME         : ReplaceMultiValueField
  DESCRIPTION  : Performs a replace on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) Beginning of index range
                 4) End of range
                 5) The new field value
  RETURNS      : TRUE if successful, FALSE otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
globle int ReplaceMultiValueField(
  DATA_OBJECT *dst,
  DATA_OBJECT *src,
  long rb,
  long re,
  DATA_OBJECT *field,
  char *funcName)
  {
   long i,j,k;
   struct field *deptr;
   struct field *septr;
   long srclen,dstlen;

   srclen = (src != NULL) ? (src->end - src->begin + 1) : 0;
   if ((re < rb) ||
       (rb < 1) || (re < 1) ||
       (rb > srclen) || (re > srclen))
     {
      MVRangeError(rb,re,srclen,funcName);
      return(FALSE);
     }
   rb = src->begin + rb - 1;
   re = src->begin + re - 1;
   if (field->type == MULTIFIELD)
     dstlen = srclen + GetpDOLength(field) - (re-rb+1);
   else
     dstlen = srclen + 1 - (re-rb+1);
   dst->type = MULTIFIELD;
   dst->begin = 0;
   dst->value = CreateMultifield(dstlen);
   dst->end = dstlen-1;
   for (i = 0 , j = src->begin ; j < rb ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   if (field->type != MULTIFIELD)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i++];
      deptr->type = (short) field->type;
      deptr->value = field->value;
     }
   else
     {
      for (k = field->begin ; k <= field->end ; k++ , i++)
        {
         deptr = &((struct multifield *) dst->value)->theFields[i];
         septr = &((struct multifield *) field->value)->theFields[k];
         deptr->type = septr->type;
         deptr->value = septr->value;
        }
     }
   while (j < re)
     j++;
   for (j++ ; i < dstlen ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   return(TRUE);
  }

/**************************************************************************
  NAME         : InsertMultiValueField
  DESCRIPTION  : Performs an insert on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) The index for the change
                 4) The new field value
  RETURNS      : TRUE if successful, FALSE otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
globle int InsertMultiValueField(
  DATA_OBJECT *dst,
  DATA_OBJECT *src,
  long index,
  DATA_OBJECT *field,
  char *funcName)
  {
   register long i,j,k;
   register FIELD *deptr, *septr;
   long srclen,dstlen; /* 6.04 Bug Fix */

   srclen = (src != NULL) ? (src->end - src->begin + 1) : 0;
   if (index < 1)
     {
      MVRangeError(index,index,srclen+1,funcName);
      return(FALSE);
     }
   if (index > (srclen + 1))
     index = srclen + 1;
   dst->type = MULTIFIELD;
   dst->begin = 0;
   if (src == NULL)
     {
      if (field->type == MULTIFIELD)
        {
         DuplicateMultifield(dst,field);
         AddToMultifieldList((struct multifield *) dst->value);
        }
      else
        {
         dst->value = CreateMultifield(0L);
         dst->end = 0;
         deptr = &((struct multifield *) dst->value)->theFields[0];
         deptr->type = (short) field->type;
         deptr->value = field->value;
        }
      return(TRUE);
     }
   dstlen = (field->type == MULTIFIELD) ? GetpDOLength(field) + srclen : srclen + 1;
   dst->value = CreateMultifield(dstlen);
   dst->end = dstlen-1;
   index--;
   for (i = 0 , j = src->begin ; i < index ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   if (field->type != MULTIFIELD)
     {
      deptr = &((struct multifield *) dst->value)->theFields[index];
      deptr->type = (short) field->type;
      deptr->value = field->value;
      i++;
     }
   else
     {
      for (k = field->begin ; k <= field->end ; k++ , i++)
        {
         deptr = &((struct multifield *) dst->value)->theFields[i];
         septr = &((struct multifield *) field->value)->theFields[k];
         deptr->type = septr->type;
         deptr->value = septr->value;
        }
     }
   for ( ; j <= src->end ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   return(TRUE);
  }

/*******************************************************
  NAME         : MVRangeError
  DESCRIPTION  : Prints out an error messages for index
                   out-of-range errors in multi-field
                   access functions
  INPUTS       : 1) The bad range start
                 2) The bad range end
                 3) The max end of the range (min is
                     assumed to be 1)
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ******************************************************/
static void MVRangeError(
  long brb,
  long bre,
  long max,
  char *funcName)
  {
   PrintErrorID("MULTIFUN",1,FALSE);
   PrintRouter(WERROR,"Multifield index ");
   if (brb == bre)
     PrintLongInteger(WERROR,(long) brb);
   else
     {
      PrintRouter(WERROR,"range ");
      PrintLongInteger(WERROR,(long) brb);
      PrintRouter(WERROR,"..");
      PrintLongInteger(WERROR,(long) bre);
     }
   PrintRouter(WERROR," out of range 1..");
   PrintLongInteger(WERROR,(long) max);
   if (funcName != NULL)
     {
      PrintRouter(WERROR," in function ");
      PrintRouter(WERROR,funcName);
     }
   PrintRouter(WERROR,".\n");
  }

/**************************************************************************
  NAME         : DeleteMultiValueField
  DESCRIPTION  : Performs a modify on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) The beginning index for deletion
                 4) The ending index for deletion
  RETURNS      : TRUE if successful, FALSE otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
globle int DeleteMultiValueField(
  DATA_OBJECT *dst,
  DATA_OBJECT *src,
  long rb,
  long re,
  char *funcName)
  {
   register long i,j;
   register FIELD_PTR deptr,septr;
   long srclen, dstlen;

   srclen = (src != NULL) ? (src->end - src->begin + 1) : 0;
   if ((re < rb) ||
       (rb < 1) || (re < 1) ||
       (rb > srclen) || (re > srclen))
     {
      MVRangeError(rb,re,srclen,funcName);
      return(FALSE);
     }
   dst->type = MULTIFIELD;
   dst->begin = 0;
   if (srclen == 0)
    {
     dst->value = CreateMultifield(0L);
     dst->end = -1;
     return(TRUE);
    }
   rb = src->begin + rb -1;
   re = src->begin + re -1;
   dstlen = srclen-(re-rb+1);
   dst->end = dstlen-1;
   dst->value = CreateMultifield(dstlen);
   for (i = 0 , j = src->begin ; j < rb ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   while (j < re)
     j++;
   for (j++ ; i <= dst->end ; j++ , i++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   return(TRUE);
  }

#endif


