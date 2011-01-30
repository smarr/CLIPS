   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                    BLOAD MODULE                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for loading constructs    */
/*   from a binary file.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _BLOAD_SOURCE_

#include "setup.h"

#include "memalloc.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"
#include "constrct.h"
#include "bsave.h"
#include "cstrnbin.h"
#include "utility.h"
#include "crc.h"
#include "bload.h"

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct FunctionDefinition **ReadNeededFunctions(long *,int *);
   static struct FunctionDefinition  *FastFindFunction(char *,struct FunctionDefinition *);
   static int                         ClearBload(void);
   static void                        AbortBload(void);
   static int                         BloadOutOfMemoryFunction(unsigned long);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static int                                    BloadActive = FALSE;
   Thread static struct callFunctionItem               *BeforeBloadFunctions = NULL;
   Thread static struct callFunctionItem               *AfterBloadFunctions = NULL;
   Thread static struct callFunctionItem               *ClearBloadReadyFunctions = NULL;
   Thread static struct callFunctionItem               *AbortBloadFunctions = NULL;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle char                                  *BinaryPrefixID = "\1\2\3\4CLIPS";
   Thread globle char                                  *BinaryVersionID = "V6.10";
   Thread globle struct FunctionDefinition            **FunctionArray;

/****************************/
/* Bload: C access routine  */
/*   for the bload command. */
/****************************/
globle int Bload(
  char *fileName)
  {
   long numberOfFunctions;
   unsigned long space;
   int error;
   unsigned long myCRC;
   char IDbuffer[20];
   char CRCstr1[9];
   char CRCstr2[9];
   char constructBuffer[CONSTRUCT_HEADER_SIZE];
   struct BinaryItem *biPtr;
   struct callFunctionItem *bfPtr;

   /*================*/
   /* Open the file. */
   /*================*/

   if (GenOpen("bload",fileName) == 0) return(FALSE);

   /*=====================================*/
   /* Determine if this is a binary file. */
   /*=====================================*/

   GenRead(IDbuffer,(unsigned long) strlen(BinaryPrefixID) + 1);
   if (strcmp(IDbuffer,BinaryPrefixID) != 0)
     {
      PrintErrorID("BLOAD",2,FALSE);
      PrintRouter(WERROR,"File ");
      PrintRouter(WERROR,fileName);
      PrintRouter(WERROR," is not a binary construct file.\n");
      GenClose();
      return(FALSE);
     }

   /*=======================================*/
   /* Determine if it's a binary file using */
   /* a format from a different version.    */
   /*=======================================*/

   GenRead(IDbuffer,(unsigned long) strlen(BinaryVersionID) + 1);
   if (strcmp(IDbuffer,BinaryVersionID) != 0)
     {
      PrintErrorID("BLOAD",3,FALSE);
      PrintRouter(WERROR,"File ");
      PrintRouter(WERROR,fileName);
      PrintRouter(WERROR," is an incompatible binary construct file.\n");
      GenClose();
      return(FALSE);
     }

   /*=======================================*/
   /* Determine if it's CRC is valid        */
   /*=======================================*/

   memset(CRCstr1,0,9);
   memset(CRCstr2,0,9);
   GenRead(CRCstr1,8);
   myCRC = CalcCRC(GetGenFP());
   sprintf(CRCstr2,"%X",myCRC);

   if(strcmp(CRCstr1, CRCstr2) != 0)	{
      PrintErrorID("BLOAD",2,FALSE);
      PrintRouter(WERROR,"File ");
      PrintRouter(WERROR,fileName);
      PrintRouter(WERROR," the CRC is invalid.\n");
      GenClose();
      return(FALSE);
   }

   /*====================*/
   /* Clear environment. */
   /*====================*/

   if (BloadActive)
     {
      if (ClearBload() == FALSE)
        {
         GenClose();
         return(FALSE);
        }
     }

   /*=================================*/
   /* Determine if the KB environment */
   /* was successfully cleared.       */
   /*=================================*/

   if (ClearReady() == FALSE)
     {
      GenClose();
      PrintRouter(WERROR,"The ");
      PrintRouter(WERROR,APPLICATION_NAME);
      PrintRouter(WERROR," environment could not be cleared.\n");
      PrintRouter(WERROR,"Binary load cannot continue.\n");
      return(FALSE);
     }

   /*==================================*/
   /* Call the list of functions to be */
   /* executed before a bload occurs.  */
   /*==================================*/

   for (bfPtr = BeforeBloadFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     { (*bfPtr->func)(); }

   /*====================================================*/
   /* Read in the functions needed by this binary image. */
   /*====================================================*/

   FunctionArray = ReadNeededFunctions(&numberOfFunctions,&error);
   if (error)
     {
      GenClose();
      AbortBload();
      return(FALSE);
     }

   /*================================================*/
   /* Read in the atoms needed by this binary image. */
   /*================================================*/

   ReadNeededAtomicValues();

   /*===========================================*/
   /* Determine the number of expressions to be */
   /* read and allocate the appropriate space   */
   /*===========================================*/

   AllocateExpressions();

   /*==========================================================*/
   /* Read in the memory requirements of the constructs stored */
   /* in this binary image and allocate the necessary space    */
   /*==========================================================*/

   for (GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE);
        strncmp(constructBuffer,BinaryPrefixID,CONSTRUCT_HEADER_SIZE) != 0;
        GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE))
     {
      BOOLEAN found;

      /*================================================*/
      /* Search for the construct type in the list of   */
      /* binary items. If found, allocate the storage   */
      /* needed by the construct for this binary image. */
      /*================================================*/

      found = FALSE;
      for (biPtr = ListOfBinaryItems;
           biPtr != NULL;
           biPtr = biPtr->next)
        {
         if (strncmp(biPtr->name,constructBuffer,CONSTRUCT_HEADER_SIZE) == 0)
           {
            if (biPtr->bloadStorageFunction != NULL)
              {
               (*biPtr->bloadStorageFunction)();
               found = TRUE;
              }
            break;
           }
        }

      /*==========================================*/
      /* If the construct type wasn't found, skip */
      /* the storage binary load information for  */
      /* this construct.                          */
      /*==========================================*/

      if (! found)
        {
         GenRead(&space,(unsigned long) sizeof(unsigned long));
         GenSeek((long) space);
         if (space != 0)
           {
            PrintRouter(WDIALOG,"\nSkipping ");
            PrintRouter(WDIALOG,constructBuffer);
            PrintRouter(WDIALOG," constructs because of unavailibility\n");
           }
        }
     }

   /*======================================*/
   /* Refresh the pointers in expressions. */
   /*======================================*/

   RefreshExpressions();

   /*==========================*/
   /* Read in the constraints. */
   /*==========================*/

   ReadNeededConstraints();

   /*======================================================*/
   /* Read in the constructs stored in this binary image.  */
   /*======================================================*/

   for (GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE);
        strncmp(constructBuffer,BinaryPrefixID,CONSTRUCT_HEADER_SIZE) != 0;
        GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE))
     {
      BOOLEAN found;

      /*==================================================*/
      /* Search for the function to load the construct    */
      /* into the previously allocated storage. If found, */
      /* call the function to load the construct.         */
      /*==================================================*/

      found = FALSE;
      for (biPtr = ListOfBinaryItems;
           biPtr != NULL;
           biPtr = biPtr->next)
        {
         if (strncmp(biPtr->name,constructBuffer,CONSTRUCT_HEADER_SIZE) == 0)
           {
            if (biPtr->bloadFunction != NULL)
              {
               (*biPtr->bloadFunction)();
               found = TRUE;
              }
            break;
           }
        }

      /*==========================================*/
      /* If the construct type wasn't found, skip */
      /* the binary data for this construct.      */
      /*==========================================*/

      if (! found)
        {
         GenRead(&space,(unsigned long) sizeof(unsigned long));
         GenSeek((long) space);
        }
     }

   /*=================*/
   /* Close the file. */
   /*=================*/

   GenClose();

   /*========================================*/
   /* Free up temporary storage used for the */
   /* function and atomic value information. */
   /*========================================*/

   if (FunctionArray != NULL)
     {
      genlongfree((void *) FunctionArray,
                  (unsigned long) sizeof(struct FunctionDefinition *) * numberOfFunctions);
     }
   FreeAtomicValueStorage();

   /*==================================*/
   /* Call the list of functions to be */
   /* executed after a bload occurs.   */
   /*==================================*/

   for (bfPtr = AfterBloadFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     { (*bfPtr->func)(); }

   /*=======================================*/
   /* Add a clear function to remove binary */
   /* load when a clear command is issued.  */
   /*=======================================*/

   BloadActive = TRUE;
   AddClearFunction("bload",(void (*)(void)) ClearBload,10000);

   /*=============================*/
   /* Return TRUE to indicate the */
   /* binary load was successful. */
   /*=============================*/

   return(TRUE);
  }

/************************************************************
  NAME         : BloadandRefresh
  DESCRIPTION  : Loads and refreshes objects - will bload
                 all objects at once, if possible, but
                 will aslo work in increments if memory is
                 restricted
  INPUTS       : 1) the number of objects to bload and update
                 2) the size of one object
                 3) An update function which takes a bloaded
                    object buffer and the index of the object
                    to refresh as arguments
  RETURNS      : Nothing useful
  SIDE EFFECTS : Objects bloaded and updated
  NOTES        : Assumes binary file pointer is positioned
                 for bloads of the objects
 ************************************************************/
globle void BloadandRefresh(
  long objcnt,
  unsigned objsz,
  void (*objupdate)(void *,long))
  {
   register long i,bi;
   char *buf;
   long objsmaxread,objsread;
   unsigned long space;
   int (*oldOutOfMemoryFunction)(unsigned long);

   if (objcnt == 0L) return;

   oldOutOfMemoryFunction = SetOutOfMemoryFunction(BloadOutOfMemoryFunction);
   objsmaxread = objcnt;
   do
     {
      space = objsmaxread * objsz;
      buf = (char *) genlongalloc(space);
      if (buf == NULL)
        {
         if ((objsmaxread / 2) == 0)
           {
            if ((*oldOutOfMemoryFunction)(space) == TRUE)
              {
               SetOutOfMemoryFunction(oldOutOfMemoryFunction);
               return;
              }
           }
         else
           objsmaxread /= 2;
        }
     }
   while (buf == NULL);

   SetOutOfMemoryFunction(oldOutOfMemoryFunction);

   i = 0L;
   do
     {
      objsread = (objsmaxread > (objcnt - i)) ? (objcnt - i) : objsmaxread;
      GenRead((void *) buf,objsread * objsz);
      for (bi = 0L ; bi < objsread ; bi++ , i++)
        (*objupdate)(buf + objsz * bi,i);
     }
   while (i < objcnt);
   genlongfree((void *) buf,space);
  }

/**********************************************/
/* ReadNeededFunctions: Reads in the names of */
/*   functions needed by the binary image.    */
/**********************************************/
static struct FunctionDefinition **ReadNeededFunctions(
  long int *numberOfFunctions,
  int *error)
  {
   char *functionNames, *namePtr;
   unsigned long int space,temp;
   long i;
   struct FunctionDefinition **newFunctionArray, *functionPtr;
   int functionsNotFound = 0;

   /*===================================================*/
   /* Determine the number of function names to be read */
   /* and the space required for them.                  */
   /*===================================================*/

   GenRead(numberOfFunctions,(unsigned long) sizeof(long int));
   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   if (*numberOfFunctions == 0)
     {
      *error = FALSE;
      return(NULL);
     }

   /*=======================================*/
   /* Allocate area for strings to be read. */
   /*=======================================*/

   functionNames = (char *) genlongalloc(space);
   GenRead((void *) functionNames,space);

   /*====================================================*/
   /* Store the function pointers in the function array. */
   /*====================================================*/

   temp = (unsigned long) sizeof(struct FunctionDefinition *) * *numberOfFunctions;
   newFunctionArray = (struct FunctionDefinition **) genlongalloc(temp);
   namePtr = functionNames;
   functionPtr = NULL;
   for (i = 0; i < *numberOfFunctions; i++)
     {
      if ((functionPtr = FastFindFunction(namePtr,functionPtr)) == NULL)
        {
         if (! functionsNotFound)
           {
            PrintErrorID("BLOAD",6,FALSE);
            PrintRouter(WERROR,"The following undefined functions are ");
            PrintRouter(WERROR,"referenced by this binary image:\n");
           }

         PrintRouter(WERROR,"   ");
         PrintRouter(WERROR,namePtr);
         PrintRouter(WERROR,"\n");
         functionsNotFound = 1;
        }

      newFunctionArray[i] = functionPtr;
      namePtr += strlen(namePtr) + 1;
     }

   /*==========================================*/
   /* Free the memory used by the name buffer. */
   /*==========================================*/

   genlongfree((void *) functionNames,space);

   /*==================================================*/
   /* If any of the required functions were not found, */
   /* then free the memory used by the function array. */
   /*==================================================*/

   if (functionsNotFound)
     {
      genlongfree((void *) newFunctionArray,temp);
      newFunctionArray = NULL;
     }

   /*===================================*/
   /* Set globals to appropriate values */
   /* and return the function array.    */
   /*===================================*/

   *error = functionsNotFound;
   return(newFunctionArray);
  }

/*****************************************/
/* FastFindFunction: Search the function */
/*   list for a specific function.       */
/*****************************************/
static struct FunctionDefinition *FastFindFunction(
  char *functionName,
  struct FunctionDefinition *lastFunction)
  {
   struct FunctionDefinition *theList, *theFunction;

   /*========================*/
   /* Get the function list. */
   /*========================*/

   theList = GetFunctionList();
   if (theList == NULL) { return(NULL); }

   /*=======================================*/
   /* If we completed a previous function   */
   /* search, start where we last left off. */
   /*=======================================*/

   if (lastFunction != NULL)
     { theFunction = lastFunction->next; }
   else
     { theFunction = theList; }

   /*======================================================*/
   /* Traverse the rest of the function list searching for */
   /* the named function wrapping around if necessary.     */
   /*======================================================*/

   while (strcmp(functionName,ValueToString(theFunction->callFunctionName)) != 0)
     {
      theFunction = theFunction->next;
      if (theFunction == lastFunction) return(NULL);
      if (theFunction == NULL) theFunction = theList;
     }

   /*=======================*/
   /* Return the pointer to */
   /* the found function.   */
   /*=======================*/

   return(theFunction);
  }

/******************************************/
/* Bloaded: Returns TRUE if the current   */
/*   environment is the result of a bload */
/*   command, otherwise returns FALSE.    */
/******************************************/
globle BOOLEAN Bloaded()
  {
   return(BloadActive);
  }

/*************************************/
/* ClearBload: Clears a binary image */
/*   from the KB environment.        */
/*************************************/
static int ClearBload()
  {
   struct BinaryItem *biPtr;
   struct callFunctionItem *bfPtr;
   int ready,error;

   /*=================================================*/
   /* Make sure it's safe to clear the bloaded image. */
   /*=================================================*/

   error = FALSE;
   for (bfPtr = ClearBloadReadyFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     {
      ready = (* ((int (*)(void)) bfPtr->func))();
      if (ready == FALSE)
        {
         if (! error)
           {
            PrintErrorID("BLOAD",5,FALSE);
            PrintRouter(WERROR,
                       "Some constructs are still in use by the current binary image:\n");
           }
         PrintRouter(WERROR,"   ");
         PrintRouter(WERROR,bfPtr->name);
         PrintRouter(WERROR,"\n");
         error = TRUE;
        }
     }

   /*==================================================*/
   /* If some constructs are still in use and can't be */
   /* cleared, indicate the binary load can't continue */
   /* and return FALSE to indicate this condition.     */
   /*==================================================*/

   if (error == TRUE)
     {
      PrintRouter(WERROR,"Binary clear cannot continue.\n");
      return(FALSE);
     }

   /*=============================*/
   /* Call bload clear functions. */
   /*=============================*/

   for (biPtr = ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     { if (biPtr->clearFunction != NULL) (*biPtr->clearFunction)(); }

   /*===========================*/
   /* Free bloaded expressions. */
   /*===========================*/

   ClearBloadedExpressions();

   /*===========================*/
   /* Free bloaded constraints. */
   /*===========================*/

   ClearBloadedConstraints();

   /*==================================*/
   /* Remove the bload clear function. */
   /*==================================*/

   BloadActive = FALSE;
   RemoveClearFunction("bload");

   /*====================================*/
   /* Return TRUE to indicate the binary */
   /* image was successfully cleared.    */
   /*====================================*/

   return(TRUE);
  }

/*************************************************/
/* AbortBload: Cleans up effects of before-bload */
/*   functions in event of failure.              */
/*************************************************/
static void AbortBload()
  {
   struct callFunctionItem *bfPtr;

   for (bfPtr = AbortBloadFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     { (*bfPtr->func)(); }
  }

/********************************************/
/* AddBeforeBloadFunction: Adds a function  */
/*   to the list of functions called before */
/*   a binary load occurs.                  */
/********************************************/
globle void AddBeforeBloadFunction(
  char *name,
  void (*func)(void),
  int priority)
  {
   BeforeBloadFunctions =
     AddFunctionToCallList(name,priority,func,BeforeBloadFunctions);
  }

/*******************************************/
/* AddAfterBloadFunction: Adds a function  */
/*   to the list of functions called after */
/*   a binary load occurs.                 */
/*******************************************/
globle void AddAfterBloadFunction(
  char *name,
  void (*func)(void),
  int priority)
  {
   AfterBloadFunctions =
      AddFunctionToCallList(name,priority,func,AfterBloadFunctions);
  }

/**************************************************/
/* AddClearBloadReadyFunction: Adds a function to */
/*   the list of functions called to determine if */
/*   a binary image can be cleared.               */
/**************************************************/
globle void AddClearBloadReadyFunction(
  char *name,
  int (*func)(void),
  int priority)
  {
   ClearBloadReadyFunctions =
      AddFunctionToCallList(name,priority,
                            (void (*)(void)) func,
                            ClearBloadReadyFunctions);
  }

/*********************************************/
/* AddAbortBloadFunction: Adds a function to */
/*   the list of functions called if a bload */
/*   has to be aborted.                      */
/*********************************************/
globle void AddAbortBloadFunction(
  char *name,
  void (*func)(void),
  int priority)
  {
   AbortBloadFunctions = AddFunctionToCallList(name,priority,func,AbortBloadFunctions);
  }

/*******************************************************
  NAME         : BloadOutOfMemoryFunction
  DESCRIPTION  : Memory function used by bload to
                   prevent exiting when out of
                   memory - used by BloadandRefresh
  INPUTS       : The memory request size (unused)
  RETURNS      : TRUE (indicates a failure and for
                 the memory functions to simply
                 return a NULL pointer)
  SIDE EFFECTS : None
  NOTES        : None
 *******************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int BloadOutOfMemoryFunction(
  unsigned long size)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(size)
#endif
   return(TRUE);
  }

/*****************************************************/
/* CannotLoadWithBloadMessage: Generic error message */
/*   for indicating that a construct can't be loaded */
/*   when a binary image is active.                  */
/*****************************************************/
globle void CannotLoadWithBloadMessage(
  char *constructName)
  {
   PrintErrorID("BLOAD",1,TRUE);
   PrintRouter(WERROR,"Cannot load ");
   PrintRouter(WERROR,constructName);
   PrintRouter(WERROR," construct with binary load in effect.\n");
  }

#endif /* (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) */

/**************************************/
/* BloadCommand: H/L access routine   */
/*   for the bload command.           */
/**************************************/
globle int BloadCommand()
  {
#if (! RUN_TIME) && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   char *fileName;

   if (ArgCountCheck("bload",EXACTLY,1) == -1) return(FALSE);
   fileName = GetFileName("bload",1);
   if (fileName != NULL) return(Bload(fileName));
#endif
   return(FALSE);
  }

