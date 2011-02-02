   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*                  USER DATA MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for attaching user data to constructs,  */
/*   facts, instances, user functions, etc.                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _USERDATA_SOURCE_

#include <stdlib.h>

#include "setup.h"

#include "envrnmnt.h"

#include "userdata.h"

/*************************************************/
/* InitializeUserDataData: Allocates environment */
/*    data for user data routines.               */
/*************************************************/
globle void InitializeUserDataData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,USER_DATA_DATA,sizeof(struct userDataData),NULL);
  }

/******************************************************/
/* InstallUserDataRecord: Installs a user data record */
/*   in the user data record array and returns the    */
/*   integer data ID associated with the record.      */
/******************************************************/
globle unsigned char InstallUserDataRecord(
  void *theEnv,
  EXEC_STATUS,
  struct userDataRecord *theRecord)
  {
   theRecord->dataID = UserDataData(theEnv,execStatus)->UserDataRecordCount;
   UserDataData(theEnv,execStatus)->UserDataRecordArray[UserDataData(theEnv,execStatus)->UserDataRecordCount] = theRecord;
   return(UserDataData(theEnv,execStatus)->UserDataRecordCount++);
  }
  
/*****************************************************/
/* FetchUserData: Searches for user data information */
/*   from a list of user data structures. A new user */
/*   data structure is created if one is not found.  */
/*****************************************************/
globle struct userData *FetchUserData(
  void *theEnv,
  EXEC_STATUS,
  unsigned char userDataID,
  struct userData **theList)
  {
   struct userData *theData;

   for (theData = *theList;
        theData != NULL;
        theData = theData->next)
     {
      if (theData->dataID == userDataID)
        { return(theData); }
     }
     
   theData = (struct userData *) (*UserDataData(theEnv,execStatus)->UserDataRecordArray[userDataID]->createUserData)(theEnv,execStatus);
   theData->dataID = userDataID;
   theData->next = *theList;
   *theList = theData;
   
   return(theData);   
  }

/*****************************************************/
/* TestUserData: Searches for user data information  */
/*   from a list of user data structures. NULL is    */
/*   returned if the appropriate user data structure */
/*   is not found.                                   */
/*****************************************************/
globle struct userData *TestUserData(
  unsigned char userDataID,
  struct userData *theList)
  {
   struct userData *theData;
   
   for (theData = theList;
        theData != NULL;
        theData = theData->next)
     {
      if (theData->dataID == userDataID)
        { return(theData); }
     }
        
   return(NULL);   
  }

/***************************************************************/
/* ClearUserDataList: Deallocates a linked list of user data.  */
/***************************************************************/
globle void ClearUserDataList(
  void *theEnv,
  EXEC_STATUS,
  struct userData *theList)
  {
   struct userData *nextData;
   
   while (theList != NULL)
     {
      nextData = theList->next;
      (*UserDataData(theEnv,execStatus)->UserDataRecordArray[theList->dataID]->deleteUserData)(theEnv,execStatus,theList);
      theList = nextData;
     }
  }
  
/*************************************************/
/* DeleteUserData: Removes user data information */
/*   from a list of user data structures.        */
/*************************************************/
globle struct userData *DeleteUserData(
  void *theEnv,
  EXEC_STATUS,
  unsigned char userDataID,
  struct userData *theList)
  {
   struct userData *theData, *lastData = NULL;
   
   for (theData = theList;
        theData != NULL;
        theData = theData->next)
     {
      if (theData->dataID == userDataID)
        { 
         if (lastData == NULL)
           { theList = theData->next; }
         else
           { lastData->next = theData->next; }
            
         (*UserDataData(theEnv,execStatus)->UserDataRecordArray[userDataID]->deleteUserData)(theEnv,execStatus,theData);
         return(theList);
        }
        
      lastData = theData;
     }
        
   return(theList);   
  }

