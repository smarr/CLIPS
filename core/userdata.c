   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/28/98            */
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
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _USERDATA_SOURCE_

#include <stdlib.h>

#include "setup.h"

#include "userdata.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct userDataRecord  *UserDataRecordArray[MAXIMUM_USER_DATA_RECORDS];
   Thread static unsigned short          UserDataRecordCount = 0;

/******************************************************/
/* InstallUserDataRecord: Installs a user data record */
/*   in the user data record array and returns the    */
/*   integer data ID associated with the record.      */
/******************************************************/
globle unsigned char InstallUserDataRecord(
  struct userDataRecord *theRecord)
  {
   theRecord->dataID = UserDataRecordCount;
   UserDataRecordArray[UserDataRecordCount] = theRecord;
   return(UserDataRecordCount++);
  }

/*****************************************************/
/* FetchUserData: Searches for user data information */
/*   from a list of user data structures. A new user */
/*   data structure is created if one is not found.  */
/*****************************************************/
globle struct userData *FetchUserData(
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
     
   theData = (struct userData *) (*UserDataRecordArray[userDataID]->createUserData)();
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
  struct userData *theList)
  {
   struct userData *nextData;
   
   while (theList != NULL)
     {
      nextData = theList->next;
      (*UserDataRecordArray[theList->dataID]->deleteUserData)(theList);
      theList = nextData;
     }
  }
  
/*************************************************/
/* DeleteUserData: Removes user data information */
/*   from a list of user data structures.        */
/*************************************************/
globle struct userData *DeleteUserData(
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
            
         (*UserDataRecordArray[userDataID]->deleteUserData)(theData);
         return(theList);
        }
        
      lastData = theData;
     }
        
   return(theList);   
  }
