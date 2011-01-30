   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/28/98            */
   /*                                                     */
   /*                USER DATA HEADER FILE                */
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

#ifndef _H_userdata
#define _H_userdata

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _USERDATA_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

struct userData
  {
   unsigned char dataID;
   struct userData *next;
  };

typedef struct userData USER_DATA;
typedef struct userData * USER_DATA_PTR;
  
struct userDataRecord
  {
   unsigned char dataID;
   void *(*createUserData)(void);
   void (*deleteUserData)(void *);
  };
  
typedef struct userDataRecord USER_DATA_RECORD;
typedef struct userDataRecord * USER_DATA_RECORD_PTR;

#define MAXIMUM_USER_DATA_RECORDS 100
  
   LOCALE unsigned char                  InstallUserDataRecord(struct userDataRecord *);
   LOCALE struct userData               *FetchUserData(unsigned char,struct userData **);
   LOCALE struct userData               *TestUserData(unsigned char,struct userData *);
   LOCALE void                           ClearUserDataList(struct userData *);
   LOCALE struct userData               *DeleteUserData(unsigned char,struct userData *);

#endif




