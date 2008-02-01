   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*               CLIPS Version 6.00  01/01/93          */
   /*                                                     */
   /*                     DDE MODULE                      */
   /*******************************************************/

/**************************************************************/
/* Purpose: For Starting, Maintaining and Stopping a Dymanic  */
/*       Data Exchange (DDE) conversation with a client.      */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

/*-------------------------------+
| Windows & System Include Files |
+-------------------------------*/
#include <windows.h>
#undef CopyMemory
#include <string.h>
#include <DDEML.h>

/*------------------------+
| CLIPS 6.0 Include Files |
+------------------------*/
#include "setup.h"
#include "clips.h"
#include "router.h"
#include "commline.h"
#include "memalloc.h"
#include "filecom.h"
#include "strngrtr.h"
#include "prntutil.h"
#include "cstrcpsr.h"

/*------------------------+
| Interface Include Files |
+------------------------*/

/*
#include "ids.h"
#include "main.h"
#include "menucmds.h"
#include "clipsdde.h"
*/
#include "display.h"

/*----------------------+
| DDE Prep for a Server |
+---------------------*/
HSZ   hszService  = NULL;
HSZ   hszCommand  = NULL;
HSZ   hszResult   = NULL;
HCONV hConvApp    = NULL;
DWORD idInst      = 0;
DATA_OBJECT DDE_RV;

/*************************************************************
* DDECallBack : This function is an application-defined dynamic
*    data exchange (DDE) callback function that processes DDE
*    transactions sent to the function as a result of DDE
*    Management Library (DDEML) calls by other applications.
*************************************************************/
#if WIN_BTC
#pragma argsused
#endif

HDDEDATA CALLBACK DDECallBack(  
  UINT uType,
  UINT uFmt,
  HCONV hconv,
  HSZ hsz1,
  HSZ hsz2,
  HDDEDATA hData,
  DWORD dwData1,
  DWORD dwData2)
  {  
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(hconv)
#pragma unused(dwData1)
#pragma unused(dwData2)
#endif

   extern HSZ hszService; 
   extern char CompleteString[255];
   DWORD size;
   HWND hWnd;
   char *TheData;
   char *Data;
   char *theString;
   void *theEnv = GetCurrentEnvironment();
   
   /* 
   char theBuffer[100];
   DWORD cb; 
   PSTR pszTopicName; 
   */
      
   switch (uType)
     {  
      case XTYP_ADVDATA:
      case XTYP_POKE:
        return ((HDDEDATA) DDE_FNOTPROCESSED);
        
      case XTYP_ADVREQ:
      case XTYP_WILDCONNECT:
        return ((HDDEDATA) NULL);
        
      case XTYP_ADVSTART:
      case XTYP_ADVSTOP:
      case XTYP_CONNECT_CONFIRM:
      case XTYP_DISCONNECT:
      case XTYP_ERROR:
      case XTYP_REGISTER:
      case XTYP_UNREGISTER:
      case XTYP_XACT_COMPLETE:
        return ((HDDEDATA) FALSE);
        
      case XTYP_MONITOR:
        return ((HDDEDATA) TRUE);
                        
      case XTYP_CONNECT:
/*
        cb = DdeQueryString(idInst,hsz1,(LPSTR) NULL, 0,CP_WINANSI) + 1; 
        pszTopicName = (PSTR) LocalAlloc(LPTR, (UINT) cb); 
        DdeQueryString(idInst,hsz1, pszTopicName,cb,CP_WINANSI); 
        
        cb = DdeQueryString(idInst,hsz2,(LPSTR) NULL, 0,CP_WINANSI) + 1; 
        pszTopicName = (PSTR) LocalAlloc(LPTR, (UINT) cb); 
        DdeQueryString(idInst,hsz2, pszTopicName,cb,CP_WINANSI); 
*/
        DDE_RV.type = RVOID;
        if (hsz2 != hszService)
          { return ((HDDEDATA) FALSE); }
          
        return((HDDEDATA) TRUE);
        
      /*--------------------------------------------------+
      | Get completed command and return result to client |
      +--------------------------------------------------*/
      
      case XTYP_REQUEST:

        if (uFmt != CF_TEXT)
          { return(NULL); }
        
        /* sprintf(theBuffer,"XTYP_REQUEST uFmt = %d\n",(int) uFmt);
        PrintRouter(WDISPLAY,theBuffer); */
       
        hData = NULL;

        if (hsz1 == hszCommand)
          {
           theString = DataObjectToString(GetCurrentEnvironment(),&DDE_RV);

           hData = DdeCreateDataHandle (idInst,
                                        (unsigned char *) theString,
                                        strlen(theString)+1,
                                        0L, hsz2,CF_TEXT,0);
          }
          
        return (hData);
      
      case XTYP_EXECUTE:
        SetFocus(DialogWindow);

        if (CommandLineData(GetCurrentEnvironment())->EvaluatingTopLevelCommand || 
            BatchActive(GetCurrentEnvironment()) )
          { return ((HDDEDATA) DDE_FBUSY  ); }

        Data = (char *) DdeAccessData ( hData, NULL);
        size = strlen((char *) Data) + 1;

        TheData = (char *) genalloc ( GetCurrentEnvironment(),(unsigned) size );
        DdeGetData ( hData, (LPBYTE)TheData, size, 0L );
        
        EnvPrintRouter(theEnv,WPROMPT,TheData);
        EnvPrintRouter(theEnv,WPROMPT,"\n");
        
        EnvEval(theEnv,TheData,&DDE_RV);
        
        if (DDE_RV.type != RVOID)
          {
           PrintDataObject(GetCurrentEnvironment(),"stdout",&DDE_RV);
           EnvPrintRouter(theEnv,"stdout","\n");
          }
        
        PrintPrompt(theEnv);
        
        DdeUnaccessData(hData);
       
        hWnd = FindWindow("ClipsEditWClass", NULL);
        SetFocus (hWnd);
        return ((HDDEDATA) DDE_FACK);

   }
   
   return ( (HDDEDATA) TRUE );
}

/***************************************************************
* StartUpDDE: The function registers an application with the DDEML,
*   creates all strings, registers the service names that a DDE
*   server supports. 
***************************************************************/

BOOL StartUpDDE(void)
  {
   if (DdeInitialize(&idInst, 
                     (PFNCALLBACK) DDECallBack,
                     APPCLASS_STANDARD,
                     0) != DMLERR_NO_ERROR)
     { return(FALSE); }
  
   hszService  = DdeCreateStringHandle(idInst,"CLIPS",CP_WINANSI );
   hszCommand  = DdeCreateStringHandle(idInst,"COMMAND",CP_WINANSI );
   hszResult   = DdeCreateStringHandle(idInst,"RESULT",CP_WINANSI );
   
   if (! DdeNameService(idInst,hszService,0L,DNS_REGISTER))
     { return(FALSE); }
   
   return(TRUE);
  }

/************************************************************
* ShutDownDDE: This function terminates a conversation,
*    invalidates the given conversation handle, and frees
*    string handles in the calling application.
*************************************************************/

void ShutDownDDE ( void )
{  extern HSZ hszService, hszItem, hszComplete, hszBatch, hszLoad;
   extern HCONV hConvApp; /* What does this do? -- Apparently Unused */
   extern DWORD idInst;

   if ( hConvApp != NULL)
   {  DdeDisconnect ( hConvApp );
      hConvApp = NULL;
   }
   DdeNameService (idInst, hszService, (HSZ)NULL, DNS_UNREGISTER );
   DdeFreeStringHandle ( idInst, hszService );
   DdeFreeStringHandle ( idInst, hszCommand );
   DdeFreeStringHandle ( idInst, hszResult    );
}

/***************************************************************
* QuitDDE: This function frees all DDEML resources associated
*   with the calling application and frees the specified function
*   from the data segment bound to it by the StartUpDDE function.
***************************************************************/
#if WIN_BTC
#pragma argsused
#endif
void QuitDDE ( void )
{  
   DdeUninitialize (idInst);
}
