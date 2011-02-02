   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  07/01/05            */
   /*                                                     */
   /*               I/O FUNCTIONS HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.24: Added the get-char function.                   */
/*                                                           */
/*            Moved IllegalLogicalNameMessage function to    */
/*            argacces.c.                                    */
/*                                                           */
/*************************************************************/

#ifndef _H_iofun

#define _H_iofun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _IOFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           IOFunctionDefinitions(void *,EXEC_STATUS);
#if IO_FUNCTIONS
   LOCALE intBool                        SetFullCRLF(void *,EXEC_STATUS,intBool);
   LOCALE void                           PrintoutFunction(void *,EXEC_STATUS);
   LOCALE void                           ReadFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE int                            OpenFunction(void *,EXEC_STATUS);
   LOCALE int                            CloseFunction(void *,EXEC_STATUS);
   LOCALE int                            GetCharFunction(void *,EXEC_STATUS);
   LOCALE void                           PutCharFunction(void *,EXEC_STATUS);
   LOCALE void                           ReadlineFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                          *FormatFunction(void *,EXEC_STATUS);
   LOCALE int                            RemoveFunction(void *,EXEC_STATUS);
   LOCALE int                            RenameFunction(void *,EXEC_STATUS);
   LOCALE void                           SetLocaleFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                           ReadNumberFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
#endif

#endif






