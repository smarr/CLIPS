   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

   LOCALE void                           IOFunctionDefinitions(void);
#if BASIC_IO
   LOCALE void                           PrintoutFunction(void);
   LOCALE void                           ReadFunction(DATA_OBJECT_PTR);
   LOCALE int                            OpenFunction(void);
   LOCALE int                            CloseFunction(void);
#endif
#if EXT_IO
   LOCALE void                           ReadlineFunction(DATA_OBJECT_PTR);
   LOCALE void                          *FormatFunction(void);
   LOCALE int                            RemoveFunction(void);
   LOCALE int                            RenameFunction(void);
#endif

#endif






