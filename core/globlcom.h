   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
  /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            DEFGLOBAL COMMANDS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_globlcom
#define _H_globlcom

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GLOBLCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DefglobalCommandDefinitions(void);
   LOCALE int                            SetResetGlobalsCommand(void);
   LOCALE BOOLEAN                        SetResetGlobals(int);
   LOCALE int                            GetResetGlobalsCommand(void);
   LOCALE BOOLEAN                        GetResetGlobals(void);
   LOCALE void                           ShowDefglobalsCommand(void);
   LOCALE void                           ShowDefglobals(char *,void *);

#endif


