   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                XMAIN HEADER MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bebe Ly                                              */
/*      Daniel J. McCoy                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_xmain
#define _H_xmain

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _XMAIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#ifndef _XMAIN_SOURCE_
   extern Pixmap                   clips_logo;
   extern Widget                   dialog_text;
   extern Arg                      TheArgs[];
   extern XEvent                   TheEvent;
   extern Widget                   toplevel; 
   extern Boolean                  quit_get_event;
   extern Boolean                  Browse_status[];
   extern Widget                   dialog;
   extern Pixmap                   checker;
   extern Boolean                  Dribble_status;
   extern Boolean                  send_to_clips;
   extern XtAppContext             app_con;
   extern KeySym                   TheKeysym;  
   extern XComposeStatus           compose_status;
#endif

#endif
