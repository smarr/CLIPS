   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  12/13/99            */
   /*                                                     */
   /*              CRC Generator HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*   provides and interface to calculate the CRC of a given  */
/*   input file, from the starting position. Returns the     */
/*   file pointer back to the originating position when 	 */
/*   complete.                                               */ 
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Mark D. Tomlinson                                    */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_crc_clips
#define _H_crc_clips

#ifndef _H_symbol
#include "symbol.h"
#endif

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   LOCALE unsigned long int CalcCRC(void *);

#endif /* _H_crc_clips */



