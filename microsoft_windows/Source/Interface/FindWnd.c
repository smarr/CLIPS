   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                 FIND WINDOW MODULE                  */
   /*******************************************************/

/**************************************************************/
/* Purpose:                                                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

#include <windows.h>       
#include "FindWnd.h"
#include "resource.h"

/****************************************************************/
/* findWindowMenu: The "Window" menu is the menu which contains */
/*   the ID_WINDOW_CASCADE menu item.  If you have decided to   */
/*   disallow this capability by removing the "Cascade" menu    */
/*   item, this will have to be rewritten.                      */
/****************************************************************/
HMENU findWindowMenu(
  HMENU hMenu)
  {
   int count = GetMenuItemCount(hMenu);
   int i;

   for (i = 0; i < count; i++)
     { 
      HMENU tmenu = GetSubMenu(hMenu, i);
      
      if (GetMenuState(tmenu,ID_WINDOW_CASCADE,MF_BYCOMMAND) != (UINT)-1)
        { return tmenu; } 
     } 
         
   return NULL;
  }
