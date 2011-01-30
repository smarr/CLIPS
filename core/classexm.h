   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_classexm
#define _H_classexm

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CLASSEXM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if DEBUGGING_FUNCTIONS
LOCALE void BrowseClassesCommand(void);
LOCALE void BrowseClasses(char *,void *);
LOCALE void DescribeClassCommand(void);
LOCALE void DescribeClass(char *,void *);
#endif

LOCALE char *GetCreateAccessorString(void *);

LOCALE SYMBOL_HN *GetDefclassModuleCommand(void);
LOCALE BOOLEAN SuperclassPCommand(void);
LOCALE BOOLEAN SuperclassP(void *,void *);
LOCALE BOOLEAN SubclassPCommand(void);
LOCALE BOOLEAN SubclassP(void *,void *);
LOCALE int SlotExistPCommand(void);
LOCALE BOOLEAN SlotExistP(void *,char *,BOOLEAN);
LOCALE int MessageHandlerExistPCommand(void);
LOCALE BOOLEAN SlotWritablePCommand(void);
LOCALE BOOLEAN SlotWritableP(void *,char *);
LOCALE BOOLEAN SlotInitablePCommand(void);
LOCALE BOOLEAN SlotInitableP(void *,char *);
LOCALE BOOLEAN SlotPublicPCommand(void);
LOCALE BOOLEAN SlotPublicP(void *,char *);
LOCALE BOOLEAN SlotDirectAccessPCommand(void);
LOCALE BOOLEAN SlotDirectAccessP(void *,char *);
LOCALE void SlotDefaultValueCommand(DATA_OBJECT_PTR);
LOCALE BOOLEAN SlotDefaultValue(void *,char *,DATA_OBJECT_PTR);
LOCALE int ClassExistPCommand(void);

#ifndef _CLASSEXM_SOURCE_
#endif

#endif
