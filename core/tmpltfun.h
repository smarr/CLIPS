   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  03/04/08            */
   /*                                                     */
   /*          DEFTEMPLATE FUNCTION HEADER FILE           */
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
/*      6.24: Added deftemplate-slot-names,                  */
/*            deftemplate-slot-default-value,                */
/*            deftemplate-slot-cardinality,                  */
/*            deftemplate-slot-allowed-values,               */
/*            deftemplate-slot-range,                        */
/*            deftemplate-slot-types,                        */
/*            deftemplate-slot-multip,                       */
/*            deftemplate-slot-singlep,                      */
/*            deftemplate-slot-existp, and                   */
/*            deftemplate-slot-defaultp functions.           */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Moved default type constants (NO_DEFAULT,      */
/*            STATIC_DEFAULT, and DYNAMIC_DEFAULT) to        */
/*            constant.h                                     */
/*                                                           */
/*************************************************************/

#ifndef _H_tmpltfun

#define _H_tmpltfun

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_factmngr
#include "fact/fact_manager.h"
#endif
#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define DeftemplateSlotNames(a,b) EnvDeftemplateSlotNames(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define DeftemplateSlotDefaultValue(a,b,c) EnvDeftemplateSlotDefaultValue(GetCurrentEnvironment(),getCurrentExecutionState(),a,b,c)
#define DeftemplateSlotCardinality(a,b,c) EnvDeftemplateSlotCardinality(GetCurrentEnvironment(),getCurrentExecutionState(),a,b,c)
#define DeftemplateSlotAllowedValues(a,b,c) EnvDeftemplateSlotAllowedValues(GetCurrentEnvironment(),getCurrentExecutionState(),a,b,c)
#define DeftemplateSlotRange(a,b,c) EnvDeftemplateSlotRange(GetCurrentEnvironment(),getCurrentExecutionState(),a,b,c)
#define DeftemplateSlotTypes(a,b,c) EnvDeftemplateSlotTypes(GetCurrentEnvironment(),getCurrentExecutionState(),a,b,c)
#define DeftemplateSlotMultiP(a,b) EnvDeftemplateSlotMultiP(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define DeftemplateSlotSingleP(a,b) EnvDeftemplateSlotSingleP(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define DeftemplateSlotExistP(a,b) EnvDeftemplateSlotExistP(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define DeftemplateSlotDefaultP(a,b) EnvDeftemplateSlotDefaultP(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)

   LOCALE intBool                        UpdateModifyDuplicate(void *,EXEC_STATUS,struct expr *,char *,void *);
   LOCALE struct expr                   *ModifyParse(void *,EXEC_STATUS,struct expr *,char *);
   LOCALE struct expr                   *DuplicateParse(void *,EXEC_STATUS,struct expr *,char *);
   LOCALE void                           DeftemplateFunctions( void *,EXEC_STATUS);
   LOCALE void                           ModifyCommand(void *,EXEC_STATUS, DATA_OBJECT_PTR);
   LOCALE void                           DuplicateCommand(void *,EXEC_STATUS, DATA_OBJECT_PTR);
   LOCALE void                           DeftemplateSlotNamesFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void                           EnvDeftemplateSlotNames(void *,EXEC_STATUS,void *,DATA_OBJECT *);
   LOCALE void                           DeftemplateSlotDefaultValueFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE intBool                        EnvDeftemplateSlotDefaultValue(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
   LOCALE void                           DeftemplateSlotCardinalityFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void                           EnvDeftemplateSlotCardinality(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
   LOCALE void                           DeftemplateSlotAllowedValuesFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void                           EnvDeftemplateSlotAllowedValues(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
   LOCALE void                           DeftemplateSlotRangeFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void                           EnvDeftemplateSlotRange(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
   LOCALE void                           DeftemplateSlotTypesFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void                           EnvDeftemplateSlotTypes(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
   LOCALE int                            DeftemplateSlotMultiPFunction(void *,EXEC_STATUS);
   LOCALE int                            EnvDeftemplateSlotMultiP(void *,EXEC_STATUS,void *,char *);
   LOCALE int                            DeftemplateSlotSinglePFunction(void *,EXEC_STATUS);
   LOCALE int                            EnvDeftemplateSlotSingleP(void *,EXEC_STATUS,void *,char *);
   LOCALE int                            DeftemplateSlotExistPFunction(void *,EXEC_STATUS);
   LOCALE int                            EnvDeftemplateSlotExistP(void *,EXEC_STATUS,void *,char *);
   LOCALE void                          *DeftemplateSlotDefaultPFunction(void *,EXEC_STATUS);
   LOCALE int                            EnvDeftemplateSlotDefaultP(void *,EXEC_STATUS,void *,char *);
   LOCALE int                            DeftemplateSlotFacetExistPFunction(void *,EXEC_STATUS);
   LOCALE int                            EnvDeftemplateSlotFacetExistP(void *,EXEC_STATUS,void *,char *,char *);
   LOCALE void                           DeftemplateSlotFacetValueFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE int                            EnvDeftemplateSlotFacetValue(void *,EXEC_STATUS,void *,char *,char *,DATA_OBJECT *);

#endif




