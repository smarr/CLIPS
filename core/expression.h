/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version X.YY  DD/MM/YY            */
/*                                                     */
/*               EXPRESSION HEADER FILE                */
/*******************************************************/

#ifndef _H_expression

#define _H_expression

struct expr;

struct expr
{
  unsigned short type;
  void *value;
  struct expr *argList;
  struct expr *nextArg;
  long refCount;   // STEFAN: added support for refCount on expression lists
};

#endif
