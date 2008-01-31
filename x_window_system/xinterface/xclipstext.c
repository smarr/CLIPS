   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                  XCLIPSTEXT MODULE                  */
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

#define _XCLIPSTEXT_SOURCE_

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <X11/Xlib.h>

#include <X11/Xaw/Text.h>

#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/Misc.h>

#ifdef XAW3D
#include <X11/Xaw3d/TextP.h>
#else
#include <X11/Xaw/TextP.h>
#endif

#include "setup.h"

#include "commline.h"
#include "evaluatn.h"
#include "filertr.h"
#include "router.h"

#include "xclips.h"
#include "xedit.h"
#include "xmenu_file.h"
#include "xmenu_exec.h"
#include "xmenu_wind.h"
#include "xclipstext.h"
#include "xmain.h"

static void StartAction(TextWidget,XEvent *);
static void EndAction(TextWidget);
static void StuffFromBuffer(TextWidget,int);
static void _SelectionReceived(Widget,caddr_t,Atom *,Atom *,caddr_t,unsigned long *,int *);
static void _ClipsSelectionReceived(Widget,caddr_t,Atom *,Atom *,caddr_t,unsigned long *,int *);
static void GetSelection(Widget,Time,String *,Cardinal);
static void ClipsGetSelection(Widget,Time,String *,Cardinal);
static void InsertClipsSelection(Widget,XEvent *,String *,Cardinal *);
static void Move(TextWidget,XEvent *,XawTextScanDirection,XawTextScanType,Boolean);
static void _DeleteOrKill(TextWidget,XawTextPosition, XawTextPosition,Boolean);
static void DeleteOrKill(TextWidget,XEvent *,XawTextScanDirection,XawTextScanType,Boolean,Boolean);
static void DeleteClipsBackwardChar(Widget,XEvent *);
static void InsertClipsNewLine(Widget,XEvent *);
static void AutoFill(TextWidget);
static void InsertClipsChar(Widget,XEvent *);
static void Clear_CLIPS(Widget,XEvent *);
static void IntReset(Widget,XEvent *);
static void FactsWindow(Widget,XEvent *);
static void AgendaWindow(Widget,XEvent *);
static void LoadRulesProc(Widget,XEvent *);
static void Dribble(Widget,XEvent *);
static void QuitProc(Widget,XEvent *);
static void IntRun(Widget,XEvent *);
static void SaveRules(Widget,XEvent *);
static void Step(Widget,XEvent *);
static void Edit(Widget,XEvent *);
static void CommandLineCLIPS(Widget,XEvent *);
static void FindBalance(Widget,XEvent *);
static void CompleteConstructInDialog(Widget,XEvent *);
static void CompleteConstructInEditor(Widget,XEvent *);
static void ClearScreen(Widget,XEvent *);
static void StopExecution(Widget,XEvent *);


#define SrcScan                XawTextSourceScan
#define FindDist               XawTextSinkFindDistance
#define FindPos                XawTextSinkFindPosition

/*
 *  These are defined in xclips.c
 */


static void StartAction(
  TextWidget ctx,
  XEvent *event)
  {
  _XawTextPrepareToUpdate(ctx);
  if (event != NULL) {
    switch (event->type) {
    case ButtonPress:
    case ButtonRelease:
      ctx->text.time = event->xbutton.time;
      ctx->text.ev_x = event->xbutton.x;
      ctx->text.ev_y = event->xbutton.y;
      break;
    case KeyPress:
    case KeyRelease:
      ctx->text.time = event->xkey.time;
      ctx->text.ev_x = event->xkey.x;
      ctx->text.ev_y = event->xkey.y;
      break;
    case MotionNotify:
      ctx->text.time = event->xmotion.time;
      ctx->text.ev_x = event->xmotion.x;
      ctx->text.ev_y = event->xmotion.y;
      break;
    case EnterNotify:
    case LeaveNotify:
      ctx->text.time = event->xcrossing.time;
      ctx->text.ev_x = event->xcrossing.x;
      ctx->text.ev_y = event->xcrossing.y;
    }
  }
  }

static void EndAction(
  TextWidget ctx)
  {
  _XawTextCheckResize(ctx);
  _XawTextExecuteUpdate(ctx);
  ctx->text.mult = 1;
  }



/*
 * These functions are superceeded by insert-selection.
 */

static void StuffFromBuffer(
  TextWidget ctx,
  int buffer)
  {
  XawTextBlock text;
  text.ptr = XFetchBuffer(XtDisplay(ctx), &(text.length), buffer);
  text.firstPos = 0;
  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) {
    XBell(XtDisplay(ctx), 0);
    return;
  }
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos,
				XawstPositions, XawsdRight, text.length, TRUE);
  XtFree(text.ptr);
  }

void UnKill(
  TextWidget ctx,
  XEvent *event)
  {
  StartAction(ctx, event);
  StuffFromBuffer(ctx, 1);
  EndAction(ctx);
  }

void Stuff(
  TextWidget ctx,
  XEvent *event)
  {
  StartAction(ctx, event);
  StuffFromBuffer(ctx, 0);
  EndAction(ctx);
  }


struct _SelectionList
  {
  String *params;
  Cardinal count;
  Time time;
  };

/* ARGSUSED */
static void _SelectionReceived(
  Widget w,
  caddr_t client_data,
  Atom *selection,
  Atom *type,
  caddr_t value,
  unsigned long *length,
  int *format)
{
  TextWidget ctx = (TextWidget)w;
  XawTextBlock text;

  if (*type == 0 /*XT_CONVERT_FAIL*/ || *length == 0) {
    struct _SelectionList* list = (struct _SelectionList*)client_data;
    if (list != NULL) {
      GetSelection(w, list->time, list->params, list->count);
      XtFree(client_data);
    }
    return;
  }

  StartAction(ctx, NULL);
  text.ptr = (char*)value;
  text.firstPos = 0;
  text.length = *length;
  text.format = FMT8BIT;
  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) {
    XBell(XtDisplay(ctx), 0);
    return;
  }
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos,
                                XawstPositions, XawsdRight, text.length, TRUE);

  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  XtFree(client_data);
  XtFree(value);
}

static void _ClipsSelectionReceived(
  Widget w,
  caddr_t client_data,
  Atom *selection,
  Atom *type,
  caddr_t value,
  unsigned long *length,
  int *format)
  {
  TextWidget ctx = (TextWidget)w;
  XawTextBlock text;
  char *cmdstr;

  if (*type == 0 /*XT_CONVERT_FAIL*/ || *length == 0) {
    struct _SelectionList* list = (struct _SelectionList*)client_data;
    if (list != NULL) {
      ClipsGetSelection(w, list->time, list->params, list->count);
      XtFree(client_data);
    }
      return;
  }
  StartAction(ctx, NULL);
  text.ptr = (char*)value;
  if(send_to_clips)
   {
       cmdstr = GetCommandString(GetCurrentEnvironment());
       if(cmdstr == NULL)
         {
           SetCommandString(GetCurrentEnvironment(),text.ptr);
         }
       else
         {
           AppendCommandString(GetCurrentEnvironment(),text.ptr);
          }
       send_to_clips = False;
   }
  text.firstPos = 0;
  text.length = *length;
  text.format = FMT8BIT;
  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) {
    XBell(XtDisplay(ctx), 0);
    return;
  }
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, 
				XawstPositions, XawsdRight, text.length, TRUE);

  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  XtFree(client_data);
  XtFree(value);
}

static void GetSelection(
  Widget w,
  Time time,
  String *params,                 /* selections in precedence order */
  Cardinal num_params)
{
    Atom selection;
    int buffer;

    XmuInternStrings(XtDisplay(w), params, (Cardinal)1, &selection);
    switch (selection) {
      case XA_CUT_BUFFER0: buffer = 0; break;
      case XA_CUT_BUFFER1: buffer = 1; break;
      case XA_CUT_BUFFER2: buffer = 2; break;
      case XA_CUT_BUFFER3: buffer = 3; break;
      case XA_CUT_BUFFER4: buffer = 4; break;
      case XA_CUT_BUFFER5: buffer = 5; break;
      case XA_CUT_BUFFER6: buffer = 6; break;
      case XA_CUT_BUFFER7: buffer = 7; break;
      default:         buffer = -1;
    }
    if (buffer >= 0) {
        int nbytes;
        unsigned long length;
        int fmt8 = 8;
        Atom type = XA_STRING;
        char *line = XFetchBuffer(XtDisplay(w), &nbytes, buffer);
        if ((length = nbytes) != 0)
            _SelectionReceived(w, NULL, &selection, &type, (caddr_t)line,
                               &length, &fmt8);
        else if (num_params > 1)
            GetSelection(w, time, params+1, num_params-1);
    } else {
        struct _SelectionList* list;
        if (--num_params) {
            list = XtNew(struct _SelectionList);
            list->params = params + 1;
            list->count = num_params;
            list->time = time;
        } else list = NULL;
        XtGetSelectionValue(w, selection, XA_STRING,
                            (XtSelectionCallbackProc)_SelectionReceived,
                            (XtPointer)list, time);
    }
}

static void ClipsGetSelection(
  Widget w,
  Time time,
  String *params,			/* selections in precedence order */
  Cardinal num_params)
{
    Atom selection;
    int buffer;
   
/*    XmuInternStrings(XtDisplay(w), params, (Cardinal)1, &selection);*/
    selection = XInternAtom(XtDisplay(w), *params, False);
    switch (selection) {
      case XA_CUT_BUFFER0: buffer = 0; break;
      case XA_CUT_BUFFER1: buffer = 1; break;
      case XA_CUT_BUFFER2: buffer = 2; break;
      case XA_CUT_BUFFER3: buffer = 3; break;
      case XA_CUT_BUFFER4: buffer = 4; break;
      case XA_CUT_BUFFER5: buffer = 5; break;
      case XA_CUT_BUFFER6: buffer = 6; break;
      case XA_CUT_BUFFER7: buffer = 7; break;
      default:	       buffer = -1;
    }
    if (buffer >= 0) {
	int nbytes;
	unsigned long length;
	int fmt8 = 8;
	Atom type = XA_STRING;
	char *line = XFetchBuffer(XtDisplay(w), &nbytes, buffer);
	if ((length = nbytes) != 0)
	    _ClipsSelectionReceived(w, NULL, &selection, &type, (caddr_t)line,
			       &length, &fmt8);
	else if (num_params > 1)
	    ClipsGetSelection(w, time, params+1, num_params-1);
    } else {
	struct _SelectionList* list;

	if (--num_params) {
	    list = (struct _SelectionList*)XtNew(struct _SelectionList);
	    list->params = params + 1;
	    list->count = num_params;
	    list->time = time;
	} else list = NULL;
	XtGetSelectionValue(w, selection, XA_STRING, 
                            (XtSelectionCallbackProc)_ClipsSelectionReceived,
			    (XtPointer)list, time);
    }
}

static void InsertClipsSelection(
  Widget w,
  XEvent *event,
  String *params,		
  Cardinal *num_params)
  {
  MoveEndOfFile(dialog_text,event);
  StartAction((TextWidget) w, event); /* Get Time. */
  ClipsGetSelection(w, ((TextWidget) w)->text.time, params, *num_params);
  EndAction((TextWidget) w);
  }

/************************************************************
 *
 * Routines for Moving Around.
 *
 ************************************************************/

static void Move(
  TextWidget ctx,
  XEvent *event,
  XawTextScanDirection dir,
  XawTextScanType type,
  Boolean include)
  {
  StartAction(ctx, event);
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, type, dir, ctx->text.mult, include);
  EndAction(ctx);
  }

void MoveBeginningOfFile(
  Widget w,
  XEvent *event)
  {
  Move((TextWidget) w, event, XawsdLeft, XawstAll, TRUE);
  }

void MoveEndOfFile(
  Widget w,
  XEvent *event)
  {
  Move((TextWidget) w, event, XawsdRight, XawstAll, TRUE);
  }

/************************************************************
 *
 * Delete Routines.
 *
 ************************************************************/

static void _DeleteOrKill(
  TextWidget ctx,
  XawTextPosition from, 
  XawTextPosition to,
  Boolean	kill)
  {
  XawTextBlock text;
  char *ptr;
  
  if (kill && from < to)
    {
    ptr = (char *) _XawTextGetText(ctx, from, to);
    XStoreBuffer(XtDisplay(ctx), ptr, strlen(ptr), 1);
    XtFree(ptr);
    }
  text.length = 0;
  text.firstPos = 0;
  if (_XawTextReplace(ctx, from, to, &text))
    {
    XBell(XtDisplay(ctx), 50);
    return;
    }
  ctx->text.insertPos = from;
  ctx->text.showposition = TRUE; 
  }

static void DeleteOrKill(
  TextWidget ctx,
  XEvent *event,
  XawTextScanDirection dir,
  XawTextScanType type,
  Boolean include,
  Boolean kill)
  {
  XawTextPosition from, to;
  
  StartAction(ctx, event);
  to = SrcScan(ctx->text.source, ctx->text.insertPos, type, dir, ctx->text.mult, include);
  
  if (dir == XawsdLeft)
    {
    from = to;
    to = ctx->text.insertPos;
    }
  else 
    from = ctx->text.insertPos;

  _DeleteOrKill(ctx, from, to, kill);
  _XawTextSetScrollBars(ctx);
  EndAction(ctx);
  }

static void DeleteClipsBackwardChar(
  Widget w,
  XEvent *event)
  {
  TextWidget ctx = (TextWidget)w;
  char *cmdstr,strbuf[2];

  MoveEndOfFile(w,event);
  strbuf[1] = 0;
  if(RouterData(GetCurrentEnvironment())->CommandBufferInputCount == 0)
    return;
  if((!quit_get_event)&&(get_clips_command())&& (!GetManagerList()))
   {
      cmdstr = GetCommandString(GetCurrentEnvironment());
      if((cmdstr != NULL) ? (cmdstr[0] != EOS) :FALSE)
       {
        strbuf[0] = (char) XK_BackSpace;
        ExpandCommandString(GetCurrentEnvironment(),strbuf[0]);
      }
   }
  DeleteOrKill(ctx, event, XawsdLeft, XawstPositions, TRUE, FALSE);
  }

void DeleteCurrentSelection(
  Widget w,
  XEvent *event)
  {
  _XawTextZapSelection( (TextWidget) w, event, FALSE);
  }

/************************************************************
 *
 * Insertion Routines.
 *
 ************************************************************/

static int InsertNewLineAndBackupInternal(
  TextWidget ctx)
  {
  int count, error = XawEditDone;
  XawTextBlock text;
  char *buf, *ptr;

  ptr = buf = XtMalloc(sizeof(char) * ctx->text.mult);
  for (count = 0; count < ctx->text.mult; count++, ptr++)
    ptr[0] = '\n';

  text.length = ctx->text.mult;
  text.ptr = buf;
  text.firstPos = 0;
  text.format = FMT8BIT;

  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text))
    {
    XBell( XtDisplay(ctx), 50);
    error = XawEditError;
    }
  else 
    ctx->text.showposition = TRUE;

  XtFree(buf);
  return(error);
  }

int LocalClipsInsertNewLine(
  TextWidget ctx,
  XEvent *event)
  {

  StartAction(ctx, event);
  if (InsertNewLineAndBackupInternal(ctx) == XawEditError)
    return(XawEditError);
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, ctx->text.mult, TRUE);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  return(XawEditDone);
  }

static void InsertClipsNewLine(
  Widget w,
  XEvent *event)
  {
  TextWidget ctx = (TextWidget)w;
  char strbuf[2];
  strbuf[1] = 0;

  MoveEndOfFile(w,event);
  if((!quit_get_event)&&(get_clips_command())&& (!GetManagerList()))
    {
      strbuf[0] = (char) XK_Linefeed;
      ExpandCommandString(GetCurrentEnvironment(),strbuf[0]);
      quit_get_event = True;
    }
  (void)LocalClipsInsertNewLine(ctx,event);
  }

/************************************************************
 *
 * Misc. Routines.
 *
 ************************************************************/

XComposeStatus compose_status = {NULL, 0};

/*	Function Name: AutoFill
 *	Description: Breaks the line at the previous word boundry when
 *                   called inside InsertChar.
 *	Arguments: ctx - The text widget.
 *	Returns: none
 */

static void AutoFill(
  TextWidget ctx)
  {
  int width, height, x, line_num, max_width;
  XawTextPosition ret_pos;
  XawTextBlock text;

  if ( !((ctx->text.auto_fill) && (ctx->text.mult == 1)) )
    return;

  for ( line_num = 0; line_num < ctx->text.lt.lines ; line_num++)
    if ( ctx->text.lt.info[line_num].position >= ctx->text.insertPos )
      break;
  line_num--;			/* backup a line. */

  max_width = Max(0, ctx->core.width - HMargins(ctx));

  x = ctx->text.margin.left;
  XawTextSinkFindPosition( ctx->text.sink,ctx->text.lt.info[line_num].position, x, max_width, TRUE, &ret_pos, &width, &height);
  
  if ( ret_pos >= ctx->text.insertPos )
    return;
  
  text.ptr = "\n";
  text.length = 1;
  text.firstPos = 0;
  text.format = FMT8BIT;

  _XawTextReplace(ctx, ret_pos - 1, ret_pos, &text);
  }

static void InsertClipsChar(
  Widget w,
  XEvent *event)
  {
  TextWidget ctx = (TextWidget) w;
  char *ptr, strbuf[BUFSIZ];
  int count, error;
  KeySym keysym;
  XawTextBlock text;
  
  MoveEndOfFile(w, event);
  if ( (text.length = XLookupString (&event->xkey, strbuf, BUFSIZ, &keysym, &compose_status)) == 0)
    return;
  if((!quit_get_event)&&(get_clips_command())&& (!GetManagerList()))
   {
        strbuf[1] = 0;
        if((keysym>= XK_space) && (keysym<= XK_asciitilde))
          {
          ExpandCommandString(GetCurrentEnvironment(),strbuf[0]);
          }
    }
  else
    return;
  text.ptr = ptr = XtMalloc(sizeof(char) * text.length * ctx->text.mult);
  for (count = 0 ; count < ctx->text.mult ; count++)
    {
    strncpy(ptr, strbuf, text.length);
    ptr += text.length;
    }

  text.length = text.length * ctx->text.mult;
  text.firstPos = 0;
  text.format = FMT8BIT;
  
  StartAction(ctx, event);
  
  error = _XawTextReplace(ctx, ctx->text.insertPos,ctx->text.insertPos, &text);

  if (error == XawEditDone)
    {
    ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, text.length, TRUE);
    AutoFill(ctx);
    }
  else 
    XBell(XtDisplay(ctx), 50);

  XtFree(text.ptr);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  }

/*ARGSUSED*/
void InsertClipsString(
  Widget w,
  XEvent *event,
  String *params,
  Cardinal *num_params)
  {
  TextWidget ctx = (TextWidget) w;
  XawTextBlock text;
  int	   i;
  
  text.firstPos = 0;
  StartAction(ctx, event);
  for (i = *num_params; i; i--, params++) 
    {
    unsigned char hexval;
    if ((*params)[0] == '0' && (*params)[1] == 'x' && (*params)[2] != '\0') 
      {
      char c, *p;
      hexval = 0;
      for (p = *params+2; (c = *p); p++) 
        {
        hexval *= 16;
        if (c >= '0' && c <= '9')
          hexval += c - '0';
        else if (c >= 'a' && c <= 'f')
          hexval += c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
          hexval += c - 'A' + 10;
        else break;
        }
      if (c == '\0') 
        {
        text.ptr = (char*)&hexval;
        text.length = 1;
        } 
      else 
        text.length = strlen(text.ptr = *params);
      } 
    else 
      text.length = strlen(text.ptr = *params);
    if (text.length == 0) 
      continue;
    if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) 
      {
      XBell(XtDisplay(ctx), 50);
      EndAction(ctx);
      return;
      }
    ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, text.length, TRUE);
    }
  EndAction(ctx);

  }
	
/* CLIPS key bound functions for the menus */

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/
static void Clear_CLIPS(
  Widget w,
  XEvent *event)
  {
  ClearCLIPSCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void IntReset(
  Widget w,
  XEvent *event)
  {
  ResetCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void FactsWindow(
  Widget w,
  XEvent *event)
  {
  FactsWindowCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void AgendaWindow(
  Widget w,
  XEvent *event)
  {
  AgendaWindowCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }
  
/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void LoadRulesProc(
  Widget w,
  XEvent *event)
  {
  LoadRulesCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void Dribble(
  Widget w,
  XEvent *event)
  {
  DribbleCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void QuitProc(
  Widget w,
  XEvent *event)
  {
  QuitCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void IntRun(
  Widget w,
  XEvent *event)
  {
  RunCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void SaveRules(
  Widget w,
  XEvent *event)
  {
  SaveRulesCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void Step(
  Widget w,
  XEvent *event)
  {
  StepCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void Edit(
  Widget w,
  XEvent *event)
  {
  EditCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void CommandLineCLIPS(
  Widget w,
  XEvent *event)
  {
  CommandLineCLIPSCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void FindBalance(
  Widget w,
  XEvent *event)
  {
    FindMatchingParenthesisCallback(w,(XtPointer)w,(XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void CompleteConstructInDialog(
  Widget w,
  XEvent *event)
  {
       CompletionDialogCallback(w,NULL,(XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void CompleteConstructInEditor(
  Widget w,
  XEvent *event)
  {
       CompletionEditCallback(w,(XtPointer)w,(XtPointer)NULL);
  }


/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void ClearScreen(
  Widget w,
  XEvent *event)
  {
    ClearScreenCallback(w,(XtPointer)w,(XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void StopExecution(
  Widget w,
  XEvent *event)
  {

    if(periodicChecking)
     {
      SetHaltExecution(GetCurrentEnvironment(),TRUE);
      CloseAllFiles(GetCurrentEnvironment());
     }
  }

/*****************************************************************************
*  Action Table
******************************************************************************
*/


XtActionsRec ClipsTxtActsTable[] =
  {
/* delete bindings */
  {"delete-clips-previous-character",  (XtActionProc)DeleteClipsBackwardChar},
/* new line stuff */
  {"Clipsnewline",              (XtActionProc)InsertClipsNewLine},
/* Selection stuff */
  {"insert-clips-selection",    (XtActionProc)InsertClipsSelection},
/* Miscellaneous */
  {"insert-clips-string",       (XtActionProc)InsertClipsString},
  {"insert-clips-char",         (XtActionProc)InsertClipsChar},
/* CLIPS Dialog Window key bindings for menus */
  {"clear-clips",               (XtActionProc)Clear_CLIPS},
  {"reset",                     (XtActionProc)IntReset},
  {"facts-window",              (XtActionProc)FactsWindow},
  {"agenda-window",             (XtActionProc)AgendaWindow},
  {"load-constructs",           (XtActionProc)LoadRulesProc},
  {"dribble",                   (XtActionProc)Dribble},
  {"quit",                      (XtActionProc)QuitProc},
  {"run",                       (XtActionProc)IntRun},
  {"save-rules",                (XtActionProc)SaveRules},
  {"step",                      (XtActionProc)Step},
  {"edit",                      (XtActionProc)Edit},
  {"command-line-clips",        (XtActionProc)CommandLineCLIPS},
  {"balance",                   (XtActionProc)FindBalance},
  {"clear-screen",              (XtActionProc)ClearScreen},
  {"complete-construct-dialog", (XtActionProc)CompleteConstructInDialog},
  {"complete-construct-editor", (XtActionProc)CompleteConstructInEditor},
  {"stop-execution",            (XtActionProc)StopExecution},
  };

Cardinal ClipsTxtActsTableCount = XtNumber(ClipsTxtActsTable);

