/*   CLIPS Version 6.10  04/09/97 */

/*
 * This program is in public domain; written by Dave G. Conroy.
 * This file contains the main driving routine, and some keyboard processing
 * code, for the MicroEMACS screen editor.
 *
 * REVISION HISTORY:
 *
 * 1.0  Steve Wilhite, 30-Nov-85
 *      - Removed the old LK201 and VT100 logic. Added code to support the
 *        DEC Rainbow keyboard (which is a LK201 layout) using the the Level
 *        1 Console In ROM INT.
 *        See "rainbow.h" for the function key definitions.
 *
 * 2.0  George Jones, 12-Dec-85
 *      - Ported to Amiga.
 *
 * 2.1  Chris Culbert, 25-Jul-86
 *      - Ported to HP9000 computers and modified commands to look more
 *        like the Zmacs editor used on the Symbolics.
 *
 * 2.2  Bebe Ly, 09-Jan-87
 *      - Added functions for global search and replace, query search
 *        and replace, and parenthesis matching.
 *
 *
 * 3.0  Chris Culbert, August 1987
 *      - Integrated with CLIPS tool. Added functions to do rule
 *        compiling and editor entry and exit clean up.
 *        Massive rearranging of code and general clean up.
 */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */

#include "setup.h"

#if     EMACS_EDITOR
#if     ! RUN_TIME

#define _EDMAIN_SOURCE_
#include "ed.h"
#include "sysdep.h"
#include "extnfunc.h"

#if     VAX_VMS
#include        <ssdef.h>
#define GOOD    (SS$_NORMAL)
#endif

#ifndef GOOD
#define GOOD    0
#endif

#define EXIT	-999

Thread globle int     currow;                  /* Working cursor row           */
Thread globle int     curcol;                  /* Working cursor column        */
Thread globle int     fillcol;                 /* Current fill column          */
Thread globle int     thisflag;                /* Flags, this command          */
Thread globle int     lastflag;                /* Flags, last command          */
Thread globle int     curgoal;                 /* Goal column                  */
Thread globle BUFFER  *curbp;                  /* Current buffer               */
Thread globle WINDOW  *curwp;                  /* Current window               */
Thread globle BUFFER  *bheadp = NULL;          /* BUFFER listhead              */
Thread globle WINDOW  *wheadp;                 /* WINDOW listhead              */
Thread globle BUFFER  *blistp;                 /* Buffer list BUFFER           */
Thread globle short   kbdm[NKBDM] = {CTLX|')'};/* Macro                        */
Thread globle short   *kbdmip;                 /* Input  for above             */
Thread globle short   *kbdmop;                 /* Output for above             */
Thread globle char    pat[NPAT];               /* Pattern                      */
Thread globle char    lastbufn[NBUFN];         /* Last buffer name             */
Thread globle BUFFER *CompileBufferp;          /* CLIPS Compile Output Buffer  */


typedef struct  {
        short   k_code;                 /* Key code                     */
        int     (*k_fp)(int,int);       /* Routine to handle it         */
}       KEYTAB;


/*
 * Command table.
 * This table  is *roughly* in ASCII order, left to right across the
 * characters of the command. This expains the funny location of the
 * control-X commands.
 */
Thread globle KEYTAB keytab[] = {
        { COTL|'@',setmark },
        { COTL|'A',gotobol },
        { COTL|'B',backchar },
        { COTL|'C',spawncli }, /* Run CLI in subjob.   */
        { COTL|'D',forwdel },
        { COTL|'E',gotoeol },
        { COTL|'F',forwchar },
        { COTL|'G',ctrlg },
        { COTL|'H',backdel },
        { COTL|'I',tab },
        { COTL|'J',indent },
        { COTL|'K',kill_fwd },
        { COTL|'L',EditorRefresh },
        { COTL|'M',newline },
        { COTL|'N',forwline },
        { COTL|'O',openline },
        { COTL|'P',backline },
        { COTL|'Q',quote }, /* Often unreachable    */
        { COTL|'R',backsearch },
        { COTL|'S',forwsearch }, /* Often unreachable    */
        { COTL|'T',twiddle },
        { COTL|'V',forwpage },
        { COTL|'W',killregion },
        { COTL|'Y',yank },
        { COTL|'Z',quickexit }, /* quick save and exit  */
        { CTLX|COTL|'B',listbuffers },
        { CTLX|COTL|'C',edquit }, /* Hard quit.           */
        { CTLX|COTL|'F',filevisit },
        { CTLX|COTL|'L',lowerregion },
        { CTLX|COTL|'O',deblank },
        { CTLX|COTL|'N',mvdnwind },
        { CTLX|COTL|'P',mvupwind },
        { CTLX|COTL|'R',filename },
        { CTLX|COTL|'S',filesave }, /* Often unreachable    */
        { CTLX|COTL|'T',compile_region },
        { CTLX|COTL|'U',upperregion },
        { CTLX|COTL|'V',fileread },
        { CTLX|COTL|'W',filewrite },
        { CTLX|COTL|'X',swapmark },
        { CTLX|COTL|'Z',shrinkwind },
        { CTLX|'!',spawn }, /* Run 1 command.       */
        { CTLX|'=',showcpos },
        { CTLX|':',gotoline },
        { CTLX|'(',ctlxlp },
        { CTLX|')',ctlxrp },
        { CTLX|'1',onlywind },
        { CTLX|'2',splitwind },
        { CTLX|'B',usebuffer },
        { CTLX|'E',ctlxe },
        { CTLX|'F',setfillcol },
        { CTLX|'K',killbuffer },
        { CTLX|'M',smatchb },
        { CTLX|'N',nextwind },
        { CTLX|'P',prevwind },
        { CTLX|'Q',temp_quit },
        { CTLX|'R',bkwrdrpl },
        { CTLX|'S',frwsr },
        { CTLX|'Z',enlargewind },
        { META|COTL|'H',delbword },
        { META|'!',reposition },
        { META|'.',setmark },
        { META|'>',gotoeob },
        { META|'<',gotobob },
        { META|'B',backword },
        { META|'C',capword },
        { META|'D',delfword },
        { META|'F',forwword },
        { META|'J',forwsearch }, /* To replace C-S */
        { META|'L',lowerword },
        { META|'R',bkwrdcr },
        { META|'S',querysr },
        { META|'T',compile_file },
        { META|'U',upperword },
        { META|'V',backpage },
        { META|'W',copyregion },
        { META|'Z',filesave }, /* To replace C-S */
        { META|DEL_KEY,delbword },
        { DEL_KEY,backdel }
};

#define NKEYTAB (sizeof(keytab)/sizeof(keytab[0]))

static VOID PerformEditCommand(void);

static VOID PerformEditCommand()
{
        register int    c;
        register int    f;
        register int    n;
        register int    mflag;
        register int    rtn_flag;
        char            bname[NBUFN];
        int num_a;
        char *fileName = NULL;
        DATA_OBJECT arg_ptr;

   /*====================*/
   /* Get the file name. */
   /*====================*/

   if ((num_a = ArgCountCheck("edit",NO_MORE_THAN,1)) == -1) return;

   if (num_a == 1)
     {
      if (ArgTypeCheck("edit",1,SYMBOL_OR_STRING,&arg_ptr) == FALSE) return;
      fileName = DOToString(arg_ptr);
     }

   if(bheadp == NULL) {

	/**********************************************/
	/* Initial entry, set up buffers and pointers */
	/**********************************************/

        strcpy(bname, "main");                  /* Work out the name of */
        if (num_a > 0)                     /* the default buffer.  */
                makename(bname,fileName);
        edinit(bname);                          /* Buffers, windows.    */
        vtinit();                               /* Displays.            */
        if (num_a > 0) {
                update();                       /* You have to update   */
                readin(fileName);             /* in case "[New file]" */
                }

	init_cmp_router();			/* Prepare the compile  */
        DeactivateRouter("cmp_router");		/* router.              */
        }
   else {

	/**********************************************************/
	/* Return from temporary exit, reset necessary stuff only */
	/**********************************************************/

	(*term.t_open)();

        if (num_a > 0) {
           filevisit_guts(fileName);
           }
        }

   sgarbf = TRUE;                          /* Force screen update  */
   lastbufn[0] = '\0';                     /* Make sure last name  */
                                           /* is cleared out       */

   lastflag = 0;                           /* Fake last flags.     */
loop:
        update();                               /* Fix up the screen    */
        c = getkey();
        if (mpresf != FALSE) {
                mlerase();
                update();
                if (c == ' ')                   /* ITS EMACS does this  */
                        goto loop;
        }
        f = FALSE;
        n = 1;
        if (c == (COTL|'U')) {                  /* ^U, start argument   */
                f = TRUE;
                n = 4;                          /* with argument of 4 */
                mflag = 0;                      /* that can be discarded. */
                mlwrite("Arg: 4");
                while ((((c=getkey()) >='0') && (c<='9'))
                       || (c==(COTL|'U')) || (c=='-')){
                        if (c == (COTL|'U'))
                                n = n*4;
                        /*
                         * If dash, and start of argument string, set arg.
                         * to -1.  Otherwise, insert it.
                         */
                        else if (c == '-') {
                                if (mflag)
                                        break;
                                n = 0;
                                mflag = -1;
                        }
                        /*
                         * If first digit entered, replace previous argument
                         * with digit and set sign.  Otherwise, append to arg.
                         */
                        else {
                                if (!mflag) {
                                        n = 0;
                                        mflag = 1;
                                }
                                n = 10*n + c - '0';
                        }
                        mlwrite("Arg: %d", (mflag >=0) ? n : (n ? -n : -1));
                }
                /*
                 * Make arguments preceded by a minus sign negative and change
                 * the special argument "^U -" to an effective "^U -1".
                 */
                if (mflag == -1) {
                        if (n == 0)
                                n++;
                        n = -n;
                }
        }
        if (c == (COTL|'X'))                    /* ^X is a prefix       */
                c = CTLX | getctl();
        if (kbdmip != NULL) {                   /* Save macro strokes.  */
                if (c!=(CTLX|')') && kbdmip>&kbdm[NKBDM-6]) {
                        ctrlg(FALSE, 0);
                        goto loop;
                }
                if (f != FALSE) {
                        *kbdmip++ = (COTL|'U');
                        *kbdmip++ = n;
                }
                *kbdmip++ = c;
        }
        rtn_flag = execute(c, f, n);                /* Do it.               */
        if(rtn_flag == EXIT)
           return;
	else
           goto loop;
}

/*
 * Initialize all of the buffers and windows. The buffer name is passed down
 * as an argument, because the main routine may have been told to read in a
 * file by default, and we want the buffer name to be right.
 */
globle VOID edinit(
char    bname[])
{
        register BUFFER *bp;
        register WINDOW *wp;

        bp     = bfind(bname, TRUE, 0);              /* First buffer        */
        blistp = bfind("[List]", TRUE, BFTEMP);      /* Buffer list buffer  */
        wp     = (WINDOW *) genalloc((unsigned) sizeof(WINDOW));  /* First window        */
        curbp  = bp;                            /* Make this current    */
        wheadp = wp;
        curwp  = wp;
        wp->w_wndp  = NULL;                     /* Initialize window    */
        wp->w_bufp  = bp;
        bp->b_nwnd  = 1;                        /* Displayed.           */
        wp->w_linep = bp->b_linep;
        wp->w_dotp  = bp->b_linep;
        wp->w_doto  = 0;
        wp->w_markp = NULL;
        wp->w_marko = 0;
        wp->w_toprow = 0;
        wp->w_ntrows = (char) term.t_nrow-1;    /* "-1" for mode line.  */
        wp->w_force = 0;
        wp->w_flag  = WFMODE|WFHARD;            /* Full.                */

        /* Secret Buffer for CLIPS Compile output */

        CompileBufferp = bfind("[Compilations]",TRUE,BFTEMP);
}

/*
 * This is the general command execution routine. It handles the fake binding
 * of all the keys to "self-insert". It also clears out the "thisflag" word,
 * and arranges to move it to the "lastflag", so that the next command can
 * look at it. Return the status of command.
 */
globle int execute(
int c,
int f,
int n)
{
        register KEYTAB *ktp;
        register int    status;

        ktp = &keytab[0];                       /* Look in key table.   */
        while (ktp < &keytab[NKEYTAB]) {
                if (ktp->k_code == c) {
                        thisflag = 0;
                        status   = (*ktp->k_fp)(f, n);
                        lastflag = thisflag;
                        return (status);
                }
                ++ktp;
        }

        /*
         * If a space was typed, fill column is defined, the argument is non-
         * negative, and we are now past fill column, perform word wrap.
         */
        if (c == ' ' && fillcol > 0 && n>=0 && getccol(FALSE) > fillcol)
                wrapword();

        if ((c>=0x20 && c<=0x7E)                /* Self inserting.      */
        ||  (c>=0xA0 && c<=0xFE)) {
                if (n <= 0) {                   /* Fenceposts.          */
                        lastflag = 0;
                        return (n<0 ? FALSE : TRUE);
                }
                thisflag = 0;                   /* For the future.      */
                status   = linsert(n, c);
                lastflag = thisflag;
                return (status);
        }
        lastflag = 0;                           /* Fake last flags.     */
        return (FALSE);
}

/*
 * Read in a key.
 * Do the standard keyboard preprocessing. Convert the keys to the internal
 * character set.
 */
globle int getkey()
{
        register int    c;

        c = (*term.t_getchar)();
	if ((c & META) == META) return(c);

#if IBM_MSC || IBM_TBC || IBM_ZTC || IBM_ICB || IBM_SC || IBM_GCC
	if (c > 255) {
         switch (c) {
            case UP_ARROW    :
	                      return (COTL | 'P');
            case DOWN_ARROW  :
                              return (COTL | 'N');
            case LEFT_ARROW  :
                              return (COTL | 'B');
            case RIGHT_ARROW :
                              return (COTL | 'F');
            case PGUP_KEY    :
                              return (META | 'V');
            case PGDN_KEY    :
                              return (COTL | 'V');
            case HOME_KEY    :
                              return (META | '<');
            case END_KEY     :
                              return (META | '>');
            case COTL_LEFT_ARROW  :
                              return (META | 'B');
            case COTL_RIGHT_ARROW :
                              return (META | 'F');
            case COTL_AT_SIGN     :
                              return (COTL | '@');
  	    default :
                              return (COTL | 'G');
            }
	}
#endif

        if (c == METACH) {                      /* Apply M- prefix      */
                c = getctl();
                return (META | c);
        }

        if (c>=0x00 && c<=0x1F)                 /* C0 control -> C-     */
                c = COTL | (c+'@');
        return (c);
}

/*
 * Get a key.
 * Apply control modifications to the read key.
 */
globle int getctl()
{
        register int    c;
        c = (*term.t_getchar)();
        if (c>='a' && c<='z')                   /* Force to upper       */
                c -= 0x20;
        if (c>=0x00 && c<=0x1F)                 /* C0 control -> C-     */
                c = COTL | (c+'@');
        return (c);
}

/*
 * Fancy quit command, as implemented by Norm. If the current buffer has
 * changed do a write current buffer and exit emacs, otherwise simply exit.
 */
globle int quickexit(
int f,
int n)
{
        if ((curbp->b_flag&BFCHG) != 0          /* Changed.             */
        && (curbp->b_flag&BFTEMP) == 0)         /* Real.                */
                filesave(f, n);
        return(edquit(f, n));                     /* conditionally quit   */
}

/*
 * Quit command. If an argument, always quit. Otherwise confirm if a buffer
 * has been changed and not written out. Normally bound to "C-X C-C".
 */
#if IBM_TBC
#pragma argsused
#endif
globle int edquit(
int f,
int n)
{
        register int    s;

        if (f != FALSE                          /* Argument forces it.  */
        || anycb() == FALSE                     /* All buffers clean.   */
                                                /* User says it's OK.   */
        || (s=mlyesno("Modified Buffers! Quit")) == TRUE) {
                vttidy();
                full_cleanup();
                return(EXIT);
        }
        return (s);
}

/*
 * Temporary exit from editor. Leave all data structures
 * intact, but tidy up video interface.
 * Connected to "C-X Q".
 */

#if IBM_TBC
#pragma argsused
#endif
globle int temp_quit(
int f,
int n)
{
   vttidy();
   return(EXIT);
}

/*
 * Begin a keyboard macro.
 * Error if not at the top level in keyboard processing. Set up variables and
 * return.
 */
#if IBM_TBC
#pragma argsused
#endif
globle int ctlxlp(
int f,
int n)
{
        if (kbdmip!=NULL || kbdmop!=NULL) {
                mlwrite("Not now");
                return (FALSE);
        }
        mlwrite("[Start macro]");
        kbdmip = &kbdm[0];
        return (TRUE);
}

/*
 * End keyboard macro. Check for the same limit conditions as the above
 * routine. Set up the variables and return to the caller.
 */
#if IBM_TBC
#pragma argsused
#endif
globle int ctlxrp(
int f,
int n)
{
        if (kbdmip == NULL) {
                mlwrite("Not now");
                return (FALSE);
        }
        mlwrite("[End macro]");
        kbdmip = NULL;
        return (TRUE);
}

/*
 * Execute a macro.
 * The command argument is the number of times to loop. Quit as soon as a
 * command gets an error. Return TRUE if all ok, else FALSE.
 */
#if IBM_TBC
#pragma argsused
#endif
globle int ctlxe(
int f,
int n)
{
        register int    c;
        register int    af;
        register int    an;
        register int    s;

        if (kbdmip!=NULL || kbdmop!=NULL) {
                mlwrite("Not now");
                return (FALSE);
        }
        if (n <= 0)
                return (TRUE);
        do {
                kbdmop = &kbdm[0];
                do {
                        af = FALSE;
                        an = 1;
                        if ((c = *kbdmop++) == (COTL|'U')) {
                                af = TRUE;
                                an = *kbdmop++;
                                c  = *kbdmop++;
                        }
                        s = TRUE;
                } while (c!=(CTLX|')') && (s=execute(c, af, an))==TRUE);
                kbdmop = NULL;
        } while (s==TRUE && --n);
        return (s);
}

/*
 * Abort.
 * Beep the beeper. Kill off any keyboard macro, etc., that is in progress.
 * Sometimes called as a routine, to do general aborting of stuff.
 */
#if IBM_TBC
#pragma argsused
#endif
globle int ctrlg(
int f,
int n)
{
        (*term.t_beep)();
        if (kbdmip != NULL) {
                kbdm[0] = (CTLX|')');
                kbdmip  = NULL;
        }
        return (ABORT);
}

globle VOID full_cleanup()
{

/*   Clear all data structures */

   kill_all_buffers(&bheadp);     /* Clear all existing buffers   */

   kill_all_windows();           /* Clear all windows            */

   kill_video_buffers();	 /* Kill special video buffers   */

   kill_cmp_router();		 /* Get rid of special router    */

/*   Clear all global pointers */

   curwp  = NULL;                /* Current window               */
   curbp  = NULL;                /* Current buffer               */
   wheadp = NULL;                /* Head of list of windows      */
   bheadp = NULL;                /* Head of list of buffers      */
   blistp = NULL;                /* Buffer for C-X C-B           */
   kbdmip = NULL;                /* Input pointer for above      */
   kbdmop = NULL;                /* Output pointer for above     */
   pat[0] = '\0';                /* Search pattern               */
   lastbufn[0] = '\0';           /* Name of Last buffer accessed */
   CompileBufferp = NULL;        /* CLIPS Compile Output Buffer  */
}

/*
 * Dispose of all buffers. Clear the buffer (ask first
 * if the buffer has been changed). Then free the header
 * line and the buffer header. Called for full cleanup.
 */

globle int kill_all_buffers(
BUFFER **top_buf)
{
   register BUFFER *bp;

   bp = *top_buf;
   while(bp != NULL) {
        spec_clear(bp);                         /* Blow text away.      */

        genfree((VOID *) bp->b_linep,           /* And free pointer     */
	        (unsigned)  sizeof(LINE)+ bp->b_linep->l_size);

        *top_buf = bp->b_bufp;                       /* Find next buffer     */
        genfree((VOID *) bp, (unsigned) sizeof(BUFFER));   /* Release buffer block */
	bp = *top_buf;
        }

   return (TRUE);
}

globle int kill_all_windows()
{
   register WINDOW *wp;
   register WINDOW *wp1;

   wp = wheadp;
   while(wp != NULL) {
        wp1 = wp->w_wndp;
        genfree((VOID *) wp, (unsigned) sizeof(WINDOW));
	wp  = wp1;
        }

   return (TRUE);
}

/*
 * This routine blows away all of the text in a
 * buffer. Does NOT care if text has been changed!
 */

globle int spec_clear(
BUFFER *bp)
{
        register LINE   *lp;

        bp->b_flag  &= ~BFCHG;                  /* Not changed          */
        while ((lp=lforw(bp->b_linep)) != bp->b_linep)
                lfree(lp);
        bp->b_dotp  = bp->b_linep;              /* Fix "."              */
        bp->b_doto  = 0;
        bp->b_markp = NULL;                     /* Invalidate "mark"    */
        bp->b_marko = 0;
        return (TRUE);
}

globle VOID EditCommand()
  {
   if (PauseEnvFunction != NULL) (*PauseEnvFunction)() ;
   PerformEditCommand();
   if (ContinueEnvFunction != NULL) (*ContinueEnvFunction)(0) ;
   if (RedrawScreenFunction != NULL) (*RedrawScreenFunction)() ;
  }

/*******************************************/
/* EditorFunctionDefinition:               */
/*******************************************/
globle VOID EditorFunctionDefinition()
  {
   DefineFunction2("edit",'v', (int (*)(VOID_ARG)) EditCommand,"EditCommand", "*1k");
  }

#else

globle VOID EditCommand()
  {
   /* Empty Stub */
  }

#endif
#endif

