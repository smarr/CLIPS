/*   CLIPS Version 6.10  04/09/97 */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */

#include "setup.h"

#if   EMACS_EDITOR && ! RUN_TIME

#define _EDTERM_SOURCE_
#include "ed.h"

#include <stdlib.h>

#if ANSI
static VOID ansimove(int,int);
static VOID ansieeol(void);
static VOID ansieeop(void);
static VOID ansibeep(void);
static VOID ansiparm(int);
static VOID ansiopen(void);
#endif

#if VT52
static VOID vt52move(int,int);
static VOID vt52eeol(void);
static VOID vt52eeop(void);
static VOID vt52beep(void);
static VOID vt52parm(int);
static VOID vt52open(void);
#endif

#if IBM_PC
static VOID pc_open(void);
static int scinit(int);
static int getboard(void);
static int pc_getc(void);
static VOID pc_putc(int);
static VOID pc_move(int,int);
static VOID pc_eeol(void);
static VOID pc_eeop(void);
static VOID pc_beep(void);
#endif

#if TERMCAP
extern int tgetent(char *,char *);
extern char *tgoto(char *,int,int);
extern int tputs(register char *,int,int (*)(int));

static VOID tcapmove(int,int);
static VOID tcapeeol(void);
static VOID tcapeeop(void);
static VOID tcapbeep(void);
static VOID tcapopen(void);
static VOID putpad(char *);
#endif

/* ==========================================================================
 *                              ANSI Terminal
 * ==========================================================================
 */

#if     ANSI

/*
 * The routines in this section provide support for ANSI style terminals
 * over a serial line. The serial I/O services are provided by routines in
 * "termio.c". It compiles into nothing if not an ANSI device.
 */

#define NROW    23                      /* Screen size.                 */
#define NCOL    77                      /* Edit if you want to.         */
#define BEL     0x07                    /* BEL character.               */
#define ESC     0x1B                    /* ESC character.               */

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */
Thread TERM    term    = {
        NROW-1,
        NCOL,
        ansiopen,
        ttclose,
        ttgetc,
        ttputc,
        ttflush,
        ansimove,
        ansieeol,
        ansieeop,
        ansibeep
};

static VOID ansimove(
int row,
int col)
{
        ttputc(ESC);
        ttputc('[');
        ansiparm(row+1);
        ttputc(';');
        ansiparm(col+1);
        ttputc('H');
}

static VOID ansieeol()
{
        ttputc(ESC);
        ttputc('[');
        ttputc('K');
}

static VOID ansieeop()
{
        ttputc(ESC);
        ttputc('[');
        ttputc('J');
}

static VOID ansibeep()
{
        ttputc(BEL);
        ttflush();
}

static VOID ansiparm(
int    n)
{
        register int    q;

        q = n/10;
        if (q != 0)
                ansiparm(q);
        ttputc((n%10) + '0');
}

static VOID ansiopen()
{
#if     UNIX_7 || UNIX_V
        register char *cp;

        if ((cp = getenv("TERM")) == NULL) {
                puts("Shell variable TERM not defined!");
                exit(1);
        }
        if (strcmp(cp, "vt100") != 0) {
                puts("Terminal type not 'vt100'!");
                exit(1);
        }
#endif
        ttopen();
}

#endif

/* ==========================================================================
 *                              VT52 Terminal
 * ==========================================================================
 */

#if     VT52

/*
 * The routines in this section
 * provide support for VT52 style terminals
 * over a serial line. The serial I/O services are
 * provided by routines in "termio.c". It compiles
 * into nothing if not a VT52 style device. The
 * bell on the VT52 is terrible, so the "beep"
 * routine is conditionalized on defining BEL.
 */

#define NROW    24                      /* Screen size.                 */
#define NCOL    80                      /* Edit if you want to.         */
#define BIAS    0x20                    /* Origin 0 coordinate bias.    */
#define ESC     0x1B                    /* ESC character.               */
#define BEL     0x07                    /* ascii bell character         */

/*
 * Dispatch table. All the
 * hard fields just point into the
 * terminal I/O code.
 */
globle TERM    term    = {
        NROW-1,
        NCOL,
        vt52open,
        ttclose,
        ttgetc,
        ttputc,
        ttflush,
        vt52move,
        vt52eeol,
        vt52eeop,
        vt52beep
};

static VOID vt52move(
int row,
int col)
{
        ttputc(ESC);
        ttputc('Y');
        ttputc(row+BIAS);
        ttputc(col+BIAS);
}

static VOID vt52eeol()
{
        ttputc(ESC);
        ttputc('K');
}

static VOID vt52eeop()
{
        ttputc(ESC);
        ttputc('J');
}

static VOID vt52beep()
{
#ifdef  BEL
        ttputc(BEL);
        ttflush();
#endif
}

static VOID vt52open()
{
#if     UNIX_7 || UNIX_V
        register char *cp;

        if ((cp = getenv("TERM")) == NULL) {
                puts("Shell variable TERM not defined!");
                exit(1);
        }
        if (strcmp(cp, "vt52") != 0 && strcmp(cp, "z19") != 0) {
                puts("Terminal type not 'vt52'or 'z19' !");
                exit(1);
        }
#endif
        ttopen();
}

#endif

/* ==========================================================================
 *                              IBM PC Code
 * ==========================================================================
 */

#if   IBM_PC                                    /* Should be an IBM PC using    */
                                                /* the Microsoft C compiler     */
                                                /* or the Turbo C compiler      */
                                                /* or the Zortech C compiler    */
                                                /* or the Intel C Code builder  */
#if   IBM_MSC || IBM_TBC || IBM_ZTC || IBM_ICB || IBM_GCC
#include        <dos.h>

#if IBM_MSC || IBM_ICB || IBM_GCC
#include    <conio.h>
#endif

#if IBM_ZTC
#include    <disp.h>
#endif

#define NROW    25                      /* Screen size. rows            */
#define NCOL    80                      /* Columns                      */
#define BEL     0x07                    /* BEL character.               */
#define ESC     0x1B                    /* ESC character.               */
#define SPACE   32

#if IBM_ICB
#define	SCADC	0xb8000         /* CGA address of screen RAM	*/
#define	SCADM	0xb0000         /* MONO address of screen RAM	*/
#else
#define	SCADC	0xb8000000L     /* CGA address of screen RAM	*/
#define	SCADM	0xb0000000L     /* MONO address of screen RAM	*/
#endif

#define MONOCRSR 0x0B0D			/* monochrome cursor	    */
#define CGACRSR 0x0607			/* CGA cursor		    */

#define	CDCGA	0			/* color graphics card		*/
#define	CDMONO	1			/* monochrome text card		*/
#define	CDSENSE	9			/* detect the card type		*/

#define NDRIVE	3			/* number of screen drivers	*/

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */

Thread globle TERM    term    = {
        NROW-1,
        NCOL,
        pc_open,
        ttclose,
        pc_getc,
        pc_putc,
        ttflush,
        pc_move,
        pc_eeol,
        pc_eeop,
        pc_beep
};

Thread static int dtype = -1;		/* current display type		*/

#if IBM_ICB
Thread static int scadd;		/* address of screen ram	*/
Thread static short *scptr[NROW];	/* pointer to screen lines	*/
Thread static unsigned short sline[NCOL];/* screen line image		*/
#else
Thread static long scadd;		/* address of screen ram	*/
Thread static int *scptr[NROW];	/* pointer to screen lines	*/
Thread static unsigned int sline[NCOL];/* screen line image		*/
#endif

Thread static union REGS rg;

static VOID pc_open()
{
 scinit(CDSENSE);
 ttopen();
}

static int scinit(	/* initialize the screen head pointers */

int type)	/* type of adapter to init for */

{
#if IBM_ICB
	union {
		int laddr;	/* long form of address */
		short *paddr;	/* pointer form of address */
	} addr;
#else
	union {
		long laddr;	/* long form of address */
		int *paddr;	/* pointer form of address */
	} addr;
#endif
	int i;

#if IBM_ZTC
	disp_open();
#endif

	/* if asked...find out what display is connected */
	if (type == CDSENSE)
		type = getboard();

	/* if we have nothing to do....don't do it */
	if (dtype == type)
		return(TRUE);

	switch (type) {
		case CDMONO:	/* Monochrome adapter */
				scadd = SCADM;
				break;

		case CDCGA:	/* Color graphics adapter */
				scadd = SCADC;
				break;

	}
	dtype = type;

	/* initialize the screen pointer array */
	for (i = 0; i < NROW; i++) {
#if IBM_ICB
		addr.laddr = scadd + (NCOL * i * 2);
#else
		addr.laddr = scadd + (long)(NCOL * i * 2);
#endif
		scptr[i] = addr.paddr;
	}
	return(TRUE);
}

/* getboard:	Determine which type of display board is attached.
		Current known types include:

		CDMONO	Monochrome graphics adapter
		CDCGA	Color Graphics Adapter
*/

/* getbaord:	Detect the current display adapter
		if MONO		set to MONO
		   CGA		set to CGA
*/

static int getboard()
{
	int type;	/* board type to return */

	type = CDCGA;
	int86(0x11, &rg, &rg);
	if ((((rg.x.ax >> 4) & 3) == 3))
		type = CDMONO;
        return(type);
}

static int pc_getc()
{
   int intrpt = 22;                  /* ROM-BIOS call for keyboard read */

   rg.h.al = 0;                  /* Clear input registers   */
   rg.h.ah = 0;                  /* and set service to read */

   int86(intrpt, &rg, &rg);

   if(rg.h.al != 0)            /* If low byte is not clear */
     return(rg.h.al);          /* then return value        */
   else {                           /* else,                    */
     switch(rg.h.ah) {         /* check hi byte for code   */
       case  3 :
                return(COTL_AT_SIGN);
       case 71 :
                return(HOME_KEY);
       case 72 :
                return(UP_ARROW);
       case 73 :
                return(PGUP_KEY);
       case 75 :
                return(LEFT_ARROW);
       case 77 :
                return(RIGHT_ARROW);
       case 79 :
                return(END_KEY);
       case 80 :
                return(DOWN_ARROW);
       case 81 :
                return(PGDN_KEY);
       case 115 :
                return(COTL_LEFT_ARROW);
       case 116 :
                return(COTL_RIGHT_ARROW);
       default :
			 return(BADKEY);
       }
   }
}

static VOID pc_putc(
int c)
{
	rg.h.ah = 14;		/* write char to screen with current attrs */
	rg.h.al = (unsigned char) c;
        rg.h.bh = 0;
	rg.h.bl = 0x07;
	int86(0x10, &rg, &rg);
}

static VOID pc_move(
int row,
int col)
{
	rg.h.ah = 2;		/* set cursor position function code */
	rg.h.dl = (unsigned char) col;
	rg.h.dh = (unsigned char) row;
	rg.h.bh = 0;		/* set screen page number */
	int86(0x10, &rg, &rg);
}

static VOID pc_eeol()
{
	unsigned int attr;	/* attribute byte mask to place in RAM */
#if IBM_ICB
	unsigned short *lnptr;	/* pointer to the destination line */
#else
	unsigned int *lnptr;	/* pointer to the destination line */
#endif
	int i;
	int ccol;	/* current column cursor lives */
	int crow;	/*	   row	*/

	/* find the current cursor position */
	rg.h.ah = 3;		/* read cursor position function code */
	rg.h.bh = 0;		/* current video page */
	int86(0x10, &rg, &rg);
	ccol = rg.h.dl;		/* record current column */
	crow = rg.h.dh;		/* and row */

	/* build the attribute byte and setup the screen pointer */
	attr = 0x0700;
	lnptr = &sline[0];
	for (i=0; i < term.t_ncol; i++)
		*lnptr++ = SPACE | attr;

	if (dtype == CDCGA) {
		/* wait for vertical retrace to be off */
		while ((inp(0x3da) & 8))
			;
	
		/* and to be back on */
		while ((inp(0x3da) & 8) == 0)
			;
	}			

#if IBM_ZTC
        disp_move(crow,ccol);
        disp_flush();
	disp_eeol();
#endif
	/* and send the string out */
#if (! IBM_ZTC)
	memmove(scptr[crow]+ccol, &sline[0], (term.t_ncol-ccol)*2);
#endif
}

static VOID pc_eeop()
{
	int attr;		/* attribute to fill screen with */

	rg.h.ah = 6;		/* scroll page up function code */
	rg.h.al = 0;		/* # lines to scroll (clear it) */
	rg.x.cx = 0;		/* upper left corner of scroll */
	rg.x.dx = (term.t_nrow << 8) | (term.t_ncol - 1);
				/* lower right corner of scroll */
	attr = 0x07;
	rg.h.bh = (unsigned char) attr;
	int86(0x10, &rg, &rg);
}

static VOID pc_beep()
{
        pc_putc(BEL);
        ttflush();
}

#endif
#endif


/* ==========================================================================
 *                         Termcap Terminal
 * ==========================================================================
 */

#if TERMCAP

/*
 * The routines in this section provide support for terminals supported
 * through the UNIX termcap capability.
 *
 * You need to include the termcap library at link time
 * or else most of the calls to t____ functions don't work
 */

#define NROW    24
#define NCOL    80
#define BEL     0x07
#define ESC     0x1B

#define TCAPSLEN 315

Thread static char tcapbuf[TCAPSLEN];
Thread static char    PC,
        *CM,
        *CE,
        *UP,
        *CD;


Thread globle TERM term = {
        NROW-1,
        NCOL,
        tcapopen,
        ttclose,
        ttgetc,
        ttputc,
        ttflush,
        tcapmove,
        tcapeeol,
        tcapeeop,
        tcapbeep
};

static VOID tcapopen()
{
        extern char *tgetstr(char *,char **);
        char *t, *p;
        char tcbuf[1024];
        char *tv_stype;
        char err_str[72];

        if ((tv_stype = getenv("TERM")) == NULL)
        {
                puts("Environment variable TERM not defined!");
                exit(1);
        }

        if((tgetent(tcbuf, tv_stype)) != 1)
        {
                sprintf(err_str, "Unknown terminal type %s!", tv_stype);
                puts(err_str);
                exit(1);
        }

        p = tcapbuf;
        t = tgetstr("pc", &p);
        if(t)
                PC = *t;

        CD = tgetstr("cd", &p);
        CM = tgetstr("cm", &p);
        CE = tgetstr("ce", &p);
        UP = tgetstr("up", &p);

        if(CD == NULL || CM == NULL || CE == NULL || UP == NULL)
        {
                puts("Incomplete termcap entry\n");
                exit(1);
        }

        if (p >= &tcapbuf[TCAPSLEN])
        {
                puts("Terminal description too big!\n");
                exit(1);
        }
        ttopen();
}

static VOID tcapmove(row, col)
int row, col;
{
        putpad(tgoto(CM, col, row));
}

static VOID tcapeeol()
{
        putpad(CE);
}

static VOID tcapeeop()
{
        putpad(CD);
}

static VOID tcapbeep()
{
        ttputc(BEL);
}

static VOID putpad(str)
char    *str;
{
        tputs(str, 1, (int (*)(int)) ttputc);
}

#endif

#endif          /* end original EMACS_EDITOR definition */
