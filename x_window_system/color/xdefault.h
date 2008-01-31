#define DIALOG_FG       0
#define DIALOG_BG       1
#define DIALOG_BDR      2
#define FACTS_FG        3
#define FACTS_BG        4
#define FACTS_BDR       5
#define INSTANCES_FG    6
#define INSTANCES_BG    7
#define INSTANCES_BDR   8
#define GLOBALS_FG       9
#define GLOBALS_BG       10
#define GLOBALS_BDR      11
#define AGENDA_FG       12
#define AGENDA_BG       13
#define AGENDA_BDR      14
#define FOCUS_FG	15
#define FOCUS_BG	16
#define FOCUS_BDR	17
#define BUTTON_FRM_BG   18
#define MENU_BUTTON_FG  19
#define MENU_BUTTON_BG  20
#define MENU_BUTTON_BDR 21
#define SMEBSB_FG       22
#define LINE_FG         23
#define MENU_FG         24
#define MENU_BG         25
#define MENU_BDR        26
#define MANGR_LIST_FG   27
#define MANGR_LIST_BG   28
#define MANGR_LIST_BDR  29
#define MANGR_VP_BDR    30
#define CONFIRM_FG      31
#define CONFIRM_BG      32
#define FILE_DIALOG_FG  33
#define FILE_DIALOG_BG  34
#define FILE_FORM_BG    35
#define MANGR_BUTTN_FG  36
#define MANGR_BUTTN_BG  37
#define MANGR_BUTTN_BDR 38
#define MANGR_CANCL_FG  39
#define MANGR_CANCL_BG  40
#define MANGR_CANCL_BDR 41
#define WATCH_FORM_FG   42
#define WATCH_FORM_BG   43
#define WATCH_FORM_BDR  44
#define TOGGLE_FG       45
#define TOGGLE_BG       46
#define TOGGLE_BDR      47
#define WATCH_FG        48
#define WATCH_BG        49
#define WATCH_BDR       50

#define NUMRES 51

static String defaultstring[NUMRES+1] = {
  "blue",        /* Xclips*dialog_text*foreground */
  "white",       /* Xclips*dialog_text*background */
  "blue",        /* Xclips*dialog_text*borderColor */
  "slateblue",   /* Xclips*facts_text*foreground */
  "seashell1",   /* Xclips*facts_text*background */
  "slateblue",   /* Xclips*facts_text*borderColor */
  "slateblue",   /* Xclips*instances_text*foreground */
  "seashell1",   /* Xclips*instances_text*background */
  "slateblue",   /* Xclips*instances_text*borderColor */
  "slateblue",   /* Xclips*global_text*foreground */
  "seashell1",   /* Xclips*global_text*background */
  "slateblue",   /* Xclips*global_text*borderColor */
  "maroon",      /* Xclips*agenda_text*foreground */
  "gray",        /* Xclips*agenda_text*background */
  "maroon",      /* Xclips*agenda_text*borderColor */
  "maroon",      /* Xclips*focus_text*foreground */
  "gray",        /* Xclips*focus_text*background */
  "maroon",      /* Xclips*focus_text*borderColor */
  "royalblue",   /* Xclips*buttonForm.background */
  "yellow",      /* Xclips*MenuButton.foreground */
  "royalblue",   /* Xclips*MenuButton.background */
  "yellow",      /* Xclips*MenuButton.borderColor */
  "yellow",      /* Xclips*SmeBSB.foreground */
  "yellow",      /* Xclips*line.foreground */
  "yellow",      /* Xclips*menu.foreground */
  "royalblue",   /* Xclips*menu.background */
  "yellow",      /* Xclips*menu.borderColor */
  "white",       /* Xclips*manager_list.foreground */
  "maroon",      /* Xclips*manager_list.background */
  "maroon",      /* Xclips*manager_form.background */
  "cadetblue",   /* Xclips*manager_viewport.borderColor */
  "yellow",      /* Xclips*confirm*foreground */
  "red",         /* Xclips*confirm*background */
  "black",       /* Xclips*file_dialog*foreground */
  "wheat",       /* Xclips*file_dialog*background */
  "wheat",       /* Xclips*file_form.background */
  "blue",        /* Xclips*managerButton.foreground */
  "white",       /* Xclips*managerButton.background */
  "blue",        /* Xclips*managerButton.borderColor */
  "red",         /* Xclips*managerCancel.foreground */
  "white",       /* Xclips*managerCancel.background */
  "red",         /* Xclips*managerCancel.borderColor */
  "black",       /* Xclips*watch_form.foreground */
  "cyan",        /* Xclips*watch_form.background */
  "blue",        /* Xclips*watch_form.borderColor */
  "yellow",      /* Xclips*Toggle.foreground*/
  "blue",        /* Xclips*Toggle.background */
  "yellow",      /* Xclips*Toggle.borderColor */
  "yellow",      /*Xclips*watchButton.foreground*/
  "blue",        /*Xclips*watchButton.background */
  "yellow",      /*Xclips*watchButton.borderColor */
  NULL};
