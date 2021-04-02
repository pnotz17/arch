/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] 	   = {"DaddyTimeMono Nerd Font:style=Book:size=11:antialias=true:autohint=true","EmojiOne:style=Regular:size=11:antialias=true:autohint=true"};
static int topbar 		   = 0;         /* -b  option; if 0, dmenu appears at bottom     */
static int centered 		   = 1;         /* -c option; centers dmenu on screen */
static int min_width 		   = 500;       /*  minimum width when centered */
static int fuzzy 		   = 1;         /* -F  option; if 0, dmenu doesn't use fuzzy matching     */
static unsigned int lineheight 	   = 0;		/* -h option; minimum height of a menu line */
static unsigned int min_lineheight = 8;		/* -h option; minimum height of a menu line */
static const char *prompt      	   = NULL;      /* -p  option; prompt to the left of input field */
static unsigned int lines      	   = 20;	/* -l and -g options; controls number of lines and columns in grid if > 0 */
static unsigned int columns    	   = 1;		/* -l and -g options; controls number of lines and columns in grid if > 0 */
static const char worddelimiters[] 	 = " ";	/*  Characters not considered part of a word while deleting words* for example: " /?\"&[]"*/
static const unsigned int border_width 	 = 2;	/*  Size of the window border */
static const char *colors[SchemeLast][2] = {
/*     			     fg         bg       */
[SchemeNorm] 		= { "#bbbbbb", "#080808" },
[SchemeSel] 		= { "#eeeeee", "#666666" },
[SchemeSelHighlight] 	= { "#000000", "#00FF00" },
[SchemeNormHighlight] 	= { "#000000", "#770000" },
[SchemeOut] 		= { "#000000", "#00ffff" },
[SchemeOutHighlight] 	= { "#ffc978", "#00ffff" },
[SchemeMid] 		= { "#C0C0C0", "#222222" },
};
