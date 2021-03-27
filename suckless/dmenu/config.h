/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static const char *fonts[]          							= { "UbuntuMono Nerd Font:pixelsize=11:antialias=true:autohint=true", "Noto Color Emoji:style=Regular:size=10:antialias=true:autohint=true" };
static int topbar 														= 1;                      			/* -b  option; if 0, dmenu appears at bottom     */
static int centered 													= 1;                    				/* -c option; centers dmenu on screen */
static int min_width 												= 500;                 			/* minimum width when centered */ /* -fn option overrides fonts[0]; default X11 font or font set */
static const char *prompt      								= NULL;      					/* -p  option; prompt to the left of input field */
static unsigned int lines      									=21;								/* -l and -g options; controls number of lines and columns in grid if > 0 */
static unsigned int lineheight 							= 1;         						/* -h option; minimum height of a menu line     */
static unsigned int columns    							= 1;                                 /* number of columns displayed*/
static const char worddelimiters[] 					= " ";								/* Characters not considered part of a word while deleting words*/
static const unsigned int border_width 		= 1;									/* Size of the window border */
static const char *colors[SchemeLast][2] 	= {
	/*     																  				    fg         		  bg       */
	[SchemeNorm] 													= { "#cccccc", "#111111" },
	[SchemeSel] 															= { "#ffffff", "#7F7F7F" },
	[SchemeSelHighlight] 										= { "#ffffff", "#000000" },
	[SchemeNormHighlight] 									= { "#FFC94B", "#000000" },
	[SchemeOut] 														= { "#000000", "#00ffff" },
};
