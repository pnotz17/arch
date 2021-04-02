static const char *fonts[] 		= {"DaddyTimeMono Nerd Font:style=Book:size=9:antialias=true:autohint=true","EmojiOne:style=Regular:size=9:antialias=true:autohint=true",};
static const char dmenufont[] 		= {"DaddyTimeMono Nerd Font:style=Book:size=9:antialias=true:autohint=true"};
static const unsigned int borderpx 	= 1;		/* border pixel of windows */
static const unsigned int gappx 	= 1;		/* gaps between windows */
static const unsigned int snap 		= 32;		/* snap pixel */
static const int showbar		= 1;		/* 0 means no bar */
static const int topbar 		= 1;		/* 0 means bottom bar */
static const char normbgcolor[] 	= "#080808";	/* bar backround color */
static const char normfgcolor[] 	= "#C0C0C0";	/* bar foreground color on right & left*/
static const char selbgcolor[] 		= "#222222";	/* highlighted tag // tasklist // focused window background color*/
static const char selfgcolor[] 		= "#FFFFFF";	/* focused tag and tasklist foreground color*/
static const char unselbordercolor[] 	= "#B3AFC2";	/* unfocused window border color*/
static const char selbordercolor[] 	= "#B3AFC2";	/* focused window border color*/
static const unsigned int baralpha 	= 235;		/* OPAQUE is defined to be 0xFF, same as 255. Choose your values between 0 and 255 where 255 is not transparent at all */
static const unsigned int borderalpha	= 235;		/* OPAQUE is defined to be 0xFF, same as 255. Choose your values between 0 and 255 where 255 is not transparent at all */

static const char *colors[][3] = {
[SchemeNorm]= { normfgcolor, normbgcolor, unselbordercolor},
[SchemeSel] = { selfgcolor,  selbgcolor,  selbordercolor},
};

static const unsigned int alphas[][3] = {
[SchemeNorm] = { OPAQUE, baralpha, borderalpha },
[SchemeSel]  =	{ OPAQUE, baralpha, borderalpha },
};

static const char *tags[]   = { "01", "02", "03", "04", "05", "06", "07", "08", "09" };
//static const char *tags[] = { "", "", "", "", "", "", "", "", "" }; 
//static const char *tags[] = { "dev", "www", "code", "sys", "doc"};

static const Rule rules[] = {
{ "mpv",     NULL,      NULL,       0,        1,       -1 },
{ "gimp",    NULL,      NULL,       0,        1,       -1 },
};

#include "layouts.c"
static const float mfact     = 0.50; 						
static const int nmaster     = 1;    				
static const int resizehints = 0;    						
static const Layout layouts[]= {
{ "[]=",	tile },    
{ "><>",	NULL },    
{ "[M]",	monocle },
{ "HHH",	grid },
{ NULL,		NULL },
};

#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

static char dmenumon[2] 	      = "0"; 						
static const char *dmenucmd[] 	      = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL }; 
static const char *termcmd[]          = { "st", NULL };
static const char *print_screen_cmd[] = { "scrot", "-d2", "pictures/screenshots/%Y-%m-%d-%s_$wx$h.jpg", NULL };
static const char *upvol[]            = { "amixer", "set", "Master", "1+",     NULL };
static const char *downvol[]          = { "amixer", "set", "Master", "1-",     NULL };
static const char *webcmd[]           = { "firefox", NULL };
static const char *filescmd[]         = { "pcmanfm", NULL };
static const char *mailcmd[]          = { "st","-e","mutt", NULL };
#include "movestack.c"

static Key keys[] = {
{ MODKEY,                       	XK_p,				spawn,			{.v = dmenucmd } },
{ MODKEY|ShiftMask,			XK_Return,			spawn,			{.v = termcmd } },
{ MODKEY,                       	XK_b,				togglebar,		{0} },
{ MODKEY,                       	XK_j,				focusstack,		{.i = +1 } },
{ MODKEY,                       	XK_k,				focusstack,		{.i = -1 } },
{ MODKEY,                       	XK_i,				incnmaster,    		{.i = +1 } },
{ MODKEY,                       	XK_d,				incnmaster,    		{.i = -1 } },
{ MODKEY,                       	XK_h,				setmfact,       	{.f = -0.05} },
{ MODKEY,                       	XK_l,				setmfact,       	{.f = +0.05} },
{ MODKEY|ShiftMask,			XK_j,				movestack,     		{.i = +1 } },
{ MODKEY|ShiftMask,			XK_k,				movestack,		{.i = -1 } },
{ MODKEY,                       	XK_Return, 			zoom,           	{0} },
{ MODKEY,                       	XK_Tab,    			view,           	{0} },
{ MODKEY|ShiftMask,			XK_c,				killclient,     	{0} },
{ MODKEY,                       	XK_t,  		    		setlayout,      	{.v = &layouts[0]} },
{ MODKEY,                       	XK_f,      			setlayout,      	{.v = &layouts[1]} },
{ MODKEY,                       	XK_m,      			setlayout,      	{.v = &layouts[2]} },
{ MODKEY,                       	XK_g,      			setlayout,      	{.v = &layouts[3]} },
{ MODKEY|ControlMask,			XK_comma,  			setlayout,      	{0} },
{ MODKEY|ControlMask,			XK_period, 			cyclelayout,	        {.i = -1 } },
{ MODKEY,                       	XK_space,  			cyclelayout,	        {.i = +1 } },
{ MODKEY|ShiftMask,      		XK_space,			togglefloating, 	{0} },
{ MODKEY,                       	XK_0,      			view,           	{.ui = ~0 } },
{ MODKEY|ShiftMask,      		XK_0,      			tag,            	{.ui = ~0 } },
{ MODKEY,                       	XK_comma,			focusmon, 	        {.i = -1 } },
{ MODKEY,                       	XK_period, 			focusmon,       	{.i = +1 } },
{ MODKEY|ShiftMask,     		XK_comma,  			tagmon,      	        {.i = -1 } },
{ MODKEY|ShiftMask,     		XK_period, 			tagmon,         	{.i = +1 } },
{ MODKEY,                        	XK_minus,  			setgaps,        	{.i = -1 } },
{ MODKEY,                       	XK_equal,  			setgaps,        	{.i = +1 } },
{ MODKEY|ShiftMask,     		XK_equal,  			setgaps,        	{.i = 0  } },
{ MODKEY|ShiftMask,			XK_q, quit,           					{0} },
{ MODKEY|ShiftMask,			XK_b,	    			spawn,	   		{.v = webcmd } },
{ MODKEY|ShiftMask,			XK_f,		    		spawn,	   		{.v = filescmd } },
{ MODKEY|ShiftMask,			XK_m,	    			spawn,	   		{.v = mailcmd } },
{ MODKEY|ShiftMask,			XK_p,      			spawn, 	         	SHCMD("~/.local/bin/power.sh") },
{ MODKEY,				XK_v,      			spawn,          	SHCMD("~/.local/bin/youtube-watch.sh") },
{ MODKEY,				XK_a,      			spawn,          	SHCMD("~/.local/bin/youtube-dl-playlist.sh") },
{ MODKEY,				XK_y,      			spawn,          	SHCMD("~/.local/bin/youtube-dl.sh") },
{ MODKEY|ShiftMask,			XK_r,     			spawn,          	SHCMD("feh --no-fehbg --randomize --bg-scale ~/Pictures/Wallpapers/*") },
{ 0,                            	XK_Print,			spawn,          	{.v = print_screen_cmd } },
{ 0,                            	XK_F12,				spawn,          	{.v = upvol   } },
{ 0,                            	XK_F11,				spawn,          	{.v = downvol } },
TAGKEYS(                        	XK_1,                      				0)
TAGKEYS(                        	XK_2,                      				1)
TAGKEYS(                        	XK_3,                      				2)
TAGKEYS(                        	XK_4,                      				3)
TAGKEYS(                        	XK_5,                      				4)
TAGKEYS(                        	XK_6,                      				5)
TAGKEYS(                        	XK_7,                      				6)
TAGKEYS(                        	XK_8,                      				7)
TAGKEYS(                        	XK_9,                      				8)
};

static Button buttons[] = {
{ ClkLtSymbol,				0,              Button1,        	setlayout,      	{0} },
{ ClkLtSymbol,				0,              Button3,        	setlayout,      	{.v = &layouts[2]} },
{ ClkWinTitle,				0,              Button2,        	zoom,           	{0} },
{ ClkStatusText,			0,             	Button2,        	spawn,          	{.v = termcmd } },
{ ClkTagBar,            		0,             	Button1,        	view,           	{0} },
{ ClkTagBar,            		0,             	Button3,       		toggleview,    		{0} },
{ ClkTagBar,            		MODKEY,		Button1,        	tag,           		{0} },
{ ClkTagBar,            		MODKEY,		Button3,        	toggletag,      	{0} },
{ ClkClientWin,         		MODKEY,		Button1,        	movemouse,		{0} },
{ ClkClientWin,         		MODKEY,		Button2,        	togglefloating, 	{0} },
{ ClkClientWin,         		MODKEY,		Button3,        	resizemouse,    	{0} },
};
