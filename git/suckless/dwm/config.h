static const char *fonts[]            ={"DaddyTimeMono Nerd Font:style=Book:size=10","EmojiOne:style=Regular:size=10:antialias=true:autohint=true",};
static const char normbgcolor[]       = "#121213";	/* bar backround color */
static const char normfgcolor[]       = "#C0C0C0";	/* bar foreground color on right & left*/
static const char selbgcolor[] 	      = "#000000";	/* highlighted tag // tasklist // focused window background color*/
static const char selfgcolor[] 	      = "#FFFFFF";	/* focused tag and tasklist foreground color*/
static const char unselbordercolor[]  = "#000000";	/* unfocused window border color*/
static const char selbordercolor[]    = "#B3AFC2";	/* focused window border color*/
static const unsigned int borderpx    = 1;              /* border pixel of windows */
static const unsigned int gappx       = 31;             /* gaps between windows */
static const unsigned int snap        = 33;             /* snap pixel */
static const int showbar              = 1;              /* 0 means no bar */
static const int topbar               = 1;              /* 0 means bottom bar */
static const unsigned int baralpha    = 221;		/* OPAQUE is defined to be 0xFF, same as 255. Choose your values between 0 and 255 where 255 is not transparent at all */
static const unsigned int borderalpha = 221;		/* OPAQUE is defined to be 0xFF, same as 255. Choose your values between 0 and 255 where 255 is not transparent at all */
static const char *colors[][3] = {
	[SchemeNorm] = { normfgcolor, normbgcolor, unselbordercolor },
	[SchemeSel]  = { selfgcolor, selbgcolor,  selbordercolor  },
};

static const unsigned int alphas[][3] = {
	[SchemeNorm] = { OPAQUE, baralpha, borderalpha },
	[SchemeSel]  = { OPAQUE, baralpha, borderalpha },
};

static const char *tags[] = {"< 01 >","< 02 >","< 03 >","< 04 >","< 05 >","< 06 >","< 07 >","< 08 > ","< 09 >"};
//static const char *tags[] = { "01", "02", "03", "04", "05", "06", "07", "08", "09" };
//static const char *tags[] = {"<1>","<2>","<3>","<4>","<5>","<6>","<7>","<8>","<9>"};
//static const char *tags[] = { "", "", "", "", "", "", "", "", "" }; 

static const Rule rules[] = {
	{ "mpv",      NULL,       NULL,       0,            1,           -1 },
	{ "Firefox",  NULL,       NULL,       1 << 1,       0,           -1 },
};

static const float mfact     = 0.50; 
static const int nmaster     = 1;    
static const int resizehints = 0;   

#include "layouts.c"
#include "fibonacci.c"
#include "horizgrid.c"
#include "movestack.c"

static const Layout layouts[] = {
	{ "[]=",      tile },    
	{ "TTT",      bstack},
	{ "###",      horizgrid },
 	{ "|M|",      centeredmaster },
	{ "HHH",      grid },
	{ "[@]",      spiral },
	{ ">M>",      centeredfloatingmaster },
 	{ "[\\]",     dwindle },
	{ "[M]",      monocle },
	{ "><>",      NULL },   
	{ NULL,       NULL },
};

#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

static Key keys[] = {
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY,                       XK_comma,  setlayout,      {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY,                       XK_minus,  setgaps,        {.i = -1 } },
	{ MODKEY,                       XK_equal,  setgaps,        {.i = +1 } },
	{ MODKEY,                       XK_space,  cyclelayout,    {.i = +1 } },
	{ ControlMask,                  XK_1,      setlayout,      {.v = &layouts[0]} },
	{ ControlMask,                  XK_2,      setlayout,      {.v = &layouts[1]} },
	{ ControlMask,                  XK_3,      setlayout,      {.v = &layouts[2]} },
	{ ControlMask,                  XK_4,      setlayout,      {.v = &layouts[3]} },
	{ ControlMask,                  XK_5,      setlayout,      {.v = &layouts[4]} },
	{ ControlMask,                  XK_6,      setlayout,      {.v = &layouts[5]} },
	{ ControlMask,                  XK_7,      setlayout,      {.v = &layouts[6]} },
	{ ControlMask,                  XK_8,      setlayout,      {.v = &layouts[7]} },
	{ ControlMask,                  XK_9,      setlayout,      {.v = &layouts[8]} },
	{ ControlMask,                  XK_0,      setlayout,      {.v = &layouts[9]} },
	{ MODKEY|ShiftMask,             XK_equal,  setgaps,        {.i = 0  } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0} },
	{ MODKEY|ShiftMask,             XK_j,      movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_k,      movestack,      {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          SHCMD("st") },
	{ MODKEY|ShiftMask,		XK_b,	   spawn,	   SHCMD("firefox") },
	{ MODKEY|ShiftMask,		XK_f,	   spawn,	   SHCMD("spacefm") },
	{ MODKEY|ShiftMask,		XK_m,	   spawn,	   SHCMD("st -e mutt") },
	{ MODKEY|ControlMask,           XK_r,      quit,           {0} },
	{ MODKEY|ControlMask,		XK_d,      spawn, 	   SHCMD("~/.local/bin/dm_fm") },
	{ MODKEY|ControlMask,		XK_e,      spawn, 	   SHCMD("~/.local/bin/dm_ed")},
	{ MODKEY|ControlMask,		XK_p,      spawn,          SHCMD("~/.local/bin/dm_pass") },
	{ Mod1Mask,			XK_d,      spawn,          SHCMD("~/.local/bin/dm_ytdl") },
	{ Mod1Mask,			XK_e,      spawn,          SHCMD("~/.local/bin/dm_uni") },
	{ Mod1Mask,		        XK_q,      spawn, 	   SHCMD("~/.local/bin/dm_power") },
	{ Mod1Mask,		        XK_n,	   spawn,	   SHCMD("~/.local/bin/dm_nerd") },
	{ Mod1Mask, 	                XK_k,	   spawn,	   SHCMD("~/.local/bin/dm_col") },
	{ MODKEY,			XK_p,	   spawn,          SHCMD("~/.local/bin/dm_path") },
	{ 0,                            XK_Print,  spawn,          SHCMD("~/.local/bin/dm_ss") },
	{ 0,                            XK_F10,	   spawn,          SHCMD("amixer -q set Master toggle") },
	{ 0,                            XK_F11,	   spawn,          SHCMD("amixer set Master Front 1-") },
	{ 0,                            XK_F12,	   spawn,          SHCMD("amixer set Master Front 1+") },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
};

static Button buttons[] = {
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          SHCMD("st") },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

