from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
altKey = "mod1"
terminal = guess_terminal()

keys = [
    # Switch between windows
    # Key([mod], "h", lazy.layout.left()),
    #Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    
    # Move windows inside the current group
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
   
    # Resize
    Key([mod, "control"], "j", lazy.layout.grow()),
    Key([mod, "control"], "l", lazy.layout.grow_main()),
    Key([mod, "control"], "k", lazy.layout.shrink()),
    Key([mod, "control"], "h", lazy.layout.shrink_main()),
    
    # Switch between groups
    Key([mod], "h",   lazy.screen.prev_group()),
    Key([mod], "l",   lazy.screen.next_group()),
    Key([mod], "Tab", lazy.screen.toggle_group()),
    
    # Layout control
    Key([mod], "u",               lazy.window.toggle_floating()),
    Key([mod], "b",               lazy.hide_show_bar()),
    Key([mod], "space",           lazy.next_layout()),
    Key([mod, "control"], "n",    lazy.layout.normalize()),
    Key([mod], "m",               lazy.layout.maximize()),
    Key([mod,  "shift"], "space", lazy.layout.flip()),

    # Others
    Key([mod],   "q",              lazy.window.kill(), desc="Kill focused window"),
    Key([mod,    "shift"],   "r",  lazy.restart(),  desc="Restart Qtile"),
    #Key([mod,    "shift"],   "F4", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod,    "shift"],   "q",  lazy.shutdown(), desc="Shutdown Qtile"),
    #Key([mod,    "shift"],   "F2", lazy.spawn("i3lock -i /home/pedro/media/images/wallpaper/lock.png -u; systemctl suspend")),
    Key([mod,    "shift"],   "F3", lazy.spawn("systemctl suspend")),
    #Key([altKey, "control"], "F4", lazy.spawn("systemctl poweroff")),

    # Audio
    Key([mod], "minus",  lazy.spawn("pamixer --decrease 5")),
    Key([mod], "equal",  lazy.spawn("pamixer --increase 5")),
    Key([mod,  "shift"], "0", lazy.spawn("pamixer --toggle-mute")),
    Key([mod], "0", lazy.spawn("/home/pedro/dotfiles/scripts/change-default-sink.sh")),

    # Deadbeef
    Key([altKey, "control"], "l",     lazy.spawn("deadbeef --random")),
    Key([altKey, "control"], "k",     lazy.spawn("deadbeef --toggle-pause")),
    Key([altKey, "control"], "j",     lazy.spawn("deadbeef --prev")),
    Key([altKey, "control"], "h",     lazy.spawn("deadbeef --stop")),
    Key([altKey, "control"], "equal", lazy.spawn("deadbeef --volume +5")),
    Key([altKey, "control"], "minus", lazy.spawn("deadbeef --volume -5")),

    # Programs
    Key([mod], "Return", lazy.spawn("alacritty"), desc="Launch terminal"),
    # Key([mod], "r",      lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "r",
        lazy.spawn("rofi -show drun -modi drun -show-icons -theme ~/.config/rofi/themes/my_dracula.rasi"),
        desc="Rofi run"),
    # Key([mod], "d",      lazy.spawn("dmenu_run -b -f -l 15 -p Apps")),
    Key([mod], "e",      lazy.spawn("thunar"), desc="File Manager"),
    Key([mod], "w",      lazy.spawn("brave")),

]

#############################################################################################
# Groups ####################################################################################
#############################################################################################

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        # Mod + [1..9] to focus group n
        Key([mod], i.name, lazy.group[i.name].toscreen(), 
            desc="Switch to group {}".format(i.name)),
        # Mod + Shift + [1..9] to send window to group n
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            desc="move focused window to group {}".format(i.name)) 
    ])

#############################################################################################
# Layouts ###################################################################################
#############################################################################################

layouts = [
    layout.MonadTall(border_focus="#ababab", border_normal="#323232", 
                     border_width=2, single_border_width=0),
    layout.MonadWide(border_focus="#ababab", border_normal="#323232", 
                     border_width=2, single_border_width=0),
    layout.Max()
]

mouse = [
    Drag([mod],  "Button1",  lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod],  "Button3",  lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop | grep WM_CLASS` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="pavucontrol"),
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ],
    border_focus="#7a7a7a",
    border_normal="#333333",
    border_width=3
)

#############################################################################################
# Widgets ###################################################################################
#############################################################################################

widget_defaults = dict(font="FiraCode Nerd Font Mono", fontsize=13, padding=1, foreground="9999bb")
extension_defaults = widget_defaults.copy()
top_bar = bar.Bar(
    [
        widget.GroupBox(active="ffffff", inactive="989898", fontsize=14, 
            highlight_method="line", highlight_color="005757"),
        widget.Spacer(),
        widget.Volume(fmt='Vol: {}'),
        widget.Sep(padding=20),
        widget.CPU(format="CPU: {load_percent}%", update_interval=1.0),
        widget.Sep(padding=20),
        widget.Memory(format="RAM: {MemUsed:.0f}/{MemTotal:.0f}GB", measure_mem='G', update_interval=1.0),
        widget.Sep(padding=20),
        widget.Clock(format="%d/%m/%Y [%a]", update_interval=60.0),
        widget.Sep(padding=20),
        widget.Clock(format="%R", update_interval=1.0, foreground="00ffff"),
        widget.Sep(padding=20),
        widget.Systray()
    ],
    24,
    background="#000000"
)
screens = [Screen(top=top_bar)]

#############################################################################################
# Configuartion Variables ###################################################################
#############################################################################################

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = False # Mouse hover wont change window focus
bring_front_click = False
cursor_warp = False
auto_fullscreen = True # Apps can ask for fullscreen
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wmname = "LG3D" # For Java UI toolkits