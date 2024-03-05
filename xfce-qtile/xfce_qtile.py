### CUSTOM_SCRIPTS #############################################################
# change-default-sink
# power-commands
# rofi-power
### APP_DEPENDENCIES ###########################################################
# thunar/pcmanfm
# bravebrowser/firefox
# pamixer
# deadbeef
# rofi
################################################################################

from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy
#from libqtile.utils import guess_terminal

mod = "mod4"
altKey = "mod1"
#terminal = guess_terminal()
terminal = "alacritty"

keys = [
    # Switch between windows
    Key([mod], "j", lazy.group.next_window()),
    Key([mod], "k", lazy.group.prev_window()),

    # Key([mod], "j", lazy.layout.next()),
    # Key([mod], "k", lazy.layout.previous()),

    # Move windows inside the current group
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),

    # Move floating windows
    Key([mod, "shift"], "Left",  lazy.window.move_floating(-10, 0)),
    Key([mod, "shift"], "Right", lazy.window.move_floating(10, 0)),
    Key([mod, "shift"], "Up",    lazy.window.move_floating(0, -10)),
    Key([mod, "shift"], "Down",  lazy.window.move_floating(0, 10)),

    # Resize
    Key([mod, "control"], "j", lazy.layout.grow()),
    Key([mod, "control"], "l", lazy.layout.grow_main()),
    Key([mod, "control"], "k", lazy.layout.shrink()),
    Key([mod, "control"], "h", lazy.layout.shrink_main()),

    # Resize floating windows
    Key([mod, "control"], "Left",  lazy.window.resize_floating(-10, 0)),
    Key([mod, "control"], "Right", lazy.window.resize_floating(10, 0)),
    Key([mod, "control"], "Up",    lazy.window.resize_floating(0, -10)),
    Key([mod, "control"], "Down",  lazy.window.resize_floating(0, 10)),

    # Switch between groups
    Key([mod], "h",   lazy.screen.prev_group()),
    Key([mod], "l",   lazy.screen.next_group()),
    Key([mod], "Tab", lazy.screen.toggle_group()),

    # Audio
    Key([mod, "shift"], "0",
        lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle"),
        desc="toggle mute on default sink"),
    Key([mod], "equal",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%"),
        desc="increase volume on default sink by 5%"),
    Key([mod], "minus",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%"),
        desc="decrease volume on default sink by 5%"),
    Key([mod], "0",
        lazy.spawn("bash /home/pedro/dotfiles/scripts/new_change_sink.sh")),

    # Deadbeef
    #Key([altKey, "control"], "l",     lazy.spawn("deadbeef --random")),
    #Key([altKey, "control"], "k",     lazy.spawn("deadbeef --toggle-pause")),
    #Key([altKey, "control"], "j",     lazy.spawn("deadbeef --stop")),
    #Key([altKey, "control"], "equal", lazy.spawn("deadbeef --volume +5")),
    #Key([altKey, "control"], "minus", lazy.spawn("deadbeef --volume -5")),

    # Programs
    #Key([mod],    "Return", lazy.spawn("alacritty"),  desc="Launch terminal"),
    Key([mod],    "Return", lazy.spawn(terminal),     desc="Launch terminal"),
    Key([mod],    "e",      lazy.spawn("thunar"),     desc="File Manager"),
    Key([mod],    "w",      lazy.spawn("firefox"),    desc="Web Browser"),
    Key([mod],    "c",      lazy.spawn("galculator"), desc="Calculator"),
    #Key([mod],    "F9",     lazy.spawn("discord")),
    Key([mod],    "m",      lazy.spawn("xfce4-settings-manager")),
    Key([mod],    "p",      lazy.spawn("xfce4-appfinder")),
    Key([mod],    "r",      lazy.spawn("xfce4-appfinder --collapsed")),
    #Key([altKey], "F3",     lazy.spawn("xflock4")),
    Key([altKey], "F4",     lazy.spawn("xfce4-session-logout")),

    # Others
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "shift"], "r", lazy.restart(),     desc="Restart Qtile"),
    #Key([mod,  "control"], "F4", lazy.shutdown(),    desc="Shutdown Qtile"),
]

#############################################################################################
# Groups ####################################################################################
#############################################################################################

groups = [
    Group(i) for i in "123456789"
]

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
# ScratchPads ###############################################################################
#############################################################################################

groups.append(ScratchPad("scratchpad", [
    #DropDown("term", "alacritty", width=0.8, height=0.8, y=0.08, opacity=1),
    DropDown("term", terminal, width=0.8, height=0.8, y=0.08, opacity=1),
    #DropDown("htop", "alacritty -e htop", width=0.8, height=0.8, y=0.08, opacity=1),
    DropDown("htop", (terminal + " -e htop"), width=0.8, height=0.8, y=0.08, opacity=1),
    DropDown("music", "deadbeef", width=0.8, height=0.8, y=0.08, opacity=1),
]))

keys.extend([
    Key([mod], "F10", lazy.group["scratchpad"].dropdown_toggle("term")),
    Key([mod], "t",   lazy.group["scratchpad"].dropdown_toggle("term")),
    Key([mod], "F11", lazy.group["scratchpad"].dropdown_toggle("htop")),
    Key([mod], "F12", lazy.group["scratchpad"].dropdown_toggle("music")),
])

#############################################################################################
# Layouts ###################################################################################
#############################################################################################

layouts = [
    layout.MonadTall(border_focus="#ababab", border_normal="#323232",
                     border_width=2, single_border_width=0),
    # layout.MonadWide(border_focus="#ababab", border_normal="#323232",
    #                  border_width=2, single_border_width=0),
    layout.Max()
]


mouse = [
    Drag([mod],  "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod],  "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]


# Floating rule for Discord to be the same size and position as the scrachpads
@hook.subscribe.client_new
def set_floating(window):
    if window.name == "discord":
        window.floating = True
        window.place(
            x=int(window.qtile.screen.width * 0.08),
            y=int(window.qtile.screen.height * 0.08),
            width=int(window.qtile.screen.width * 0.8),
            height=int(window.qtile.screen.height * 0.8),
        )

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
        # My rules
        Match(wm_class="MPlayer"),
        Match(wm_class="Galculator"),
        Match(wm_class="Pamac-manager"),
        Match(wm_class="Lxpolkit"),
        Match(wm_class="Image Resizer"),
        Match(wm_class="Xfce4-appfinder"),
        Match(wm_class="Deadbeef"),
        Match(wm_class="steamwebhelper"),
        Match(wm_class="TIPP10"),
        Match(wm_class="discord"),
        Match(wm_class="Blueman-manager"),
        Match(wm_class="Gpick"),
        # My Apps
        Match(wm_class="music-downloader"),
        Match(wm_class="todos-electron"),
    ],
    border_focus="#7a7a7a",
    border_normal="#333333",
    border_width=3
)


@lazy.function
def minimize_all(qtile):
    for win in qtile.current_group.windows:
        if hasattr(win, "toggle_minimize"):
            win.toggle_minimize()


keys.extend([
    # Layout control
    Key([mod], "i",                 lazy.window.toggle_floating()),
    Key([mod], "o",                 lazy.window.bring_to_front()),
    Key([mod], "u",                 lazy.window.toggle_minimize()),
    Key([mod,  "shift"],   "u",     minimize_all()),
    Key([mod], "b",                 lazy.hide_show_bar()),
    Key([mod], "n",                 lazy.next_layout()),
    Key([mod,  "control"], "n",     lazy.layout.normalize()),
    Key([mod,  "shift"],   "space", lazy.layout.flip()),
])

#############################################################################################
# Widgets ###################################################################################
#############################################################################################

# widget.GenPollText(name="deadbeef", fmt="Music: {}", update_interval=3.0, func=lambda: subprocess.check_output("<script>").decode("UTF-8"))

widget_defaults = dict(font="Fira Code", fontsize=13, padding=3, foreground="aaaabb")
extension_defaults = widget_defaults.copy()
top_bar = bar.Bar(
    [
        widget.GroupBox(active="00ffff", inactive="bababa", fontsize=14,
                        highlight_method="line", highlight_color="005757"),
        widget.TaskList(fontsize=13, font="Fira Code",
                        foreground="aaaabb", highlight_method="block", border="343434",
                        margin_x=10, padding_x=2, spacing=4, parse_text=lambda x : x[:25]),
        widget.Volume(fmt='Vol: {}', step=5, update_interval=0.4),
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
