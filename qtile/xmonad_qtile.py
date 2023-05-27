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

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
altKey = "mod1"
terminal = guess_terminal()

keys = [
    # Switch between windows
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
    Key([mod], "t",               lazy.window.toggle_floating()),
    Key([mod], "b",               lazy.hide_show_bar()),
    Key([mod], "a",               lazy.next_layout()),
    Key([mod, "control"], "n",    lazy.layout.normalize()),
    Key([mod,  "shift"], "space", lazy.layout.flip()),

    # Audio
    Key([mod], "minus",       lazy.spawn("audio-commands decrease")),
    Key([mod], "equal",       lazy.spawn("audio-commands increase")),
    Key([mod,  "shift"], "0", lazy.spawn("audio-commands toggle-mute")),
    Key([mod], "0",           lazy.spawn("audio-commands change")), # Change output

    # Deadbeef
    Key([altKey, "control"], "l",     lazy.spawn("deadbeef --random")),
    Key([altKey, "control"], "k",     lazy.spawn("deadbeef --toggle-pause")),
    Key([altKey, "control"], "j",     lazy.spawn("deadbeef --stop")),
    Key([altKey, "control"], "equal", lazy.spawn("deadbeef --volume +5")),
    Key([altKey, "control"], "minus", lazy.spawn("deadbeef --volume -5")),

    # Programs
    Key([mod], "Return",     lazy.spawn("alacritty"),  desc="Launch terminal"),
    Key([mod], "e",          lazy.spawn("thunar"),     desc="File Manager"),
    Key([mod], "w",          lazy.spawn("firefox"),    desc="Web Browser"),
    Key([mod], "c",          lazy.spawn("galculator"), desc="Calculator"),
    Key([mod, "shift"], "p", lazy.spawn("xfce4-appfinder")),
    Key([mod], "p",          lazy.spawn("""
        rofi -show drun \
             -modi drun \
             -show-icons \
             -theme ~/.config/rofi/themes/my_dracula.rasi""")),

    # Power
    Key([mod,     "shift"],   "F2", lazy.spawn("power-commands lock-suspend")),
    Key([mod,     "shift"],   "F3", lazy.spawn("power-commands suspend")),
    Key([altKey],             "F4", lazy.spawn("rofi-power")),

    # Others
    Key([mod],    "q",              lazy.window.kill(), desc="Kill focused window"),
    Key([mod,     "control"], "F4", lazy.shutdown(),    desc="Shutdown Qtile"),
    Key([mod,     "shift"],   "r",  lazy.restart(),     desc="Restart Qtile"),
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
    # layout.MonadWide(border_focus="#ababab", border_normal="#323232",
    #                  border_width=2, single_border_width=0),
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
        # My Apps
        Match(wm_class="music-downloader"),
        Match(wm_class="todos-electron"),
    ],
    border_focus="#7a7a7a",
    border_normal="#333333",
    border_width=3
)

#############################################################################################
# Widgets ###################################################################################
#############################################################################################

# widget.GenPollText(name="deadbeef", fmt="Music: {}", update_interval=3.0, func=lambda: subprocess.check_output("<script>").decode("UTF-8"))

widget_defaults = dict(font="FiraCode Nerd Font Mono", fontsize=13, padding=1, foreground="aaaabb")
extension_defaults = widget_defaults.copy()
top_bar = bar.Bar(
    [
        widget.GroupBox(active="ffffff", inactive="989898", fontsize=14,
                        highlight_method="line", highlight_color="005757"),
        widget.TaskList(fontsize=14, font="FiraCode Nerd Font Mono", foreground="aaaabb",
                        highlight_method="block", border="343434", margin_x=20, padding_x=4,
                        spacing=8, parse_text=lambda x : x[:30]),
        widget.Volume(fmt='Vol: {}', step=5, update_interval=0.4),
        # widget.Sep(padding=20),
        # widget.CPU(format="CPU: {load_percent}%", update_interval=1.0),
        # widget.Sep(padding=20),
        # widget.Memory(format="RAM: {MemUsed:.0f}/{MemTotal:.0f}GB", measure_mem='G', update_interval=1.0),
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
