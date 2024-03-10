from libqtile import bar, layout, widget
from libqtile.config import Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy

from os import path

mod = "mod4"
terminal = "alacritty"
HOMEDIR = path.expanduser("~/")

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html

    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),

    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),

    # Move floating windows
    Key([mod, "shift"], "Left",  lazy.window.move_floating(-10, 0)),
    Key([mod, "shift"], "Right", lazy.window.move_floating(10, 0)),
    Key([mod, "shift"], "Up",    lazy.window.move_floating(0, -10)),
    Key([mod, "shift"], "Down",  lazy.window.move_floating(0, 10)),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),

    # Resize floating windows
    Key([mod, "control"], "Left",  lazy.window.resize_floating(-10, 0)),
    Key([mod, "control"], "Right", lazy.window.resize_floating(10, 0)),
    Key([mod, "control"], "Up",    lazy.window.resize_floating(0, -10)),
    Key([mod, "control"], "Down",  lazy.window.resize_floating(0, 10)),

    #Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Switch between groups
    Key([mod], "p",   lazy.screen.prev_group()),
    Key([mod], "n",   lazy.screen.next_group()),
    Key([mod], "Tab", lazy.screen.toggle_group()),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"], "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod], "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key([mod, "shift"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),

    # Programs
    Key([mod], "Return", lazy.spawn(terminal),  desc="Launch terminal"),
    Key([mod], "w",      lazy.spawn("firefox"), desc="Web Browser"),
    Key([mod], "e",      lazy.spawn("thunar"),  desc="File Manager"),
    Key([mod], "c",      lazy.spawn("galculator"), desc="Calculator"),
    Key([mod], "d",      lazy.spawn("xfce4-appfinder")),
    Key([mod], "y",      lazy.spawn("emacsclient -c")),

    # Sound
    Key([mod], "equal",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%"),
        desc="increase volume on default sink by 5%"),
    Key([mod], "minus",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%"),
        desc="decrease volume on default sink by 5%"),
    Key([mod], "0",      lazy.spawn("bash {0}/dotfiles/scripts/new_change_sink.sh".format(HOMEDIR))),
]

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
    Key([mod,  "control"], "n",     lazy.layout.normalize()),
    Key([mod], "a",                 lazy.next_layout()),
])

#############################################################################################
# Groups ####################################################################################
#############################################################################################

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + group number = switch to group
            Key([mod], i.name, lazy.group[i.name].toscreen(), desc="Switch to group {}".format(i.name)),
            # mod1 + shift + group number = switch to & move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True), desc="Switch to & move focused window to group {}".format(i.name)),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + group number = move focused window to group
            Key([mod, "shift"], i.name, lazy.window.togroup(i.name), desc="move focused window to group {}".format(i.name)),
        ]
    )

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
    layout.Columns(border_focus="#ababab", border_normal="#323232",
                   border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=2),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

#############################################################################################
# Widgets ###################################################################################
#############################################################################################

widget_defaults = dict(font="Fira Code", fontsize=13, padding=3, foreground="aaaabb")
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(active="00ffff", inactive="bababa", fontsize=14,
                        highlight_method="line", highlight_color="005757"),
                widget.Prompt(),
                widget.TaskList(fontsize=13, font="Fira Code",
                        foreground="aaaabb", highlight_method="block", border="343434",
                        margin_x=10, padding_x=2, spacing=4, parse_text=lambda x : x[:25]),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Volume(fmt='Vol: {}', step=5, update_interval=0.4),
                widget.Sep(padding=20),
                widget.Clock(format="%d/%m/%Y [%a]", update_interval=60.0),
                widget.Sep(padding=20),
                widget.Clock(format="%R", update_interval=1.0, foreground="00ffff"),
                widget.Sep(padding=20),
                widget.Systray()
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    #Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = False
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
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
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
