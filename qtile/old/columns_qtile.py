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
    Key([mod], "j", lazy.group.next_window(), desc="Focus next"),
    Key([mod], "k", lazy.group.prev_window(), desc="Focus previous"),

    # Move windows within the current group
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),  desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),  desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),    desc="Move window up"),

    # Resize focused window
    Key([mod, "control"], "h", lazy.layout.grow_left(),  desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),  desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(),    desc="Grow window up"),
    Key([mod], "n",            lazy.layout.normalize(),  desc="Reset all window sizes"),

    # Switch between workspaces
    Key([mod], "h",   lazy.screen.prev_group()),
    Key([mod], "l",   lazy.screen.next_group()),
    Key([mod], "Tab", lazy.screen.toggle_group()),

    # Layout toggle
    Key([mod], "u",     lazy.window.toggle_floating()),
    Key([mod], "i",     lazy.window.toggle_fullscreen()),
    Key([mod], "o",     lazy.layout.toggle_split()),
    Key([mod], "b",     lazy.hide_show_bar()),
    Key([mod], "space", lazy.next_layout()),

    # Others
    Key([mod],   "q",            lazy.window.kill(), desc="Kill focused window"),
    Key([mod,    "shift"], "r",  lazy.restart(),  desc="Restart Qtile"),
    Key([mod,    "shift"], "F4", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod,    "shift"], "q",  lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod,    "shift"], "F3", lazy.spawn("systemctl suspend")),
    Key([altKey, "shift"], "F4", lazy.spawn("systemctl poweroff")),

    # Audio
    Key([mod], "minus",       lazy.spawn("pamixer --decrease 5")),
    Key([mod], "equal",       lazy.spawn("pamixer --increase 5")),
    Key([mod,  "shift"], "0", lazy.spawn("pamixer --toggle-mute")),
    Key(
        [mod], "0",
        lazy.spawn("/home/pedro/dotfiles/scripts/change-default-sink.sh")
    ),

    # Programs
    Key([mod], "Return", lazy.spawn("alacritty"), desc="Launch terminal"),
    Key([mod], "r",      lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key(
        [mod], "p",
        lazy.spawn("rofi -show drun -modi drun -theme ~/.config/rofi/themes/my_dracula.rasi"),
        desc="Rofi run"
    ),
    Key([mod], "d",      lazy.spawn("dmenu_run -b -f -l 15 -p Apps")),
    Key([mod], "e",      lazy.spawn("thunar"), desc="File Manager"),

]

#############################################################################################
# Groups ####################################################################################
#############################################################################################

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        # Mod + [1..9] to focus group n
        Key(
            [mod], i.name,
            lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)
        ),
        # Mod + Shift + [1..9] to send window to group n
        Key(
            [mod, "shift"], i.name,
            lazy.window.togroup(i.name),
            desc="move focused window to group {}".format(i.name)
        ),
    ])

#############################################################################################
# Layouts ###################################################################################
#############################################################################################

layouts = [
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.Max(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Stack(num_stacks=2),
    # layout.Tile(ratio=0.5, border_focus="#666666", border_normal="#333333", border_width=3),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
    layout.Columns(border_focus="#959595", border_normal="#333333", border_width=2, insert_position=1)
]

# Drag floating layouts.
mouse = [
    # Super+Left_mouse moves
    Drag([mod], "Button1",  lazy.window.set_position_floating(), start=lazy.window.get_position()),
    # Super+Right_mouse resizes
    Drag([mod], "Button3",  lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
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

widget_defaults = dict(
    font="FiraCode Nerd Font Mono",
    fontsize=13,
    padding=1,
    foreground="aaaaff"
)

extension_defaults = widget_defaults.copy()

top_bar = bar.Bar(
    [
        widget.GroupBox(active="aaaaff", inactive="545454"),
        # widget.CurrentLayout(),
        # widget.Prompt(),
        widget.Spacer(),
        widget.Volume(fmt='Vol: {}'),
        widget.Sep(padding=20),
        widget.CPU(format="CPU: {load_percent}%", update_interval=1.0),
        # widget.ThermalSensor(
        #     foreground="aaaaff",
        #     fmt=' / {}',
        #     update_interval=3.0
        # ),
        widget.Sep(padding=20),
        widget.Memory(format="RAM: {MemUsed: .0f}MB", update_interval=1.0),
        # widget.ThermalSensor(
        #     foreground="aaaaff",
        #     fmt=' /  {}',
        #     tag_sensor="mem",
        #     update_interval=3.0
        # ),
        # widget.Sep(padding=20),
        # widget.ThermalSensor(
        #     foreground="aaaaff",
        #     fmt='GPU: {}',
        #     tag_sensor="edge",
        #     update_interval=3.0
        # ),
        widget.Sep(padding=20),
        widget.Clock(format="%d/%m/%Y (%a)", update_interval=60.0),
        widget.Sep(padding=20),
        widget.Clock(format="%R", update_interval=1.0),
        widget.Sep(padding=20),
        widget.Systray(foreground="aaaaff")
    ],
    24,
    background="#2f2838"
)

screens = [Screen(top=top_bar)]

#############################################################################################
# Configuartion Variables ###################################################################
#############################################################################################

dgroups_key_binder = None

dgroups_app_rules = []  # type: List

# Mouse hover wont change window focus
follow_mouse_focus = False

bring_front_click = False

cursor_warp = False

# Apps can ask for fullscreen
auto_fullscreen = True

focus_on_window_activation = "smart"

reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True


#############################################################################################
# Don't know don't care #####################################################################
#############################################################################################

# XXX: Gasp! We"re lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn"t work correctly. We may as well just lie
# and say that we"re a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java"s whitelist.
wmname = "LG3D"
