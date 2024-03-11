#! /usr/bin/env bash

function get_default_sink() {
    # alsa_output of the default sink
    echo $(pactl get-default-sink)
}

function get_sink_input_number() {
    # Index number of the sink input
    echo $(pactl list sink-inputs short | grep -o '^[0-9]*')
}

function change_default_sink() {
    # 1. Get a short list of system sinks
    # 2 and 3. tr replaces tabs and spaces for # so the line dont get devided
    # when cahnged to an array
    sinks=($(pactl list sinks short | tr '\t' '#' | tr ' ' '#' ))

    default=$(get_default_sink)

    # Check with the default string to get active and inactive sink numbers
    if [[ ${sinks[0]} == *"$default"* ]]; then
        active_sink_n=$(echo ${sinks[0]} | grep -o '^[0-9]')
        inactive_sink_n=$(echo ${sinks[1]} | grep -o '^[0-9]')
    else
        active_sink_n=$(echo ${sinks[1]} | grep -o '^[0-9]')
        inactive_sink_n=$(echo ${sinks[0]} | grep -o '^[0-9]')
    fi

    # Change default sink to the inactive index
    pacmd set-default-sink $inactive_sink_n

    sink_input_number=$(get_sink_input_number)

    # Required for some programs that wont change only with default-sink update
    pacmd move-sink-input ${sink_input_number} ${inactive_sink_n}
}

function decrease_volume() {
    pactl set-sink-volume @DEFAULT_SINK@ -5%
}

function increase_volume() {
    pactl set-sink-volume @DEFAULT_SINK@ +5%
}

function toggle_mute() {
    default=$(get_default_sink)
    pactl set-sink-mute $default toggle
}

opt=$1

echo $opt

case "$opt" in
    "increase") increase_volume;;
    "decrease") decrease_volume;;
    "toggle-mute") toggle_mute;;
    "change") change_default_sink;;
    *) echo "Commands are: increase decrease toggle-mute change" ;;
esac

