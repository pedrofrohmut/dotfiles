#! /usr/bin/env bash

# 1. Get a short list of system sinks
# 2 and 3. tr replaces tabs and spaces for # so the line dont get devided
# when cahnged to an array
sinks=($(pactl list sinks short | tr '\t' '#' | tr ' ' '#' ))

# Get the default sink
default=$(pactl get-default-sink)

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

# Get sink input number
sink_input_number=$(pactl list sink-inputs short | grep -o '^[0-9]*')

# Required for some programs that wont change only with default-sink update
pacmd move-sink-input ${sink_input_number} ${inactive_sink_n}
