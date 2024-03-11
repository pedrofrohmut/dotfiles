#! /usr/bin/env sh

# List the sinks in short form => replace tabs by # => replace spaces by #
# so it wont split the output in only 1 item for 1 sink
sinks=($(pactl list sinks short | tr '\t' '#' | tr ' ' '#' ))

# Get default sink name to compare
default=$(pactl get-default-sink)

# Expected to be only two sinks so an if is enough
# Get the sink number of the not-default sink
if [[ ${sinks[0]} == *"$default"* ]]; then
    sink_n=$(echo ${sinks[1]} | awk -F '#' '{print $1}')
else
    sink_n=$(echo ${sinks[0]} | awk -F '#' '{print $1}')
fi

# Set the not-default sink as default sink. (Toggle Between Effect)
pactl set-default-sink $sink_n
