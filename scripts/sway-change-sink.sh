#! /usr/bin/env sh

default_sink_name=$(pactl get-default-sink)

sink_ids=($(pactl list sinks short | awk '{print $1}'))
sink_names=($(pactl list sinks short | awk '{print $2}'))

if [[ ${sink_names[0]} == ${default_sink_name} ]]; then
    new_sink_name=${sink_names[1]}
    new_sink_id=${sink_ids[1]}
else
    new_sink_name=${sink_names[0]}
    new_sink_id=${sink_ids[0]}
fi

pactl set-default-sink $new_sink_id

inputs_ids=$(pactl list sink-inputs short | awk '{print $1}')

for input_id in $inputs_ids; do
    pactl move-sink-input $input_id $new_sink_id
done
