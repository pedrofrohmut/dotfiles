#! /usr/bin/env bash

suspended_sink_id="$(pactl list sinks short | grep --invert-match 'RUNNING' | awk '{print $1}')"
pactl set-default-sink "$suspended_sink_id"
