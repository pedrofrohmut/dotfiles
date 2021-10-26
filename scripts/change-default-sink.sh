#! /bin/bash

# This script is minimal and only works if you have only 2 output devices

# 1. list the sinks available
# 2. grep the line with index but that does not have the * (that means active)
# 3. grep and output the number from the line returned from step 2
inactive_sink_number=$(pacmd list-sinks | grep '. ^* index' | grep -o '[0-9]')
echo $inactive_sink_number

# 1. list the sink inputs available
# 2. grep the line with the word 'index' in it
# 3. grep output the number from the line returned from step 2
sink_input_number=$(pacmd list-sink-inputs | grep 'index' | grep -o '[0-9][0-9]')
echo $sink_input_number

# Set the inactive sink as default sink
# Redirects the input input to the new default sink
pacmd set-default-sink ${inactive_sink_number} && \
  pacmd move-sink-input ${sink_input_number} ${inactive_sink_number} 
