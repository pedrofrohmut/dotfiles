#! /bin/bash

# This script is minimal and only works if you have only 2 output devices

# 1. list the sinks available
# 2. grep the line with index but that does not have the * (that means active)
# 3. grep and output the number from the line returned from step 2
inactive_sink_number=($(pacmd list-sinks | grep '. ^* index' | grep -o '[0-9]'))

# Activate the inactive_sink_number
pacmd set-default-sink ${inactive_sink_number}
