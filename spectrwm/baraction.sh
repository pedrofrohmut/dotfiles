volume() {
  volume=$(pamixer --get-volume)
  echo -e "$volume"
}

while :; do
  echo "Vol: $(volume)%"
  sleep 5
done
