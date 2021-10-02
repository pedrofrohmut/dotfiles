volume() {
  volume=$(amixer get Master | awk -F'[][]' 'END{ print $4":"$2 }' | sed 's/on://g')
  echo -e "$volume"
}

while :; do
  echo "Vol: $(volume)"
  sleep 5
done
