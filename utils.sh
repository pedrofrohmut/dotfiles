# Find all dirs by name
find . -name node_modules -type d

# Seek and Destroy all folders by name
find . -name node_modules -type d -exec rm -rf {} +

# Copy current dir to system clipboard (require xclip)
pwd | xclip -selection clipboard

# List dir sizes, biggest first and first 20
du -h -d 1 | sort -hr | head --lines 20

# Tar some file To a target directory
tar -xzvf input.tar.gz -C target_dir

# Last 10 installed files on arch based with pacman
# check pacman log, grep for installed lines, get last 10
cat /var/log/pacman.log | grep "installed" | tail -n 10
