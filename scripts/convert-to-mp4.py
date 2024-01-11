#! /usr/bin/env python3

# ffmpeg -i <input.webm> -preset fast -c:v libx265 -b:v 1M \
#        -c:a aac -strict experimental -crf 31 -vf scale=-1:720 -r 30 -f mp4 <output.mp4>

import argparse
import os
import shutil
import subprocess

def log(text):
    print("[Convert to MP4] " + text)

def is_ffmpeg_installed():
    return shutil.which('ffmpeg') is not None

def convert_webm_to_mp4(input_file, output_file=None):
    if not is_ffmpeg_installed():
        log("Error: FFmpeg is not installed. Please install FFmpeg and try again.")
        return

    # Makes the output_file with the same name, but different extension, if
    # output_file is not provided
    if output_file is None:
        input_file_name = os.path.splitext(input_file)[0]
        output_file = input_file_name + ".mp4"

    command = [
        'ffmpeg',
        '-i', input_file,
        '-preset', 'fast',         # output file is a little bigger but process is faster
        '-c:v', 'libx265',         # video codec (MP4 - libx264, HVEC - libx265)
        '-b:v', '1M',              # video bitrate
        '-c:a', 'aac',             # audio codec
        '-b:a', '192k',            # audio bitrate
        '-strict', 'experimental', # for audio codec
        '-crf', '31',              # set frame rate (default: 28)
        '-vf', 'scale=-1:720',     # scale the resolution (-1 for auto width, preserve the ratio)
        '-r', '30',                # fps
        '-f', 'mp4',               # set format
        output_file
    ]

    try:
        log("Start convertion.")
        subprocess.run(command)
    except KeyboardInterrupt:
        log("Convertion interrupt.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Convert WebM to MP4 using FFmpeg')
    parser.add_argument('input_file', help='Path to the input WebM file')
    parser.add_argument('output_file', nargs='?', help='Path to the output MP4 file (optional)')

    args = parser.parse_args()

    convert_webm_to_mp4(args.input_file, args.output_file)
