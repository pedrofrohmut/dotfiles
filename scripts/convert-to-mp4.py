#! /usr/bin/env python3

import argparse
import os
import shutil
import subprocess
import shlex

def log(text):
    print("[Convert to MP4] " + text)

def is_ffmpeg_installed():
    return shutil.which('ffmpeg') is not None

def main(input_file, output_file=None, compress=False):
    if not is_ffmpeg_installed():
        log("Error: FFmpeg is not installed. Please install FFmpeg and try again.")
        return

    # Makes the output_file with the same name, but different extension, if
    # output_file is not provided
    if output_file is None:
        log("Not output name provided. Using input file name")
        input_file_name = os.path.splitext(input_file)[0]
        output_file = input_file_name + ".mp4"

    # Return a shell-escaped version of the string s (POSIX only)
    input_file = shlex.quote(input_file)
    output_file = shlex.quote(output_file)

    crf = "25" if not compress else "31"
    scale = "scale=1920:1080" if not compress else "scale=1280:720"

    # Shell command as a list of strings
    command = [
        'ffmpeg',
        '-i', input_file,
        '-preset', 'fast',         # output file is a little bigger but process is faster
        '-c:v', 'libx265',         # video codec (MP4 - libx264, HVEC - libx265)
        '-c:a', 'aac',             # audio codec
        '-strict', 'experimental', # for audio codec
        '-crf', crf,               # set frame rate (default: 28)
        '-vf', scale,              # scale the resolution (-1 for auto width, preserve the ratio, -1 doesnt work for shorts format == 'vetical')
        '-r', '30',                # fps
        '-f', 'mp4'                # set format
    ]

    if compress:
        log("Compress option enabled")
        command.extend([
            '-b:v', '1M',          # video bitrate
            '-b:a', '192k',        # audio bitrate
        ])

    command.append(output_file)

    # Cast the command from list to a string so it works on names with spaces
    str_command = " ".join(command)
    log("Executing: " + str_command)

    try:
        log("Start convertion.")
        subprocess.run(str_command, shell=True)
    except KeyboardInterrupt: # Normal exit and msg on Ctrl + C
        log("Convertion interrupt.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert WebM to MP4 using FFmpeg")

    # the path to the input file
    parser.add_argument("input_file", help="Path to the input WebM file")
    # Option to name the output file
    parser.add_argument("--output", "-o", help="Path to the output MP4 file (optional)")
    # Option to set compress flags to true
    parser.add_argument("--compress", "-c", action="store_true", help="Output file should compressing parameters")

    args = parser.parse_args()

    main(args.input_file, args.output, args.compress)
