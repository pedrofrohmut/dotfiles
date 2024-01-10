#! /usr/bin/env python3

import subprocess
import argparse

# ffmpeg -i musializer.webm -c:v libx264 -c:a aac -b:a 128k -b:v 5 -crf 23 -f mp4 musializer-1.mp4

def convert_webm_to_mp4(input_file, output_file):
    command = [
        'ffmpeg',
        '-i', input_file,
        '-c:v', 'libx264', '-c:a', 'aac', # video and audio codecs
        '-strict', 'experimental',        # for audio codec
        '-b:a', '128k',                   # audio bit rate
        '-b:v', '5',                      # video quality 0 best 10 worst
        '-crf', '23',                     # set frame rate
        '-f', 'mp4',                      # set format
        output_file
    ]

    subprocess.run(command)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Convert WebM to MP4 using FFmpeg')
    parser.add_argument('input_file', help='Path to the input WebM file')
    parser.add_argument('output_file', help='Path to the output MP4 file')

    args = parser.parse_args()

    convert_webm_to_mp4(args.input_file, args.output_file)
