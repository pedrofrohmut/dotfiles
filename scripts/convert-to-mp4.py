#! /usr/bin/env python3

import subprocess
import argparse

# ffmpeg -i musializer.webm -preset fast -c:v libx264 -c:a aac -b:a 128k -b:v 5 -crf 23 -f mp4 musializer-1.mp4

def convert_webm_to_mp4(input_file, output_file):
    command = [
        'ffmpeg',
        '-i', input_file,
        '-preset', 'fast',
        '-c:v', 'libx264',
        '-c:a', 'aac',
        '-strict', 'experimental',
        '-b:a', '128k',
        '-b:v', '5',
        '-crf', '23',
        '-f', 'mp4',
        output_file
    ]

    subprocess.run(command)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Convert WebM to MP4 using FFmpeg')
    parser.add_argument('input_file', help='Path to the input WebM file')
    parser.add_argument('output_file', help='Path to the output MP4 file')

    args = parser.parse_args()

    convert_webm_to_mp4(args.input_file, args.output_file)
