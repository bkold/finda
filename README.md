# finda
A file search tool for linux

## About

Finda was designed as a leaner, meaner find tool, as compared to the current default 'find' in Linux systems. In its current state it is 3x faster and a fraction of the binary size, at the cost of some features.

## Usage

The current flags are

    -e - pattern, specify the regex to match
    -t - number of threads to use
    -d - maximum depth to search to
    -p - print results with colors

    --help - display use info

    Multiple paths are allowed

### Examples

    finda / -e=*find*
    finda / -e=g-*.ads -t=8
    finda /home /bin -e=picture.png -t=8 -d=12

## Installation

This program is written with Ada2012 and requires a Ada2012 GNAT compiler.
This project uses GNATgpr. This allows for simple and convenient compilation. To compile, simply run 'gprbuild -p'. Now it can be run with one of the above example commands. If you do not have gprbuild installed, and would rather use gnatmake, use the below commands in the project's root directory.

    gcc -c src/ada_c_dirent.c -Ofast -o obj/ada_c_dirent.o
    gnatmake src/main.adb -Ofast -D obj -o finda -largs obj/ada_c_dirent.o
