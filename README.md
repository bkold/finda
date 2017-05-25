# finda
A file search tool for linux

## About

Finda was designed as a leaner, meaner find tool, as compared to the current default 'find' in Linux systems. In its current state it is 5x faster and a fraction of the binary size, at the cost of some features.

## Usage

The current flags are

    -e -- pattern, specify the regex to match
    -a -- prints the full path, if used
    -f -- matches against the full path, can be used with 'a' or else relative path will be used
    -d -- sets depth level
    -t -- sets thread count, for optimal performance use your total CPU count
    -p -- determines the printing scheme
        -0 -- prints to one line enclosing each entry in quotation marks; use for xargs
        -1 -- prints to one line enclosing each entry in quotation marks
        -2 -- prints one entry to a line, colors according to file type; use when printing to terminal
        -3 -- (Default) prints one entry to a line, no added characters

    --help - display use info

    Multiple paths are allowed

### Examples

    finda / -e=*find*
    finda / -e=g-*.ads -t8
    finda /home /bin -e=picture.png -t8 -d12
    finda / -e=*.ads -p0 | xargs grep Text_IO

## Installation

This program is written with Ada2012 and requires a Ada2012 GNAT compiler.
This project uses GNATgpr. This allows for simple and convenient compilation. To compile, simply run 'gprbuild -p'. Now it can be run with one of the above example commands. If you do not have gprbuild installed, and would rather use gnatmake, use the below commands in the project's root directory.

    gcc -c src/ada_c_dirent.c -Ofast -o obj/ada_c_dirent.o
    gnatmake src/main.adb -Ofast -D obj -o finda -largs obj/ada_c_dirent.o
