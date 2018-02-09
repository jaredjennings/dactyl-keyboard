#!/bin/sh

USE_FSTL=1
if [ "$1" = "--no-fstl" ]; then
	USE_FSTL=0
	shift
fi
if [ -n "$1" ]; then
    SOURCE="$1"
    shift
else
    SOURCE="src/dactyl_keyboard/dactyl.clj"
fi
if [ -n "$1" ]; then
    OBN="$1"
    shift
else
    OBN=dactyl-blank-all
fi
OD=things
SCAD="${OD}/${OBN}.scad"
ASTL="${OD}/${OBN}.stl"
BSTL="${OD}/${OBN}-binary.stl"

step () {
    local name=$1
    shift
    local eta=unknown
    local sec=unknown
    local elapsedfn=".elapsed.$name"
    if [ -f $elapsedfn ]; then
	sec=$(cat $elapsedfn)
	eta=$(date -d "$sec seconds")
    fi
    echo
    echo
    echo "$(datef): (eta $eta, $sec seconds) $@"
    local t1=$(date +%s.%N)
    "$@"
    local t2=$(date +%s.%N)
    echo "$t2 $t1 - p" | dc > "$elapsedfn"
}

datef () {
    date +%H:%M:%S "$@"
}

while inotifywait -e close_write $SOURCE; do
    echo
    echo
    echo "$(datef): !!!!!!!!!!!!!!!!!!!!!!!!! $SOURCE modified; rebuilding"
    echo
    echo "(load-file \"$SOURCE\")" | step make-scad lein repl :connect 127.0.0.1:18237
    echo "\$\? was $?"
    if [ "$USE_FSTL" -eq 1 ]; then
        step render-stl openscad -o "$ASTL" --render "$SCAD"
        convert_stl "$ASTL"
        killall fstl
        fstl "$BSTL" &
    fi
    echo
    echo
    echo
    echo
    echo
    echo "$(datef): ????????????????????????? go"
done