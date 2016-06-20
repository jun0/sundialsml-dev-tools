#!/bin/sh -eu

HERE=$(dirname "$0")
if ! [ -x "$HERE/sundials-trace" ] \
        || [ "$HERE/Trace.hs" -nt "$HERE/sundials-trace" ]; then
    echo "Recompiling Trace.hs..."
    ghc -O2 -o "$HERE/sundials-trace" "$HERE/Trace.hs"
fi

if [ "x$1" = "x-c" ]; then
    # Instrument C.
    shift
    if ! [ -d "$1" ] || ! [ -f "$1/CMakeLists.txt" ] || ! [ -d "$1/.git" ];then
        echo 1>&2 "usage: $0 -c <git-controlled src dir> <sundials build dir>"
        exit 1
    fi
    if ! [ -d "$2" ] || ! [ -f "$2/CMakeCache.txt" ]; then
        echo 1>&2 "usage: $0 -c <sundials src dir> <sundials build dir>"
        exit 2
    fi

    EXPORTS=/dvl/sundials/exports
    rm -f "$EXPORTS"

    find "$1" -name '*.h' | grep -v sundials_config.h | while read i; do
        echo "<$i>"
        /dvl/sundials/sundials-trace -x < "$i" >> "$EXPORTS"
    done

    (cd "$1" && git reset --hard)
    find "$1" -name '*.c' | grep -v sundials_nvector.c | while read i; do
        echo "[$i]"
        /dvl/sundials/sundials-trace -c "$EXPORTS" < "$i" > "$i.tmp"
        mv "$i.tmp" "$i"
    done
else
    # Instrument headers.
    if ! [ -d "$1" ] || ! [ -d "$1/include" ]; then
        echo 1>&2 "usage: $0 <install dir> <sundials build dir>"
        exit 1
    fi
    if ! [ -d "$2" ] || ! [ -f "$2/CMakeCache.txt" ]; then
        echo 1>&2 "usage: $0 <install dir> <sundials build dir>"
        exit 2
    fi

    cd "$1/include"

    if [ "$#" -gt 2 ]; then
        args="$3"
    else
        args=
    fi

    find . -name '*.h' | grep -v sundials_config.h | while read i; do
        [ -f $2/include/$i ] && f=$2/include/$i
        [ -f $2/../src/include/$i ] && f=$2/../src/include/$i
        [ -f $2/../src/src/$i ] && f=$2/../src/src/$i
        echo "[$f] -> $i"
        /dvl/sundials/sundials-trace $args < $f > $i
    done
fi
