MYDIR="$(dirname "$(realpath "$0")")"
rm -f -- "$MYDIR/debhelper-build-stamp"
rm -f -- "$MYDIR/files"
rm -f -- "$MYDIR"/*.substvars
rm -r -f -- "$MYDIR/.debhelper"
rm -r -f -- "$MYDIR/pg-abris-deb"
