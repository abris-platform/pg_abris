MYDIR="$(dirname "$(realpath "$0")")"
cd "$MYDIR/.."
debuild -uc -us -b
mkdir -p "$MYDIR/result"
mv "$MYDIR/../.."/pg-abris-deb* "$MYDIR/result"
"$MYDIR/clean.sh"
