#!/bin/zsh


SRC_DIR="$HOME/.config/emacs"
DEST_DIR="$1"

if ! [[ -d "$DEST_DIR" ]]; then
    echo "Usage copy-from-dotfile.sh DEST_DIR"
    exit 1
fi

# -r: recursive
# -v: verbose
# -z: compress
# -t: --times, preserve modification times
# -U: --atiems, preserve access (use) times
rsync -rvzt -U --progress "$SRC_DIR/elisp/" "$DEST_DIR/elisp/"
cp -p "$SRC_DIR/init.el" "$DEST_DIR/init.el"
