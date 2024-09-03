#!/bin/zsh


SRC_DIR="$1"
DEST_DIR="$HOME/.config/emacs"

if ! [[ -d "$SRC_DIR" ]]; then
    echo "Usage copy-to-dotfile.sh SRC_DIR"
    exit 1
fi

# -r: recursive
# -v: verbose
# -z: compress
# -t: --times, preserve modification times
# -U: --atiems, preserve access (use) times
rsync -rvzt -U --progress "$SRC_DIR/elisp/" "$DEST_DIR/elisp/"
cp -p "$SRC_DIR/init.el" "$DEST_DIR/init.el"

