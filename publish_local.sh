#!/bin/bash

E_NO_ARGS=86           # Argument project not given
E_DIR_NOT_FOUND=87    # Folder for project was not found

MYSELF=`basename $0`   # This very script
VERSION="220"
TARGETDIR="example/ui/js/target/scala-2.12"

# Fail graciously if no project was specified
if [ $# -lt 1 ]; then
  echo "Usage: $MYSELF /path/to/web/folder/" >&2
  echo "(Make sure to include trailing slash in directory path)" >&2
  exit $E_NO_ARGS
else
  SERV="$1"
  JSDIR="${SERV}js"
  PROJECT=`basename "$1"`
fi

# Fail graciously if project folder does not exist
if [ ! -d "$SERV" ]; then
  echo "Folder $SERV was not found" >&2
  exit $E_DIR_NOT_FOUND
fi

[[ -d "$JSDIR" ]] || mkdir "$JSDIR"

cp -v "$TARGETDIR/cyby-example-ui-fastopt.js" "$JSDIR/cyby-ui_${VERSION}.js"
cp -v "$TARGETDIR/cyby-example-ui-fastopt.js.map" "$JSDIR/cyby-ui_${VERSION}.js.map"
cp -v "$TARGETDIR/cyby-example-ui-jsdeps.js" "$JSDIR/cyby-ui-jsdeps_${VERSION}.js"
cp -av html/css "$SERV"
cp -av html/chemdoodle "$SERV"
mv -v "${SERV}css/cyby.css" "${SERV}css/cyby_${VERSION}.css" 
mv -v "${SERV}css/doc.css" "${SERV}css/doc_${VERSION}.css" 
