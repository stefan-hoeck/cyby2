#!/bin/bash

E_NO_ARGS=86           # Argument project not given
E_DIR_NOT_FOUND=87    # Folder for project was not found

MYSELF=`basename $0`   # This very script
VERSION="220"
TARGETDIR="example/ui/js/target/scala-2.12"

# Fail graciously if no project was specified
if [ $# -lt 1 ]; then
  echo "Usage: $MYSELF /path/to/web/folder" >&2
  exit $E_NO_ARGS
else
  DATADIR="$1"
  PAGEDIR="${DATADIR}/page"
  JSDIR="${PAGEDIR}/js"
  PROJECT=`basename "$1"`
fi

# Fail graciously if project folder does not exist
if [ ! -d "$DATADIR" ]; then
  echo "Folder $DATADIR was not found" >&2
  exit $E_DIR_NOT_FOUND
fi

[[ -d "$JSDIR" ]] || mkdir -p "$JSDIR"

sbt -mem 4096 uiExJS/fastOptJS
sbt -mem 4096 uiExJS/fullOptJS
sbt -mem 4096 "uiExJVM/run $PAGEDIR"
sbt -mem 4096 serverEx/assembly


cp -v "$TARGETDIR/cyby-example-ui-fastopt.js" "$JSDIR/cyby-ui_${VERSION}.js"
cp -v "$TARGETDIR/cyby-example-ui-fastopt.js.map" "$JSDIR/cyby-ui_${VERSION}.js.map"
cp -v "$TARGETDIR/cyby-example-ui-jsdeps.js" "$JSDIR/cyby-ui-jsdeps_${VERSION}.js"
cp -av html/css "$PAGEDIR"
cp -av html/chemdoodle "$PAGEDIR"
mv -v "${PAGEDIR}/css/cyby.css" "${PAGEDIR}/css/cyby_${VERSION}.css" 
mv -v "${PAGEDIR}/css/doc.css" "${PAGEDIR}/css/doc_${VERSION}.css" 

cp -rv example/db/* "$DATADIR"
