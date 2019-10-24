#!/bin/bash

E_NO_ARGS=86           # Argument project not given
E_DIR_NOT_FOUND=87    # Folder for project was not found

MYSELF=`basename $0`   # This very script

# Fail graciously if no project was specified
if [ $# -lt 1 ]; then
  echo "Usage: $MYSELF /path/to/web/folder" >&2
  exit $E_NO_ARGS
else
  DIR="$1"
  PROJECT=`basename "$1"`
fi

# Fail graciously if project folder does not exist
if [ ! -d "$DIR" ]; then
  echo "Folder $DIR was not found" >&2
  exit $E_DIR_NOT_FOUND
fi


sbt -mem 4096 uiExJS/fastOptJS
sbt -mem 4096 uiExJS/fullOptJS
sbt -mem 4096 "uiExJVM/run $DIR cyby-serv"
sbt -mem 4096 serverEx/assembly
