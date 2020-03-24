#!/usr/bin/bash

pushd ./src

# --------------------------------------------------------------------------------------
TARGET_FILE="cl-diagram.lisp"

LINE1=`grep -E -n ' BEGIN EXPORT$' ./${TARGET_FILE} | perl -pe 's/^([0-9]+):.+$/\1/'`
LINE2=`grep -E -n   ' END EXPORT$' ./${TARGET_FILE} | perl -pe 's/^([0-9]+):.+$/\1/'`
TOTAL=`cat ./${TARGET_FILE} | wc -l`
LINE2=$((TOTAL - LINE2 + 1))

head -${LINE1} ./${TARGET_FILE}                  > ./${TARGET_FILE}.new
grep '^#|EXPORT' *.lisp | perl -pe 's/^.+\|#//' >> ./${TARGET_FILE}.new
tail -${LINE2} ./${TARGET_FILE}                 >> ./${TARGET_FILE}.new

mv ./${TARGET_FILE}      ./${TARGET_FILE}.bak
mv ./${TARGET_FILE}.new  ./${TARGET_FILE}


# --------------------------------------------------------------------------------------
TARGET_FILE="cl-diagram.asd"

LINE1=`grep -E -n ' BEGIN COMPONENTS$' ./${TARGET_FILE} | perl -pe 's/^([0-9]+):.+$/\1/'`
LINE2=`grep -E -n   ' END COMPONENTS$' ./${TARGET_FILE} | perl -pe 's/^([0-9]+):.+$/\1/'`
TOTAL=`cat ./${TARGET_FILE} | wc -l`
LINE2=$((TOTAL - LINE2 + 1))

head -${LINE1} ./${TARGET_FILE}               > ./${TARGET_FILE}.new
grep '^#|ASD' *.lisp | perl -pe 's/^.+\|#//' >> ./${TARGET_FILE}.new
tail -${LINE2} ./${TARGET_FILE}              >> ./${TARGET_FILE}.new

mv ./${TARGET_FILE}      ./${TARGET_FILE}.bak
mv ./${TARGET_FILE}.new  ./${TARGET_FILE}

popd

