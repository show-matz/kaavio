#!bin/bash

IN=$1
OUT=$2
CACHE=.cache
APP=./diagram

if [ ! -e ./${CACHE} ]; then
	mkdir ${CACHE}
fi

if [ -e ./${CACHE}/${OUT}.svg ]; then
	touch ./${CACHE}/${OUT}.svg
else
	diagram ${IN} > ./${CACHE}/${OUT}.svg
fi

echo "<div align='center'>"  > ./${OUT}
cat ./${CACHE}/${OUT}.svg   >> ./${OUT}
echo "</div>"               >> ./${OUT}
