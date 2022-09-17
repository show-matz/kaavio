#!bin/bash

IN=$1
OUT=$2
CACHE=.cache

if [ ! -e ./${CACHE} ]; then
	mkdir ${CACHE}
fi

if [ -e ./${CACHE}/${OUT}.svg ]; then
	touch ./${CACHE}/${OUT}.svg
else
	kaavio ${IN} > ./${CACHE}/${OUT}.svg
fi

echo "<div align='center'>"  > ./${OUT}
cat ./${CACHE}/${OUT}.svg   >> ./${OUT}
echo "</div>"               >> ./${OUT}
