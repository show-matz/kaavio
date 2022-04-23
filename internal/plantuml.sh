#!bin/bash

IN=$1
OUT=$2
CACHE=.cache
JAR=~/lib/plantuml-1.2022.2.jar
OPT=-Dfile.encoding=UTF-8
CFG=
#CFG=-config uml.theme

if [ ! -e ./${CACHE} ]; then
	mkdir ${CACHE}
fi

if [ -e ./${CACHE}/${OUT}.svg ]; then
	touch ./${CACHE}/${OUT}.svg
else
	cat ${IN} | java ${OPT} -jar ${JAR} ${CFG} -pipe -tsvg >> ./${CACHE}/${OUT}.svg
fi

echo "<div align='center'>"  > ./${OUT}
cat ./${CACHE}/${OUT}.svg   >> ./${OUT}
echo "</div>"               >> ./${OUT}

