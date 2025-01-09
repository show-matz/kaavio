#!/usr/bin/bash

function extract-reference {
	local RELPATH="$1"
	local OUTPATH="$2"
    local LSTFILE="$3"
    local TMPFILE=${OUTPATH}/$$.tmp
	pushd ${RELPATH}  > /dev/null
	local TAG1="BEGIN TURNUP"
	local TAG2="END TURNUP"
	for ENTRY in `find . -name '*.lisp' -or -name '*.stencil' \
                   | sort \
                   | xargs grep -nE '^;;-+ (BEGIN|END) TURNUP$' \
                   | perl -0pe "s{^\./(.+?\.(?:lisp|stencil)):(\d+):;;-+ ${TAG1}\n\./\1:(\d+):;;-+ ${TAG2}$}{\1,\2,\3}gm"`
	do
		local FILE_NAME=`echo  $ENTRY | cut -d , -f 1`
		local BEGIN_LINE=`echo $ENTRY | cut -d , -f 2`
		local END_LINE=`echo   $ENTRY | cut -d , -f 3`

        echo "operating ${FILE_NAME}:${BEGIN_LINE}:${END_LINE}..."

		cat ${FILE_NAME} \
			| head -$((END_LINE - 1)) \
			| tail -$((END_LINE - BEGIN_LINE - 1)) \
			| perl -pe 's/^;;(.*)$/\1/' > ${TMPFILE}

        local ENTRY_NAME=$(cat ${TMPFILE} | head -1 | perl -pe 's/^#### ([^ \n]+) (.*)$/\2/')
        local ENTRY_FILE="${FILE_NAME}.${BEGIN_LINE}-${END_LINE}.data"

        cat ${TMPFILE}  > ${OUTPATH}/${ENTRY_FILE}
        echo ""        >> ${OUTPATH}/${ENTRY_FILE}
        cat ${TMPFILE} \
            | head -1 \
            | perl -pe 's/^#### ([^ \n]+) (.*)$/<!-- autolink: [\2](#\1 \2) -->/' \
                                   >> ${OUTPATH}/${ENTRY_FILE}
        echo ""                    >> ${OUTPATH}/${ENTRY_FILE}
        echo "\${BLANK_PARAGRAPH}" >> ${OUTPATH}/${ENTRY_FILE}
        echo ""                    >> ${OUTPATH}/${ENTRY_FILE}

        echo "${ENTRY_NAME}	${ENTRY_FILE}" >> ${LSTFILE}
	done
    rm -f ${TMPFILE}
	popd  > /dev/null
}


BASEPATH="$(cd "$(dirname "$(realpath "${BASH_SOURCE:-0}")")"; pwd)"
REFDATA=.refdata
OUTFILE="${BASEPATH}/reference.md"
LSTFILE="${BASEPATH}/${REFDATA}/$$.LST"

if [ ! -e ./${REFDATA} ]; then
    mkdir ${REFDATA}
fi

rm -f ./${REFDATA}/*.data
rm -f ./${REFDATA}/*.list

extract-reference ../src ${BASEPATH}/${REFDATA} ${LSTFILE}
extract-reference ../lib ${BASEPATH}/${REFDATA} ${LSTFILE}

rm -f ${OUTFILE}
for FILENAME in `cat ${LSTFILE} | sort | cut -d '	' -f 2`
do
    cat ${BASEPATH}/${REFDATA}/${FILENAME} >> ${OUTFILE}
done
