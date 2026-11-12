TARGET ?= target

# pp: pretty printer
${TARGET}/pp.c: src/* lib.ks pp.ks
	kast compile --target c --output ${TARGET}/pp.c pp.ks
${TARGET}/pp: ${TARGET}/pp.c
	${CC} \
		-I${KAST_LIB}/c-runtime/include \
		-lbacktrace \
		${KAST_LIB}/c-runtime/src/* \
		${TARGET}/pp.c \
		-o ${TARGET}/pp
pp: ${TARGET}/pp
	cp ${TARGET}/pp ./

# jq: query tool
${TARGET}/jq.c: src/* lib.ks jq.ks
	kast compile --target c --output ${TARGET}/jq.c jq.ks
${TARGET}/jq: ${TARGET}/jq.c
	${CC} \
		-I${KAST_LIB}/c-runtime/include \
		-lbacktrace \
		${KAST_LIB}/c-runtime/src/* \
		${TARGET}/jq.c \
		-o ${TARGET}/jq
jq: ${TARGET}/jq
	cp ${TARGET}/jq ./

# tests
${TARGET}/test.c: src/* lib.ks test.ks
	kast compile --target c --output ${TARGET}/test.c test.ks
${TARGET}/test: ${TARGET}/test.c
	${CC} \
		-I${KAST_LIB}/c-runtime/include \
		-lbacktrace \
		${KAST_LIB}/c-runtime/src/* \
		${TARGET}/test.c \
		-o ${TARGET}/test
test: ${TARGET}/test
	cp ${TARGET}/test ./

# clean
clean:
	rm -f pp
	rm -f jq
	rm -f test
	rm -rf ${TARGET}
