
# COMSPEC 環境変数が定義されていれば Windows(cygwin) 環境とみなす
ifdef COMSPEC
	TARGET   = kaavio.exe
else
	TARGET   = kaavio
endif

build:
	make VERSION1=`git tag | tail -1 | cut -d '_' -f 2` \
         VERSION2=`git log --oneline | head -1 | cut -d ' ' -f 1` $(TARGET)

$(TARGET): utils.lisp kaavio-main.lisp sandbox-main.lisp make-executable.lisp
	sbcl --noinform \
         --no-sysinit \
         --eval "(defconstant +KAAVIO-VERSION+  \"$(VERSION1).$(VERSION2)\")" \
         --eval "(defconstant +OUTPUT-FILENAME+ \"$(TARGET)\")"  \
         --load utils.lisp \
         --load kaavio-main.lisp \
         --load sandbox-main.lisp \
         --load make-executable.lisp
