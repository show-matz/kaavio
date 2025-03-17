
# COMSPEC 環境変数が定義されていれば Windows(cygwin) 環境とみなす
ifdef COMSPEC
	TARGET   = kaavio.exe
else
	TARGET   = kaavio
endif

build:
	make VERSION1=`git tag | tail -1 | cut -d '_' -f 2` \
         VERSION2=`git log --oneline | head -1 | cut -d ' ' -f 1` $(TARGET)

bare-build:
	make VERSION1=`git tag | tail -1 | cut -d '_' -f 2` \
         VERSION2=`git log --oneline | head -1 | cut -d ' ' -f 1` bare-build-impl

$(TARGET): ../lib/*.stencil ./*.lisp
	sbcl --noinform \
         --no-sysinit \
         --eval "(defconstant +KAAVIO-VERSION+  \"$(VERSION1).$(VERSION2)\")" \
         --eval "(defconstant +OUTPUT-FILENAME+ \"$(TARGET)\")"  \
         --eval "(require :sb-posix)" \
         --eval "(require :kaavio)" \
         --load utils.lisp \
         --load kaavio-main.lisp \
         --load sandbox-main.lisp \
         --load make-executable.lisp

bare-build-impl: ../src/*.lisp ../lib/*.stencil ./*.lisp
	sbcl --noinform \
         --no-sysinit \
         --no-userinit \
         --eval "(defconstant +KAAVIO-VERSION+  \"$(VERSION1).$(VERSION2)\")" \
         --eval "(defconstant +OUTPUT-FILENAME+ \"$(TARGET)\")"  \
         --eval "(require :sb-posix)" \
         --eval "(require :uiop)" \
         --load bare-load.lisp \
         --load utils.lisp \
         --load kaavio-main.lisp \
         --load sandbox-main.lisp \
         --load make-executable.lisp
