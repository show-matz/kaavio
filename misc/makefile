
TARGET     = diagram
OPTIONS    = --noinform --no-sysinit
LOADLIST   = --load cl-diagram-main.lisp --load make-executable.lisp
PRE_EVAL   = 

# COMSPEC 環境変数が定義されていれば Windows(cygwin) 環境とみなす
ifdef COMSPEC
	TARGET   += .exe
	PRE_EVAL  = --eval '(require :sb-posix)'
endif

TARGET_DEF = --eval "(defconstant +OUTPUT-FILENAME+ \"$(TARGET)\")"

$(TARGET): cl-diagram-main.lisp make-executable.lisp
	sbcl $(OPTIONS) $(PRE_EVAL) $(TARGET_DEF) $(LOADLIST)

