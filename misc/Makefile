
TARGET     = kaavio
OPTIONS    = --noinform --no-sysinit
LOADLIST   = --load utils.lisp \
             --load kaavio-main.lisp \
             --load sandbox-main.lisp \
             --load make-executable.lisp

# COMSPEC 環境変数が定義されていれば Windows(cygwin) 環境とみなす
ifdef COMSPEC
	TARGET_SUFFIX = .exe
	PRE_EVAL      = --eval '(require :sb-posix)'
else
	TARGET_SUFFIX =
	PRE_EVAL      = 
endif

TARGET_DEF = --eval "(defconstant +OUTPUT-FILENAME+ \"$(TARGET)$(TARGET_SUFFIX)\")"

$(TARGET)$(TARGET_SUFFIX): utils.lisp kaavio-main.lisp sandbox-main.lisp make-executable.lisp
	sbcl $(OPTIONS) $(PRE_EVAL) $(TARGET_DEF) $(LOADLIST)

