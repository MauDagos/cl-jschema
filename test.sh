#!/usr/bin/env bash

CL_IMPLEMENTATION="$1"

CLJSCHEMA_DIR=$(pwd)
CLJSCHEMA_ASD="$CLJSCHEMA_DIR/cl-jschema.asd"

LOAD_QUICKLISP_EXPR="(load #p\"~/quicklisp/setup.lisp\")"
SETUP_EXPR="
(progn
  (asdf:load-asd #p\"$CLJSCHEMA_ASD\")
  (ql:quickload :cl-jschema/tests))"

test_sbcl() {
    echo "Running CL-JSCHEMA tests on SBCL"

    SBCL_TEST_EXPR="
    (if (fiveam:run! :cl-jschema.tests)
        (sb-ext:exit :code 0)
        (sb-ext:exit :code 1))"

    sbcl --noinform \
         --end-runtime-options \
         --no-sysinit \
         --no-userinit \
         --disable-debugger \
         --eval "$LOAD_QUICKLISP_EXPR" \
         --eval "$SETUP_EXPR" \
         --eval "$SBCL_TEST_EXPR"
}

test_alisp() {
    echo "Running CL-JSCHEMA tests on ALISP"

    ALISP_SETUP_EXPR="
    (handler-bind ((error
                     (lambda (e)
                       (format *error-output* \"~&Error while compiling:~%~a~%\" e)
                       (top-level.debug:zoom *error-output* :all t :brief t)
                       (excl:exit 1))))
      $SETUP_EXPR)"

    ALISP_TEST_EXPR="
    (handler-bind ((error
                     (lambda (e)
                       (format *error-output* \"~&Error while running tests:~%~a~%\" e)
                       (top-level.debug:zoom *error-output* :all t :brief t)
                       (excl:exit 1))))
      (if (fiveam:run! :cl-jschema.tests)
          (excl:exit 0)
          (excl:exit 1))"

    alisp -qq \
          -e "$LOAD_QUICKLISP_EXPR" \
          -e "$ALISP_SETUP_EXPR" \
          -e "$ALISP_TEST_EXPR"
}

case $CL_IMPLEMENTATION in
    sbcl) test_sbcl ;;
    alisp) test_alisp ;;
    *) echo "Unknown implementation"
       exit 1
       ;;
esac
