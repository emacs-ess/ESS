dnl
dnl Determine the emacs version we are running.
dnl Automatically substitutes @EMACS_VERSION@ with this number.
dnl
AC_DEFUN(AC_EMACS_VERSION, [
AC_MSG_CHECKING(for emacs version)
AC_CACHE_VAL(EMACS_cv_SYS_VERSION, [ version=`${EMACS} --version`
case "${version}" in
     GNU\ Emacs*) EMACS_cv_SYS_VERSION=`echo ${version} | cut -d' ' -f3- | cut -d'.' -f1-2`;;
     *) EMACS_cv_SYS_VERSION=`echo ${version} | cut -d' ' -f2`
;;
esac])
EMACS_VERSION=${EMACS_cv_SYS_VERSION}
AC_SUBST(EMACS_VERSION)
AC_MSG_RESULT(${EMACS_VERSION})
])

dnl
dnl Determine whether the specified version of Emacs supports packages
dnl or not.  Currently, only XEmacs 20.3 does, but this is a general
dnl check.
dnl
AC_DEFUN(AC_EMACS_PACKAGES, [
AC_MSG_CHECKING(for packages availability)
AC_CACHE_VAL(EMACS_cv_SYS_PACKAGES,[ packages=`${EMACS} -q -batch -eval "(princ (boundp 'package-path))"`
if test "${packages}" = "t"; then
   EMACS_cv_SYS_PACKAGES=yes
else
   EMACS_cv_SYS_PACKAGES=no
fi])
EMACS_PACKAGES=${EMACS_cv_SYS_PACKAGES}
AC_SUBST(EMACS_PACKAGES)
AC_MSG_RESULT(${EMACS_PACKAGES})
])

dnl
dnl Execute arbitrary emacs lisp
dnl
AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	retval=`${EMACS} -batch -eval "(princ ${elisp})"`
	EMACS_cv_SYS_$1=$retval
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

dnl
dnl Check whether a function exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_LIB, [
if test -z "$3"; then
	AC_MSG_CHECKING(for $2 in $1)
fi
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(condition-case nil (progn (require '$library) (fboundp '$2)) (error nil)),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
if test -z "$3"; then
	AC_MSG_RESULT($HAVE_$1)
fi
])

dnl
dnl Check whether a variable exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_VAR, [
AC_MSG_CHECKING(for $2 in $1)
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(condition-case nil (progn (require '$library) (boundp '$2)) (error nil)),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
AC_MSG_RESULT($HAVE_$1)
])

dnl
dnl Perform sanity checking and try to locate the custom and widget packages
dnl
AC_DEFUN(AC_CHECK_CUSTOM, [
AC_MSG_CHECKING(for acceptable custom library)
AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_CUSTOM,[
AC_EMACS_CHECK_LIB(widget,widget-convert-text,"noecho")
AC_EMACS_CHECK_LIB(wid_edit,widget-convert-text,"noecho")
if test "${HAVE_widget}" = "yes"; then
	EMACS_cv_ACCEPTABLE_CUSTOM=yes
else
	if test "${HAVE_wid_edit}" != "no"; then
		EMACS_cv_ACCEPTABLE_CUSTOM=yes
	else
		EMACS_cv_ACCEPTABLE_CUSTOM=no
	fi
fi
if test "${EMACS_cv_ACCEPTABLE_CUSTOM}" = "yes"; then
	AC_EMACS_LISP(widget_dir,(file-name-directory (locate-library \"widget\")),"noecho")
	EMACS_cv_ACCEPTABLE_CUSTOM=$EMACS_cv_SYS_widget_dir
fi
])
   CUSTOM=${EMACS_cv_ACCEPTABLE_CUSTOM}
   AC_MSG_RESULT("${CUSTOM}")
])
