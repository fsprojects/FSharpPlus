#!/usr/bin/env bash

set -eu

cd "$(dirname "$0")"

PAKET_BOOTSTRAPPER_EXE=.paket/paket.bootstrapper.exe
PAKET_EXE=.paket/paket.exe
FAKE_EXE=packages/FAKE/tools/FAKE.exe

FSIARGS=""
FSIARGS2=""
OS=${OS:-"unknown"}
if [ "$OS" != "Windows_NT" ]
then
  # Can't use FSIARGS="--fsiargs -d:MONO" in zsh, so split it up
  # (Can't use arrays since dash can't handle them)
  FSIARGS="--fsiargs"
  FSIARGS2="-d:MONO"
fi

run() {
  if [ "$OS" != "Windows_NT" ]
  then
    mono "$@"
  else
    "$@"
  fi
}

yesno() {
  # NOTE: Defaults to NO
  read -p "$1 [y/N] " ynresult
  case "$ynresult" in
    [yY]*) true ;;
    *) false ;;
  esac
}

set +e
run $PAKET_BOOTSTRAPPER_EXE
bootstrapper_exitcode=$?
set -e

if [ "$OS" != "Windows_NT" ] &&
       [ $bootstrapper_exitcode -ne 0 ] &&
       [ $(certmgr -list -c Trust | grep X.509 | wc -l) -le 1 ] &&
       [ $(certmgr -list -c -m Trust | grep X.509 | wc -l) -le 1 ]
then
  echo "Your Mono installation has no trusted SSL root certificates set up."
  echo "This may result in the Paket bootstrapper failing to download Paket"
  echo "because Github's SSL certificate can't be verified. One way to fix"
  echo "this issue would be to download the list of SSL root certificates"
  echo "from the Mozilla project by running the following command:"
  echo ""
  echo "    mozroots --import --sync"
  echo ""
  echo "This will import over 100 SSL root certificates into your Mono"
  echo "certificate repository."
  echo ""
  if yesno "Run 'mozroots --import --sync' now?"
  then
    mozroots --import --sync
  else
    echo "Attempting to continue without running mozroots. This might fail."
  fi
  # Re-run bootstrapper whether or not the user ran mozroots, because maybe
  # they fixed the problem in a separate terminal window.
  run $PAKET_BOOTSTRAPPER_EXE
fi

run $PAKET_EXE restore

[ ! -e build.fsx ] && run $PAKET_EXE update
[ ! -e build.fsx ] && run $FAKE_EXE init.fsx
run $FAKE_EXE "$@" $FSIARGS $FSIARGS2 build.fsx

