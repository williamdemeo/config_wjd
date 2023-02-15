#!/bin/bash

if type -p java; then
  echo '         Found Java executable in PATH.'
  _java=java
elif [[ -n "$JAVA_HOME" ]] && [[ -x "$JAVA_HOME/bin/java" ]];  then
  echo '         Found Java executable in JAVA_HOME.'
  _java="$JAVA_HOME/bin/java"
else
  echo
  read -p '         No Java found. Install it? [Y/n]' -n 1 -r
  if [[ $REPLY =~ ^[Yy]$ ]];then
    sudo apt install -y openjdk-9-jdk
  else
    echo
	  echo '         WARNING: skipping java setup!'
	  echo
  fi
fi
