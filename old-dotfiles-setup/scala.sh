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
echo
echo "       4.2 Scala Build Tool..."
echo
if type -p sbt; then
    echo '         Found sbt executable in PATH.'
    _sbt=sbt
else
    echo
    read -p '         No sbt found in PATH. Install it? [Y/n]' -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
      sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
      sudo apt update
      sudo apt install -y sbt
    else
      echo
      echo '      ...ok, skipping sbt setup!'
      echo
    fi
fi
