@echo off
set MAVEN_OPTS=-Xms512M -Xmx1024M  
mvn test -Dfile.encoding=UTF-8
