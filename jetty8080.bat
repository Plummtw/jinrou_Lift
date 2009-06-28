@echo off
set MAVEN_OPTS=-Xms256M -Xmx1024M  
rem -Drun.mode=production
mvn jetty:run -Dfile.encoding=UTF-8 -Djetty.port=8080   
