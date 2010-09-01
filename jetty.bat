@echo off
set MAVEN_OPTS=-Xms256M -Xmx1024M -XX:MaxPermSize=256M
rem -Drun.mode=production
rem -Djetty.port=8080 
:jetty
mvn jetty:run -Dfile.encoding=UTF-8 -Djetty.port=80   
goto jetty