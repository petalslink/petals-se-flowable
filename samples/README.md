#
# Copyright (c) 2017-2019 Linagora
#
# This program/library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 2.1 of the License, or (at your
# option) any later version.
#
# This program/library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
# for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program/library; If not, see http://www.gnu.org/licenses/
# for the GNU Lesser General Public License version 2.1.
#

To run the following sample:
  1/ Deploy following web-app into a servlet container as Tomcat 8 running on localhost:
     a/ sample-vacation-webapp
     b/ sample-SOAP-services
  2/ Start a clean and fresh Petals ESB container on localhost
  3/ Set your environnement in file 'env.sh':
       - PETALS_HOME
  4/ Use the script 'deploy-and-start.sh' to
       - Launch a H2 database server
       - Deploy all needed Petals artifacts
  5/ Let's go to http://localhost:8080/sample-vacation-webapp-1.0.0-1.0.0-SNAPSHOT
  
Note: sample-vacation-webapp can also be started through Spring Boot using the following
      Maven command:
         > mvn spring-boot:run
         
      It will be available at http://localhost:8079

Use the same users as in Flowable for the managers
