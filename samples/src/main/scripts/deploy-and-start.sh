#!/bin/sh
#
# Copyright (c) 2017-2024 Linagora
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
################################################################################
#
# 1/ CONFIGURE THE 'env.sh' FILE
# 2/ RUN THIS SCRIPT
#

ENV_FILE="./env.sh"
if [ -f "$ENV_FILE" ]; then
   . "$ENV_FILE"
else
	echo "Script to execute from its directory !!"
	exit 1
fi

# Stop H2 Database if needed
for i in `ps -ef | grep "h2-1.4.178.jar -tcp" | grep java | awk '{print $2}'`
do
	kill -9 $i
done
rm -rf ${FLOWABLE_H2_DB}.*

# Start H2 Database if needed
java -jar ${HOME}/.m2/repository/com/h2database/h2/1.4.178/h2-1.4.178.jar -tcp &

#
# Generate component configuration files
#
cat > ${PETALS_BC_SOAP_PROPERTIES} << EOF
notifyService.url=http://localhost:8080/samples-SOAP-services-${SAMPLE_VERSION}/services/notifyVacationService
archiveService.url=http://localhost:8080/samples-SOAP-services-${SAMPLE_VERSION}/services/archiveService
EOF


# Deploy all artifacts
petals-cli -h ${PETALS_HOSTNAME} -n ${PETALS_JMX_PORT} -u ${PETALS_JMX_USER} -p ${PETALS_JMX_PWD} - << EOF
connect -y

deploy -u mvn:https://repository.ow2.org/nexus/content/repositories/public@id=ow2.public!org.ow2.petals/petals-bc-soap/${BC_SOAP_VERSION}/zip -D propertiesFile=${PETALS_BC_SOAP_PROPERTIES}
deploy -u mvn:https://repository.ow2.org/nexus/content/repositories/public@id=ow2.public!org.ow2.petals/petals-se-flowable/${SE_FLOWABLE_VERSION}/zip -D jdbcUrl=jdbc:h2:tcp://localhost/${FLOWABLE_H2_DB}
EOF

# Hack Database for Flowable
cat > /tmp/flowable-hack.sql << EOF
ALTER TABLE ACT_GE_BYTEARRAY ALTER COLUMN NAME_ VARCHAR(512);
EOF
java -cp ${HOME}/.m2/repository/com/h2database/h2/1.4.178/h2-1.4.178.jar org.h2.tools.RunScript -url jdbc:h2:tcp://localhost/${FLOWABLE_H2_DB} -script /tmp/flowable-hack.sql -user sa

petals-cli -h ${PETALS_HOSTNAME} -n ${PETALS_JMX_PORT} -u ${PETALS_JMX_USER} -p ${PETALS_JMX_PWD} - << EOF
connect -y
deploy -u mvn:https://repository.ow2.org/nexus/content/repositories/public@id=ow2.public!org.ow2.petals.samples.flowable/sa-flowable-vacation-sample/${SAMPLE_VERSION}/zip
EOF


