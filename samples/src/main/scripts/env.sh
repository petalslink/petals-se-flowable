#!/bin/sh
#
# Copyright (c) 2017-2026 Linagora
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

PETALS_JMX_USER=petals
PETALS_JMX_PWD=petals
PETALS_HOSTNAME=localhost
PETALS_JMX_PORT=7700

PETALS_HOME=${HOME}/workspace/petals-trunk/container/petals-esb-default-zip/target/petals-esb-default-zip-5.2.1-SNAPSHOT/petals-esb-default-zip-5.2.1-SNAPSHOT
PETALS_DATA=${PETALS_HOME}/data/work
#PETALS_DATA=/var/lib/petals-esb/sample-0/work
PETALS_COMPONENT_CONFIG_FILES_HOME=${PETALS_HOME}/conf
#PETALS_COMPONENT_CONFIG_FILES_HOME=/etc/petals-esb/container-available/default

PETALS_BC_SOAP_PROPERTIES=${PETALS_COMPONENT_CONFIG_FILES_HOME}/bc-soap.properties

FLOWABLE_H2_DB=${PETALS_DATA}/h2-flowable.db

BC_SOAP_VERSION=5.1.0
SE_FLOWABLE_VERSION=1.4.0
SAMPLE_VERSION=1.4.0-1.0.0