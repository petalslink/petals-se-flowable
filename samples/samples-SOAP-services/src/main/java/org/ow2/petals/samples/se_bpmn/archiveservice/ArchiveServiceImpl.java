/**
 * Copyright (c) 2015 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.samples.se_bpmn.archiveservice;

import java.util.logging.Logger;

import javax.jws.WebService;
import javax.xml.datatype.XMLGregorianCalendar;

@WebService(serviceName = "archiveService", portName = "autogenerate", targetNamespace = "http://petals.ow2.org/samples/se-bpmn/archiveService", wsdlLocation = "file:/home/cdeneux/workspace/petals-trunk/components-sls/components/petals-se-activiti/samples/samples-SOAP-services/src/main/resources/wsdl/archiveService.wsdl", endpointInterface = "org.ow2.petals.samples.se_bpmn.archiveservice.Archive")
public class ArchiveServiceImpl implements Archive {

    private static final Logger LOG = Logger.getLogger(ArchiveServiceImpl.class.getName());

    @Override
    public java.lang.String archiveVacationRequest(final String enquirer, final long dayNumber,
            final XMLGregorianCalendar startDate, final String reason, final String vacationRequestId,
            final String approvedBy) {

        LOG.info("Archive vacation request #" + vacationRequestId + ":");
        LOG.info("\t- Enquirer: " + enquirer);
        LOG.info("\t- Day number: " + dayNumber);
        LOG.info("\t- Start date: " + startDate);
        LOG.info("\t- Reason: " + reason);
        LOG.info("\t- Approved by:" + approvedBy);

        return vacationRequestId;

    }

}
