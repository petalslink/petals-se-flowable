/**
 * Copyright (c) 2015-2023 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.samples.se_flowable.archiveservice;

import java.util.logging.Logger;

import javax.jws.WebService;
import javax.xml.datatype.XMLGregorianCalendar;

import org.ow2.petals.samples.se_flowable.vacation.archiveservice.Archive;

@WebService(serviceName = "archiveService", portName = "autogenerate", targetNamespace = "http://petals.ow2.org/samples/se-flowable/vacation/archiveService", wsdlLocation = "classpath:wsdl/archiveService.wsdl", endpointInterface = "org.ow2.petals.samples.se_flowable.vacation.archiveservice.Archive")
public class ArchiveServiceImpl implements Archive {

    private static final Logger LOG = Logger.getLogger(ArchiveServiceImpl.class.getName());

    @Override
    public java.lang.String archiveVacationRequest(final String enquirer, final long dayNumber,
            final XMLGregorianCalendar startDate, final String reason, final String vacationRequestId,
            final String approvedBy) {

        final StringBuilder message = new StringBuilder();
        message.append("Archive vacation request #" + vacationRequestId + ":").append('\n');
        message.append("\t- Enquirer: " + enquirer).append('\n');
        message.append("\t- Day number: " + dayNumber).append('\n');
        message.append("\t- Start date: " + startDate).append('\n');
        message.append("\t- Reason: " + reason).append('\n');
        message.append("\t- Approved by:" + approvedBy);

        LOG.info(message.toString());

        return vacationRequestId;

    }

}
