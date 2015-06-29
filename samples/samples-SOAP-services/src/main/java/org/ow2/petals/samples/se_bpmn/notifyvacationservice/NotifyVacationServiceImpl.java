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
package org.ow2.petals.samples.se_bpmn.notifyvacationservice;

import java.util.logging.Logger;

import javax.jws.WebService;
import javax.xml.datatype.XMLGregorianCalendar;

@WebService(serviceName = "notifyVacationService", portName = "autogenerate", targetNamespace = "http://petals.ow2.org/samples/se-bpmn/notifyVacationService", wsdlLocation = "classpath:wsdl/notifyVacationService.wsdl", endpointInterface = "org.ow2.petals.samples.se_bpmn.notifyvacationservice.NotifyVacation")
public class NotifyVacationServiceImpl implements NotifyVacation {

    private static final Logger LOG = Logger.getLogger(NotifyVacationServiceImpl.class.getName());

    @Override
    public String vacationRequestUpdated(final String enquirer, final long dayNumber,
            final XMLGregorianCalendar startDate, final String reason, final String vacationRequestId,
            final String confirmed) {

        LOG.info("Vacation request updated #" + vacationRequestId + ":");
        LOG.info("\t- Enquirer: " + enquirer);
        LOG.info("\t- Day number: " + dayNumber);
        LOG.info("\t- Start date: " + startDate);
        LOG.info("\t- Reason: " + reason);
        LOG.info("\t- Confirmed:" + confirmed);

        return vacationRequestId;
    }

    @Override
    public String newVacationRequest(final String enquirer, final long dayNumber, final XMLGregorianCalendar startDate,
            final String reason, final String vacationRequestId) {

        LOG.info("New vacation request #" + vacationRequestId + ":");
        LOG.info("\t- Enquirer: " + enquirer);
        LOG.info("\t- Day number: " + dayNumber);
        LOG.info("\t- Start date: " + startDate);
        LOG.info("\t- Reason: " + reason);

        return vacationRequestId;
    }

    @Override
    public OkResponse vacationRequestApproved(final VacationRequestApproved parameters) {

        LOG.info("Vacation request approved #" + parameters.getVacationRequestId() + ":");
        LOG.info("\t- Enquirer: " + parameters.getEnquirer());
        LOG.info("\t- Day number: " + parameters.getDayNumber());
        LOG.info("\t- Start date: " + parameters.getStartDate());
        LOG.info("\t- Reason: " + parameters.getReason());
        LOG.info("\t- Approved by: " + parameters.getApprovedBy());

        final OkResponse okResponse = new OkResponse();
        okResponse.setOk(parameters.getApprovedBy().isEmpty() ? "ko" : "ok");
        return okResponse;
    }

}
