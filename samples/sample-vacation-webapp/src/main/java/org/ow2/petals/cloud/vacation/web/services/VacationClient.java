/**
 * Copyright (c) 2015-2026 Linagora
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
package org.ow2.petals.cloud.vacation.web.services;

import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeFactory;

import org.ow2.petals.cloud.vacation.web.VacationRequest.PendingVacationRequest;
import org.ow2.petals.samples.se_flowable.vacation.vacationrequest.VacationRequestType;
import org.ow2.petals.samples.se_flowable.vacation.vacationservice.NewVacationResponse;
import org.ow2.petals.samples.se_flowable.vacation.vacationservice.VacationUpdateRequest;
import org.ow2.petals.samples.se_flowable.vacation.vacationservice.ValidationRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.client.core.support.WebServiceGatewaySupport;
import org.springframework.ws.soap.client.core.SoapActionCallback;

/**
 * 
 * @author vnoel
 *
 */
public class VacationClient extends WebServiceGatewaySupport {

    @Autowired
    private DatatypeFactory dtf;

    private static final org.ow2.petals.samples.se_flowable.vacation.vacationservice.ObjectFactory serviceOF = new org.ow2.petals.samples.se_flowable.vacation.vacationservice.ObjectFactory();

    private static final org.ow2.petals.samples.se_flowable.vacation.vacationrequest.ObjectFactory requestOF = new org.ow2.petals.samples.se_flowable.vacation.vacationrequest.ObjectFactory();

    public String create(final String enquirer, final PendingVacationRequest newRequest) {
        assert enquirer != null;
        assert newRequest != null;

        final VacationRequestType request = new VacationRequestType();
        request.setDayNumber(newRequest.getDayNumber());
        request.setEnquirer(enquirer);
        request.setReason(newRequest.getReason());

        final GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(newRequest.getStartDate());
        request.setStartDate(dtf.newXMLGregorianCalendar(cal));

        return ((NewVacationResponse) getWebServiceTemplate()
                .marshalSendAndReceive(requestOF.createVacationRequest(request),
                        new SoapActionCallback(
                                "http://petals.ow2.org/samples/se-flowable/vacation/vacationService/newVacationRequest")))
                                        .getVacationRequestId();
    }

    public void validate(final String id, final String manager, final boolean accepted, final String rejectionReason) {

        assert id != null;
        assert manager != null;

        final ValidationRequest request = serviceOF.createValidationRequest();
        request.setApprovedBy(manager);
        request.setApproval("" + accepted);
        request.setVacationRequestId(id);
        request.setRejectionReason(rejectionReason == null ? "" : rejectionReason);

        // TODO handle faults...
        getWebServiceTemplate().marshalSendAndReceive(request,
                new SoapActionCallback(
                        "http://petals.ow2.org/samples/se-flowable.vacation/vacationService/handleRequest"));

    }

    public void edit(final PendingVacationRequest original, final PendingVacationRequest pendingRequest) {
        assert original != null;
        assert pendingRequest != null;

        pendingRequest.setEnquirer(original.getEnquirer());
        pendingRequest.setId(original.getId());

        final VacationUpdateRequest request = createUpdateRequest(pendingRequest);
        request.setConfirmed("true");

        // TODO handle faults...
        getWebServiceTemplate().marshalSendAndReceive(request,
                new SoapActionCallback(
                        "http://petals.ow2.org/samples/se-flowable.vacation/vacationService/updateVacationRequest"));
    }

    public void cancel(final PendingVacationRequest pendingRequest) {
        assert pendingRequest != null;

        final VacationUpdateRequest request = createUpdateRequest(pendingRequest);
        request.setConfirmed("false");

        // TODO handle faults...
        getWebServiceTemplate().marshalSendAndReceive(request,
                new SoapActionCallback(
                        "http://petals.ow2.org/samples/se-flowable.vacation/vacationService/updateVacationRequest"));
    }

    private VacationUpdateRequest createUpdateRequest(final PendingVacationRequest pendingRequest) {
        final VacationUpdateRequest request = serviceOF.createVacationUpdateRequest();
        request.setDayNumber(pendingRequest.getDayNumber());
        request.setEnquirer(pendingRequest.getEnquirer());
        request.setReason(pendingRequest.getReason());
        final GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(pendingRequest.getStartDate());
        request.setStartDate(dtf.newXMLGregorianCalendar(cal));
        request.setVacationRequestId(pendingRequest.getId());
        return request;
    }

}
