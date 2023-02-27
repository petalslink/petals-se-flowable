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
package org.ow2.petals.cloud.vacation.web.services;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.DatatypeFactory;

import org.ow2.petals.cloud.vacation.web.VacationRequest;
import org.ow2.petals.cloud.vacation.web.VacationRequest.ArchivedVacationRequest;
import org.ow2.petals.cloud.vacation.web.VacationRequest.PendingVacationRequest;
import org.ow2.petals.cloud.vacation.web.VacationRequest.RefusedVacationRequest;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstances;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.ObjectFactory;
import org.ow2.petals.components.flowable.generic._1.ProcessInstance;
import org.ow2.petals.components.flowable.generic._1.ProcessInstanceState;
import org.ow2.petals.components.flowable.generic._1.Variable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.client.core.support.WebServiceGatewaySupport;
import org.springframework.ws.soap.client.core.SoapActionCallback;

/**
 * 
 * @author vnoel
 *
 */
public class FlowableProcessClient extends WebServiceGatewaySupport {

    private static final ObjectFactory flowableOF = new ObjectFactory();

    @Autowired
    private DatatypeFactory dtf;

    public Iterable<PendingVacationRequest> getMyPendingRequests(final String username) {

        assert username != null;
        
        final GetProcessInstances request = flowableOF.createGetProcessInstances();
        request.setProcessDefinitionIdentifier("vacationRequest");
        request.setState(ProcessInstanceState.ACTIVE);

        final GetProcessInstancesResponse response = (GetProcessInstancesResponse) getWebServiceTemplate()
                .marshalSendAndReceive(request, new SoapActionCallback(
                        "http://petals.ow2.org/components/flowable/generic/1.0/getProcessInstances"));
        
        final List<PendingVacationRequest> res = new LinkedList<>();
        for (final ProcessInstance processInstance : response.getProcessInstances().getProcessInstance()) {
            final PendingVacationRequest pendingRequest = toPendingRequest(processInstance);
            // TODO: The Flowable integration service should filter on the user that starts the process instance
            if (username.equals(pendingRequest.getEnquirer())) {
                res.add(pendingRequest);
            }
        }

        return res;
    }

    public Iterable<RefusedVacationRequest> getMyFinishedRequests(final String username) {

        assert username != null;

        final GetProcessInstances request = flowableOF.createGetProcessInstances();
        request.setProcessDefinitionIdentifier("vacationRequest");
        request.setState(ProcessInstanceState.FINISHED);

        final GetProcessInstancesResponse response = (GetProcessInstancesResponse) getWebServiceTemplate()
                .marshalSendAndReceive(
                        request,
                        new SoapActionCallback(
                                "http://petals.ow2.org/components/flowable/generic/1.0/getProcessInstances"));

        final List<RefusedVacationRequest> res = new LinkedList<>();
        for (final ProcessInstance processInstance : response.getProcessInstances().getProcessInstance()) {
            final RefusedVacationRequest archivedRequest = toFinishedRequest(processInstance);
            // TODO: The Flowable integration service should filter on the user that starts the process instance
            if (username.equals(archivedRequest.getEnquirer())) {
                res.add(archivedRequest);
            }
        }

        return res;
    }

    private PendingVacationRequest toPendingRequest(final ProcessInstance processInstance) {

        final Map<String, String> vars = extractVariables(processInstance);

        final PendingVacationRequest request = new PendingVacationRequest();
        populate(request, processInstance, vars);

        return request;
    }

    private RefusedVacationRequest toRefusedRequest(final ProcessInstance processInstance) {

        final Map<String, String> vars = extractVariables(processInstance);

        final RefusedVacationRequest request = new RefusedVacationRequest();
        populate(request, processInstance, vars);
        // TODO add manager name? not available apparently...
        request.setRejectionReason(vars.get("managerMotivation"));

        return request;
    }

    private RefusedVacationRequest toFinishedRequest(final ProcessInstance processInstance) {

        final Map<String, String> vars = extractVariables(processInstance);

        final ArchivedVacationRequest request = new ArchivedVacationRequest();
        populate(request, processInstance, vars);
        // TODO add manager name? not available apparently...
        request.setRejectionReason(vars.get("managerMotivation"));
        request.setAccepted(vars.get("resendRequest") == null || Boolean.parseBoolean(vars.get("resendRequest")));

        return request;
    }

    private Map<String, String> extractVariables(final ProcessInstance processInstance) {
        final Map<String, String> vars = new HashMap<>();

        for (final Variable variable : processInstance.getVariables().getVariable()) {
            vars.put(variable.getName(), variable.getValue());
        }
        return vars;
    }

    private void populate(final VacationRequest request, final ProcessInstance processInstance,
            Map<String, String> vars) {
        request.setDayNumber(Long.parseLong(vars.get("numberOfDays")));
        request.setEnquirer(vars.get("employeeName"));
        request.setReason(vars.get("vacationMotivation"));
        request.setStartDate(dtf.newXMLGregorianCalendar(vars.get("startDate")).toGregorianCalendar().getTime());
        request.setId(processInstance.getProcessInstanceIdentifier());
        request.setReason(vars.get("vacationMotivation"));
    }

    private ProcessInstance getRequest(final String id) {

        final GetProcessInstances request = flowableOF.createGetProcessInstances();
        request.setProcessDefinitionIdentifier("vacationRequest");
        request.setProcessInstanceIdentifier(id);
        request.setState(ProcessInstanceState.ACTIVE);

        final GetProcessInstancesResponse response = (GetProcessInstancesResponse) getWebServiceTemplate()
                .marshalSendAndReceive(request, new SoapActionCallback(
                        "http://petals.ow2.org/components/flowable/generic/1.0/getProcessInstances"));

        final List<ProcessInstance> processInstance = response.getProcessInstances().getProcessInstance();
        assert processInstance.size() == 1;

        return processInstance.iterator().next();
    }

    public PendingVacationRequest getPendingRequest(final String id) {
        return toPendingRequest(getRequest(id));
    }

    public RefusedVacationRequest getRefusedRequest(final String id) {
        return toRefusedRequest(getRequest(id));
    }

}
