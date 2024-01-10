/**
 * Copyright (c) 2015-2024 Linagora
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

import java.util.LinkedList;
import java.util.List;

import org.ow2.petals.cloud.vacation.web.VacationRequest.PendingVacationRequest;
import org.ow2.petals.cloud.vacation.web.VacationRequest.RefusedVacationRequest;
import org.ow2.petals.components.flowable.generic._1.GetTasks;
import org.ow2.petals.components.flowable.generic._1.GetTasksResponse;
import org.ow2.petals.components.flowable.generic._1.ObjectFactory;
import org.ow2.petals.components.flowable.generic._1.Task;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.client.core.support.WebServiceGatewaySupport;
import org.springframework.ws.soap.client.core.SoapActionCallback;

/**
 * 
 * @author vnoel
 *
 */
public class FlowableTaskClient extends WebServiceGatewaySupport {

    private static final ObjectFactory flowableOF = new ObjectFactory();

    @Autowired
    private FlowableProcessClient processClient;

    public Iterable<RefusedVacationRequest> getRefusedRequests(final String username) {
        final List<RefusedVacationRequest> res = new LinkedList<>();
        for (final Task task : getTasks(username, "adjustVacationRequest")) {
            res.add(processClient.getRefusedRequest(task.getProcessInstanceIdentifier()));
        }

        return res;
    }

    public Iterable<PendingVacationRequest> getNewRequests(final String username) {
        
        final List<PendingVacationRequest> res = new LinkedList<>();
        for (final Task task : getTasks(username, "handleVacationRequest")) {
            res.add(processClient.getPendingRequest(task.getProcessInstanceIdentifier()));
        }
        
        return res;
    }

    private Iterable<Task> getTasks(final String username, final String taskDef) {
        assert username != null;

        final GetTasks request = flowableOF.createGetTasks();
        request.setActive(true);
        request.setAssignee(username);
        request.setProcessDefinitionIdentifier("vacationRequest");
        request.setTaskDefinitionIdentifier(taskDef);

        final GetTasksResponse response = (GetTasksResponse) getWebServiceTemplate().marshalSendAndReceive(request,
                new SoapActionCallback("http://petals.ow2.org/components/flowable/generic/1.0/getTasks"));
        
        return response.getTasks().getTask();
    }
}
