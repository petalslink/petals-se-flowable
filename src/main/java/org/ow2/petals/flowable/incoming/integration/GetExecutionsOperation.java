/**
 * Copyright (c) 2019-2020 Linagora
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
package org.ow2.petals.flowable.incoming.integration;

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETEXECUTIONS;

import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import org.flowable.engine.RuntimeService;
import org.flowable.engine.runtime.ExecutionQuery;
import org.ow2.petals.components.flowable.generic._1.Execution;
import org.ow2.petals.components.flowable.generic._1.Executions;
import org.ow2.petals.components.flowable.generic._1.GetExecutions;
import org.ow2.petals.components.flowable.generic._1.GetExecutionsResponse;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;

/**
 * The integration operation to search executions
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetExecutionsOperation extends AbstractOperation<GetExecutions, GetExecutionsResponse> {

    public static final URI FAULT_ACTOR = URI
            .create("http://petals.ow2.org/components/flowable/generic/1.0/GetExecutions");

    /**
     * The Flowable task service
     */
    private final RuntimeService runtimeService;

    public GetExecutionsOperation(final RuntimeService runtimeService, final Logger log)
            throws OperationInitializationException {
        super(ITG_OP_GETEXECUTIONS, FAULT_ACTOR,
                new Class[] { GetExecutions.class, GetExecutionsResponse.class, InvalidRequest.class }, log);
        this.runtimeService = runtimeService;
    }

    @Override
    public GetExecutionsResponse doExecute(final GetExecutions incomingObject) throws Exception {

        final ExecutionQuery executionQuery = this.runtimeService.createExecutionQuery();

        final String processDefinitionId = incomingObject.getProcessDefinitionIdentifier();
        if (processDefinitionId != null && !processDefinitionId.isEmpty()) {
            executionQuery.processDefinitionKey(processDefinitionId);
        }

        final String processInstanceId = incomingObject.getProcessInstanceIdentifier();
        if (processInstanceId != null && !processInstanceId.isEmpty()) {
            executionQuery.processInstanceId(processInstanceId);
        }

        final String eventName = incomingObject.getEventName();
        if (eventName != null && !eventName.isEmpty()) {
            executionQuery.messageEventSubscriptionName(eventName);
        }

        final GetExecutionsResponse response = new GetExecutionsResponse();
        final Executions responseExecutions = new Executions();
        response.setExecutions(responseExecutions);
        final List<org.flowable.engine.runtime.Execution> executions = executionQuery.list();
        for (final org.flowable.engine.runtime.Execution execution : executions) {
            final Execution responseExecution = new Execution();
            responseExecutions.getExecution().add(responseExecution);

            responseExecution.setProcessInstanceIdentifier(execution.getProcessInstanceId());
        }

        return response;
    }
}
