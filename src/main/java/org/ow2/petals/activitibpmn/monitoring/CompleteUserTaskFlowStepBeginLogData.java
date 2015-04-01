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
package org.ow2.petals.activitibpmn.monitoring;

import javax.jbi.messaging.MessageExchange;

import org.ow2.petals.component.framework.logger.ProvideFlowStepBeginLogData;

/**
 * @author Christophe DENEUX - Linagora
 */
public final class CompleteUserTaskFlowStepBeginLogData extends ProvideFlowStepBeginLogData {

    private static final long serialVersionUID = 1179141207903698624L;

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier of the task completing the user task
     * @param flowPreviousStepId
     *            The flow step identifier used to create the process instance
     * @param exchange
     *            The exchange associated to the request completing the user task, and containing: the interface name,
     *            the service name, the operation name and the endpoint name.
     * @param correlatedFlowInstanceId
     *            The flow instance identifier of the correlated process completing the user task, ie. the flow instance
     *            identifier coming from the request completing the user task
     * @param correlatedFlowStepId
     *            The flow step identifier of the correlated process completing the user task, ie. the flow step
     *            identifier coming from the request completing the user task
     */
    public CompleteUserTaskFlowStepBeginLogData(final String flowInstanceId, final String flowStepId,
            final String flowPreviousStepId, final MessageExchange exchange, final String correlatedFlowInstanceId,
            final String correlatedFlowStepId) {

        super(flowInstanceId, flowStepId, exchange.getInterfaceName().toString(), exchange.getService().toString(),
                exchange.getOperation().toString(), exchange.getEndpoint().getEndpointName(), flowPreviousStepId,
                exchange);
        ActivitiActivityFlowStepData.addActivitiActivityFlowStepData(this, correlatedFlowInstanceId,
                correlatedFlowStepId);

    }
}
