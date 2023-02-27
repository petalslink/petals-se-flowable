/**
 * Copyright (c) 2017-2023 Linagora
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
package org.ow2.petals.flowable.monitoring;

import org.ow2.petals.component.framework.logger.ProvideFlowStepEndLogData;

/**
 * @author Christophe DENEUX - Linagora
 */
public final class IntermediateCatchMessageEventFlowStepEndLogData extends ProvideFlowStepEndLogData {

    private static final long serialVersionUID = -8452849644907567269L;

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier of the intermediate catch message event
     * @param correlatedFlowInstanceId
     *            The flow instance identifier of the correlated process completing the intermediate catch message
     *            event, ie. the flow instance identifier coming from the request completing the intermediate catch
     *            message event
     * @param correlatedFlowStepId
     *            The flow step identifier of the correlated process completing the intermediate catch message event,
     *            ie. the flow step identifier coming from the request completing the intermediate catch message event
     */
    public IntermediateCatchMessageEventFlowStepEndLogData(final String flowInstanceId, final String flowStepId,
            final String correlatedFlowInstanceId, final String correlatedFlowStepId) {

        super(flowInstanceId, flowStepId, null);
        FlowableActivityFlowStepData.addCorrelatedFlowStepData(this, correlatedFlowInstanceId, correlatedFlowStepId);

    }
}
