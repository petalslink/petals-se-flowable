/**
 * Copyright (c) 2017-2024 Linagora
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

import org.ow2.petals.commons.log.TraceCode;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * @author Christophe DENEUX - Linagora
 */
public final class CallActivityFlowStepBeginLogData extends AbstractFlowLogData {

    private static final long serialVersionUID = -7785621518708139882L;

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier associated to the call activity
     * @param flowPreviousStepId
     *            The previous flow step identifier
     * @param processDefinition
     *            The process definition identifier of the process instance created
     * @param processInstanceId
     *            The identifier of the process instance created
     * @param callActivityDefinition
     *            The call activity definition identifier of the call activity instance created
     * @param callActivityInstanceId
     *            The identifier of the call activity instance created
     */
    public CallActivityFlowStepBeginLogData(final String flowInstanceId, final String flowStepId,
            final String flowPreviousStepId, final String processDefinition, final String processInstanceId,
            final String callActivityDefinition, final String callActivityInstanceId) {

        super(TraceCode.PROVIDE_FLOW_STEP_BEGIN, flowInstanceId, flowStepId);
        putData(FLOW_PREVIOUS_STEP_ID_PROPERTY_NAME, flowPreviousStepId);

        FlowableActivityFlowStepData.addProcessInstanceFlowStepData(this, processDefinition, processInstanceId);
        FlowableActivityFlowStepData.addCallActivityFlowStepData(this, callActivityDefinition, callActivityInstanceId);
    }
}
