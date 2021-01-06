/**
 * Copyright (c) 2015-2021 Linagora
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

import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * @author Christophe DENEUX - Linagora
 */
public class FlowableActivityFlowStepData {

    /**
     * Name of the MONIT trace attribute containing the process definition of the process instance created.
     */
    public static final String PROCESS_DEFINITION_KEY = "processDefinition";

    /**
     * Name of the MONIT trace attribute containing the identifier of process instance created.
     */
    public static final String PROCESS_INSTANCE_ID_KEY = "processInstanceId";

    /**
     * Name of the MONIT trace attribute containing the call activity definition of the call activity instance created.
     */
    public static final String CALL_ACTIVITY_DEFINITION_KEY = "callActivityDefinition";

    /**
     * Name of the MONIT trace attribute containing the identifier of call activity instance created.
     */
    public static final String CALL_ACTIVITY_INSTANCE_ID_KEY = "callActivityInstanceId";

    private FlowableActivityFlowStepData() {
        // No constructor because it is an utility class
    }

    public static void addCorrelatedFlowStepData(final AbstractFlowLogData logData,
            final String correlatedFlowInstanceId, final String correlatedFlowStepId) {
        logData.putData(FlowLogData.CORRELATED_FLOW_INSTANCE_ID_PROPERTY_NAME, correlatedFlowInstanceId);
        logData.putData(FlowLogData.CORRELATED_FLOW_STEP_ID_PROPERTY_NAME, correlatedFlowStepId);
    }

    public static void addProcessInstanceFlowStepData(final AbstractFlowLogData logData, final String processDefinition,
            final String processInstanceId) {
        logData.putData(PROCESS_DEFINITION_KEY, processDefinition);
        logData.putData(PROCESS_INSTANCE_ID_KEY, processInstanceId);
    }

    public static void addCallActivityFlowStepData(final AbstractFlowLogData logData, final String callActivityDefinition,
            final String callActivityInstanceId) {
        logData.putData(CALL_ACTIVITY_DEFINITION_KEY, callActivityDefinition);
        logData.putData(CALL_ACTIVITY_INSTANCE_ID_KEY, callActivityInstanceId);
    }
}
