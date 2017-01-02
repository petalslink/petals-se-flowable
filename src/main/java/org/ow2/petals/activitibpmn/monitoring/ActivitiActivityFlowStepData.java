/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.activitibpmn.monitoring;

import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * @author Christophe DENEUX - Linagora
 */
public class ActivitiActivityFlowStepData {

    /**
     * Name of the MONIT trace attribute containing the flow instance identifier of the interaction request associated
     * to the activity.
     */
    public static final String CORRELATED_FLOW_INSTANCE_ID_KEY = "correlatedFlowInstanceId";

    /**
     * Name of the MONIT trace attribute containing the flow step identifier of the interaction request associated to
     * the activity.
     */
    public static final String CORRELATED_FLOW_STEP_ID_KEY = "correlatedFlowStepId";

    private ActivitiActivityFlowStepData() {
        // No constructor because it is an utility class
    }

    public static void addActivitiActivityFlowStepData(final AbstractFlowLogData logData,
            final String correlatedFlowInstanceId,
            final String correlatedFlowStepId) {
        logData.putData(CORRELATED_FLOW_INSTANCE_ID_KEY, correlatedFlowInstanceId);
        logData.putData(CORRELATED_FLOW_STEP_ID_KEY, correlatedFlowStepId);
    }
}
