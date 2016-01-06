/**
 * Copyright (c) 2015-2016 Linagora
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

import org.ow2.petals.component.framework.logger.ConsumeExtFlowStepFailureLogData;

/**
 * @author Christophe DENEUX - Linagora
 */
public final class ProcessInstanceFlowStepFailureLogData extends ConsumeExtFlowStepFailureLogData {

    private static final long serialVersionUID = 8599049420875173677L;

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier of the task completing the user task
     * @param failureMessage
     *            Cancellation message of the process instance
     */
    public ProcessInstanceFlowStepFailureLogData(final String flowInstanceId, final String flowStepId,
            final String failureMessage) {
        super(flowInstanceId, flowStepId, failureMessage);
    }
}
