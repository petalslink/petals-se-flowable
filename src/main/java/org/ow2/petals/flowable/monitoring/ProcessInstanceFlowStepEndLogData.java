/**
 * Copyright (c) 2015-2025 Linagora
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

import org.ow2.petals.component.framework.logger.ConsumeExtFlowStepEndLogData;


/**
 * @author Christophe DENEUX - Linagora
 */
public final class ProcessInstanceFlowStepEndLogData extends ConsumeExtFlowStepEndLogData {

    private static final long serialVersionUID = -4045452523559316672L;

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier of the task completing the user task
     */
    public ProcessInstanceFlowStepEndLogData(final String flowInstanceId, final String flowStepId) {
        super(flowInstanceId, flowStepId);
    }
}
