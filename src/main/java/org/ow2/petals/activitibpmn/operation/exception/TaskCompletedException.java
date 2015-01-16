/**
 * Copyright (c) 2014-2015 Linagora
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
package org.ow2.petals.activitibpmn.operation.exception;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.ow2.petals.activitibpmn.operation.ActivitiOperation;

/**
 * The task to complete is alreay completed in the BPMN engine
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class TaskCompletedException extends OperationProcessingFault {

    private static final long serialVersionUID = -5755892148980017293L;

    private static final String MESSAGE_PATTERN = "The task '%s' to complete for the process instance '%s' is already completed in the BPMN engine";

    /**
     * The identifier of the completed task
     */
    private final String taskId;

    /**
     * The identifier of the process instance for which the task is completed
     */
    private final String processInstanceId;

    /**
     * @param processInstanceId
     *            The identifier of the process instance not found
     * @param taskId
     *            The identifier of the process instance not found
     */
    public TaskCompletedException(final String wsdlOperationName, final String processInstanceId, final String taskId) {
        super(wsdlOperationName, String.format(MESSAGE_PATTERN, taskId, processInstanceId));
        this.processInstanceId = processInstanceId;
        this.taskId = taskId;
    }

    /**
     * @return The identifier of the completed task
     */
    public String getTaskId() {
        return this.taskId;
    }

    /**
     * @return The identifier of the process instance not found
     */
    public String getProcessInstanceId() {
        return this.processInstanceId;
    }

    @Override
    public Map<QName, String> getXslParameters() {
        final Map<QName, String> xslParameters = new HashMap<QName, String>();
        xslParameters.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS, "processInstanceId"),
                this.processInstanceId);
        xslParameters.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS, "taskId"), this.taskId);
        return xslParameters;
    }
}
